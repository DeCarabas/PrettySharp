namespace OnceAndFuture
{
    using System.Linq;
    using Newtonsoft.Json;
    using Npgsql;
    using Npgsql.Logging;
    using NpgsqlTypes;
    using Polly;
    using Serilog;
    using Serilog.Events;
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Globalization;
    using System.IO;
    using System.IO.Compression;
    using System.Net;
    using System.Net.Http;
    using System.Net.Http.Headers;
    using System.Security.Cryptography;
    using System.Text;
    using System.Threading.Tasks;
    using System.Xml.Linq;

    static class HeaderExtensions
    {
        public static string Value(this HttpHeaders headers, string header)
        {
            string result = "";
            if (headers.TryGetValues(header, out IEnumerable<string> headerValues))
            {
                result = String.Join(",", headerValues);
            }
            return result;
        }
    }

    public static class AmazonUtils
    {
        static readonly Comparison<KeyValuePair<string, string>> CompareByOrdinalKey = DoCompareByOrdinalKey;

        public static void GetAuthInfo(out string accessKeyId, out string secretAccessKey)
        {
            string keyIdString = Environment.GetEnvironmentVariable("AWS_ACCESS_KEY_ID");
            string accessKeyString = Environment.GetEnvironmentVariable("AWS_SECRET_ACCESS_KEY");

            if (String.IsNullOrWhiteSpace(keyIdString) || String.IsNullOrWhiteSpace(accessKeyString))
            {
                string credPath = Path.Combine(Environment.GetEnvironmentVariable("HOME"), ".aws", "credentials");
                if (File.Exists(credPath))
                {
                    string[] lines = File.ReadAllLines(credPath);
                    for (int i = 0; i < lines.Length; i++)
                    {
                        string line = lines[i];
                        if (line.StartsWith("aws_access_key_id", StringComparison.OrdinalIgnoreCase))
                        {
                            keyIdString = line.Split(new[] { '=' }, 2)[1].Trim();
                        }
                        if (line.StartsWith("aws_secret_access_key", StringComparison.OrdinalIgnoreCase))
                        {
                            accessKeyString = line.Split(new[] { '=' }, 2)[1].Trim();
                        }
                    }
                }
            }

            accessKeyId = keyIdString;
            secretAccessKey = accessKeyString;
        }

        /// <summary>Construct the Authorization header value for the specified request.</summary>
        /// <param name="request"></param>
        /// <returns>The value of the 'Authorization' header for this request.</returns>
        /// <remarks>See http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#ConstructingTheAuthenticationHeader for more.</remarks>
        public static string AuthenticateRequestS3(
            HttpRequestMessage request,
            string accessKeyId,
            byte[] secretAccessKey
        )
        {
            string httpVerb = request.Method.ToString().ToUpperInvariant();
            string contentMD5 = request.Content?.Headers?.Value("Content-MD5") ?? String.Empty;
            string contentType = request.Content?.Headers?.Value("Content-Type") ?? String.Empty;
            string date = request.Headers.Value("Date");

            string canonicalizedAmzHeaders = "";
            foreach (KeyValuePair<string, IEnumerable<string>> headers in request.Headers)
            {
                if (headers.Key.StartsWith("x-amz-", StringComparison.OrdinalIgnoreCase))
                {
                    canonicalizedAmzHeaders +=
                        headers.Key.ToLowerInvariant() + ":" + String.Join(",", headers.Value) + "\n";
                }
            }

            string canonicalizedResource = request.RequestUri.AbsolutePath;

            string stringToSign =
                httpVerb + "\n" +
                contentMD5 + "\n" +
                contentType + "\n" +
                date + "\n" +
                canonicalizedAmzHeaders +
                canonicalizedResource;
            using (var hmac = new HMACSHA1(secretAccessKey))
            {
                string signature = Convert.ToBase64String(hmac.ComputeHash(Encoding.UTF8.GetBytes(stringToSign)));
                return "AWS " + accessKeyId + ":" + signature;
            }
        }

        /// <summary>Construct the Authorization header value for the specified request.</summary>
        /// <param name="request"></param>
        /// <returns>The value of the 'Authorization' header for this request.</returns>
        /// <remarks>See http://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html for more.</remarks>
        public static async Task AuthenticateRequestV4(
            HttpRequestMessage request,
            string region,
            string service,
            string accessKeyId,
            string secretAccessKey
        )
        {
            const string algorithm = "AWS4-HMAC-SHA256";

            if (request.Headers.Contains("x-amz-date")) { throw new InvalidOperationException(); }
            DateTimeOffset date = DateTimeOffset.UtcNow;
            string dateStamp = date.ToString("yyyyMMdd");
            string amzdate = date.ToString("yyyyMMdd'T'HHmmss'Z'");
            request.Headers.Add("x-amz-date", amzdate);
            if (!request.Headers.Contains("host")) { request.Headers.Add("host", request.RequestUri.Host); }

            List<KeyValuePair<string, string>> headers = GetCanonicalHeaders(request);

            // Task 1: Build the 'canonical request'.
            var canonicalRequest = new StringBuilder();
            await BuildCanonicalRequest(request, headers, canonicalRequest);
            byte[] requestBytes = Encoding.UTF8.GetBytes(canonicalRequest.ToString());

            // Task 2: Build the 'string to sign'
            var builder = new StringBuilder();
            builder.Append(algorithm);
            builder.Append('\n');
            builder.Append(amzdate);
            builder.Append('\n');
            BuildCredentialScope(dateStamp, region, service, builder);
            builder.Append('\n');
            BuildSHA256Digest(requestBytes, builder);

            // Task 3: Calculate the signature
            byte[] signingKey = GetSignatureKey(secretAccessKey, dateStamp, region, service);
            byte[] signatureBytes = HmacSHA256(builder.ToString(), signingKey);

            // Task 4: Build the header.
            builder.Clear();
            builder.Append(algorithm);
            builder.Append(" Credential=");
            builder.Append(accessKeyId);
            builder.Append('/');
            BuildCredentialScope(dateStamp, region, service, builder);
            builder.Append(", SignedHeaders=");
            BuildSignedHeaders(headers, builder);
            builder.Append(", Signature=");
            HexEncode(signatureBytes, builder);

            // It's JUST THAT EASY.
            request.Headers.TryAddWithoutValidation("Authorization", builder.ToString());
        }

        static void BuildCredentialScope(string date, string region, string service, StringBuilder builder)
        {
            builder.Append(date);
            builder.Append('/');
            builder.Append(region);
            builder.Append('/');
            builder.Append(service);
            builder.Append("/aws4_request");
        }

        static async Task BuildCanonicalRequest(
            HttpRequestMessage request,
            List<KeyValuePair<string, string>> headers,
            StringBuilder builder
        )
        {
            builder.Append(request.Method.ToString().ToUpperInvariant());
            builder.Append('\n');
            BuildCanonicalResource(request, builder);
            builder.Append('\n');
            BuildCanonicalQueryString(request, builder);
            builder.Append('\n');
            BuildCanonicalHeaders(headers, builder);
            builder.Append('\n');
            BuildSignedHeaders(headers, builder);
            builder.Append('\n');
            await BuildPayloadHash(request, builder);
        }

        static async Task BuildPayloadHash(HttpRequestMessage request, StringBuilder builder)
        {
            byte[] payload = new byte[0];
            if (request.Content != null) { payload = await request.Content.ReadAsByteArrayAsync(); }
            BuildSHA256Digest(payload, builder);
        }

        static void AddCanonicalHeaders(HttpHeaders headers, List<KeyValuePair<string, string>> result)
        {
            foreach (KeyValuePair<string, IEnumerable<string>> header in headers)
            {
                string name = header.Key.ToLowerInvariant().Trim();
                string value = String.Join(",", header.Value.Select(v => v.Replace("  ", " "))).Trim();
                result.Add(new KeyValuePair<string, string>(name, value));
            }
        }

        static List<KeyValuePair<string, string>> GetCanonicalHeaders(HttpRequestMessage request)
        {
            var headers = new List<KeyValuePair<string, string>>();
            AddCanonicalHeaders(request.Headers, headers);
            if (request.Content != null) { AddCanonicalHeaders(request.Content.Headers, headers); }
            headers.Sort(CompareByOrdinalKey);
            return headers;
        }

        static void BuildCanonicalHeaders(List<KeyValuePair<string, string>> headers, StringBuilder builder)
        {
            for (int i = 0; i < headers.Count; i++)
            {
                builder.Append(headers[i].Key);
                builder.Append(':');
                builder.Append(headers[i].Value);
                builder.Append('\n');
            }
        }

        static void BuildSignedHeaders(List<KeyValuePair<string, string>> headers, StringBuilder builder)
        {
            for (int i = 0; i < headers.Count; i++)
            {
                if (i > 0) { builder.Append(';'); }
                builder.Append(headers[i].Key);
            }
        }

        static void BuildCanonicalQueryString(HttpRequestMessage request, StringBuilder builder)
        {
            // A note about escaping: we don't do any. We assume that the parameters in the request are already
            // escaped appropriately. We hope that the sort order is not affected by this.
            string query = request.RequestUri.Query;
            var parameters = new List<KeyValuePair<string, string>>();

            int index = 1;
            while (index < query.Length)
            {
                int keyStart = index;
                while (index < query.Length && query[index] != '&' && query[index] != '=') { index++; }

                // Skip degenerate query parameters, e.g. '...&&&...'
                if (index < query.Length && query[index] == '&') { index += 1; continue; }

                string key = query.Substring(keyStart, index - keyStart);
                string value = String.Empty;
                if (index < query.Length && query[index] == '=')
                {
                    index += 1;
                    int valueStart = index;
                    while (index < query.Length && query[index] != '&') { index += 1; }

                    // Don't bother allocating in the case of '...&foo=&...'
                    if (index - valueStart > 0) { value = query.Substring(valueStart, index - valueStart); }
                }


                parameters.Add(new KeyValuePair<string, string>(key, value));

                // Now index either points off the end or at a '&', either way this is safe.
                index++;
            }

            parameters.Sort(CompareByOrdinalKey);

            for (int i = 0; i < parameters.Count; i++)
            {
                if (i > 0) { builder.Append('&'); }
                builder.Append(parameters[i].Key);
                builder.Append('=');
                if (parameters[i].Value.Length > 0)
                {
                    builder.Append(parameters[i].Value);
                }
            }
        }

        static void BuildCanonicalResource(HttpRequestMessage request, StringBuilder builder)
        {
            builder.Append(request.RequestUri.AbsolutePath);
        }

        static int DoCompareByOrdinalKey(KeyValuePair<string, string> a, KeyValuePair<string, string> b)
            => StringComparer.Ordinal.Compare(a.Key, b.Key);

        static void BuildSHA256Digest(byte[] data, StringBuilder builder)
        {
            using (var hasher = SHA256.Create())
            {
                byte[] hash = hasher.ComputeHash(data);
                HexEncode(hash, builder);
            }
        }

        static void HexEncode(byte[] data, StringBuilder builder)
        {
            for (int i = 0; i < data.Length; i++)
            {
                builder.AppendFormat("{0:x2}", data[i]);
            }
        }

        static byte[] HmacSHA256(string data, byte[] key)
        {
            using (KeyedHashAlgorithm kha = new HMACSHA256(key))
            {
                return kha.ComputeHash(Encoding.UTF8.GetBytes(data));
            }
        }

        static byte[] GetSignatureKey(string key, string dateStamp, string region, string service)
        {
            byte[] kSecret = Encoding.UTF8.GetBytes(("AWS4" + key).ToCharArray());
            byte[] kDate = HmacSHA256(dateStamp, kSecret);
            byte[] kRegion = HmacSHA256(region, kDate);
            byte[] kService = HmacSHA256(service, kRegion);
            byte[] kSigning = HmacSHA256("aws4_request", kService);

            return kSigning;
        }
    }

    public class BlobStore
    {
        static readonly HttpClient Client = CreateHttpClient();
        const string OriginalLengthHeader = "x-amz-meta-original-length";
        static string ValidPathCharacters = DetermineValidPathCharacters();

        static readonly Policy<HttpResponseMessage> HttpPolicy = Policy
            .Handle<HttpRequestException>(Policies.ShouldRetryException)
            .Or<TaskCanceledException>()
            .Or<WebException>(Policies.ShouldRetryException)
            .OrResult<HttpResponseMessage>(ShouldRetryFromResponse)
            .WaitAndRetryAsync(retryCount: 3, sleepDurationProvider: Policies.RetryTime);

        readonly string accessKeyId;
        readonly string bucket;
        readonly ILogger logger;
        readonly byte[] secretAccessKey;
        readonly string subdir;


        public BlobStore(string bucket, string subdir)
        {
            this.logger = Serilog.Log
                .ForContext(HoneycombSink.DatasetPropertyKey, "Storage")
                .ForContext("bucket", bucket)
                .ForContext("subdir", subdir);

            this.bucket = bucket;
            this.subdir = subdir;

            string accessKeyString;
            AmazonUtils.GetAuthInfo(out this.accessKeyId, out accessKeyString);
            this.secretAccessKey = Encoding.UTF8.GetBytes(accessKeyString);
        }

        static HttpClient CreateHttpClient()
        {
            const int TenMegabytes = 10 * 1024 * 1024;
            var handler = new HttpClientHandler
            {
                AllowAutoRedirect = true,
                UseCookies = false,
                UseDefaultCredentials = false,
                MaxConnectionsPerServer = 1000,
            };

            var client = new HttpClient(handler, true);
            client.Timeout = TimeSpan.FromSeconds(15);
            client.MaxResponseContentBufferSize = TenMegabytes;
            client.DefaultRequestHeaders.UserAgent.ParseAdd("TheOnceAndFuture/1.0");
            return client;
        }

        public Uri GetObjectUri(string name)
        {
            return new Uri(String.Concat(
                "https://s3-us-west-2.amazonaws.com/",
                this.bucket,
                "/",
                this.subdir,
                "/",
                Uri.EscapeDataString(name)
            ));
        }

        public Uri GetObjectUri(ObjectKey key)
        {
            string resourcePath = UrlEncode(this.bucket + "/" + key.ToString(), path: true);
            return new Uri("https://s3-us-west-2.amazonaws.com/" + resourcePath);
        }

        public async Task<byte[]> GetObject(string name)
        {
            ObjectKey key = KeyForName(name);
            byte[] result = await GetObject(key);
            if (result == null)
            {
                // In time all data will be moved to the new scheme, but until then...
                result = await GetObject(new ObjectKey(key: name));
            }
            return result;
        }

        public async Task<byte[]> GetObject(ObjectKey key)
        {
            Stopwatch timer = Stopwatch.StartNew();
            Func<HttpRequestMessage> request = () => new HttpRequestMessage
            {
                RequestUri = GetObjectUri(key),
                Method = HttpMethod.Get,
                Headers = { { "Date", DateTimeOffset.UtcNow.ToString("r") } }
            };

            string contentType = null;
            long? contentLength = null;
            long? objectLength = null;
            try
            {
                bool isCompressed;
                MemoryStream responseBuffer;
                using (HttpResponseMessage response = await SendAsync(request))
                {
                    if (!response.IsSuccessStatusCode)
                    {
                        S3Error error = await DecodeError(response);
                        LogOperation("Get", key, null, null, null, timer, OperationStatus.Error, error.ResponseBody);
                        if (error.Code == "NoSuchKey")
                        {
                            return null;
                        }
                        if (error.Code == "AccessDenied")
                        {
                            return null;
                        }
                        throw new S3Exception("Get", key, error);
                    }

                    contentType = response.Content.Headers.ContentType.ToString();
                    contentLength = response.Content.Headers.ContentLength ?? 0;
                    objectLength = contentLength;
                    string ol = response.Headers.Value(OriginalLengthHeader);
                    if (!String.IsNullOrWhiteSpace(ol))
                    {
                        objectLength = long.Parse(ol);
                    }

                    responseBuffer = new MemoryStream();
                    await response.Content.CopyToAsync(responseBuffer);
                    responseBuffer.Position = 0;

                    isCompressed = response.Content.Headers.ContentEncoding.Contains("gzip");
                }

                byte[] data = new byte[objectLength.Value];
                if (isCompressed)
                {
                    var sourceStream = new GZipStream(responseBuffer, CompressionMode.Decompress);
                    var targetStream = new MemoryStream(data);
                    sourceStream.CopyTo(targetStream);
                }
                else
                {
                    responseBuffer.Read(data, 0, data.Length);
                }

                LogOperation(
                    "Get",
                    key,
                    contentType,
                    objectLength,
                    contentLength,
                    timer,
                    OperationStatus.OK,
                    null);
                return data;
            }
            catch (S3Exception) { throw; }
            catch (Exception e)
            {
                LogOperation(
                    "Get",
                    key,
                    contentType,
                    objectLength,
                    contentLength,
                    timer,
                    OperationStatus.Exception,
                    e.ToString());
                throw;
            }
        }


        public async Task PutObject(string name, string type, MemoryStream stream, bool compress = false)
        {
            ObjectKey key = KeyForName(name);
            await PutObject(key, type, stream, compress);
        }

        public async Task PutObject(ObjectKey key, string type, MemoryStream stream, bool compress)
        {
            Stopwatch timer = Stopwatch.StartNew();
            long objectLength = stream.Length;
            long contentLength = objectLength;
            string encoding = null;
            Stream sourceStream = stream;

            try
            {
                if (compress)
                {
                    var tempStream = new MemoryStream();
                    using (var compressStream = new GZipStream(tempStream, CompressionMode.Compress, leaveOpen: true))
                    {
                        stream.CopyTo(compressStream);
                    }
                    tempStream.Position = 0;
                    sourceStream = tempStream;
                    encoding = "gzip";
                }
                contentLength = sourceStream.Length;

                Func<HttpRequestMessage> request = () => new HttpRequestMessage
                {
                    RequestUri = GetObjectUri(key),
                    Method = HttpMethod.Put,
                    Headers =
                    {
                        { "Date", DateTimeOffset.UtcNow.ToString("r") },
                        { OriginalLengthHeader, objectLength.ToString() },
                    },
                    Content = new StreamContent(sourceStream)
                    {
                        Headers =
                        {
                            { "Content-Type", type },
                            { "Content-Encoding", encoding },
                        },
                    },
                };

                using (HttpResponseMessage response = await SendAsync(request))
                {
                    if (!response.IsSuccessStatusCode)
                    {
                        S3Error error = await DecodeError(response);
                        LogOperation(
                            "Put",
                            key,
                            type,
                            objectLength,
                            sourceStream.Length,
                            timer,
                            OperationStatus.Error,
                            error.ResponseBody);
                        throw new S3Exception("Put", key, error);
                    }

                    LogOperation("Put", key, type, objectLength, contentLength, timer, OperationStatus.OK, null);
                }
            }
            catch (S3Exception) { throw; }
            catch (Exception e)
            {
                LogOperation(
                    "Put",
                    key,
                    type,
                    objectLength,
                    contentLength,
                    timer,
                    OperationStatus.Exception,
                    e.ToString());
                throw;
            }
        }

        /// <summary>Construct the Authorization header value for the specified request.</summary>
        /// <param name="request"></param>
        /// <returns></returns>
        /// <remarks>See http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#ConstructingTheAuthenticationHeader for more.</remarks>
        HttpRequestMessage AuthenticateRequest(HttpRequestMessage request)
        {
            request.Headers.Add(
                "Authorization",
                AmazonUtils.AuthenticateRequestS3(request, this.accessKeyId, this.secretAccessKey)
            );
            return request;
        }

        static async Task<S3Error> DecodeError(HttpResponseMessage response)
        {
            string body = await response.Content.ReadAsStringAsync();
            XDocument doc = XDocument.Load(new StringReader(body));
            string code = doc.Root.Element("Code").Value;

            return new S3Error { Code = code, ResponseBody = body };
        }

        static string DetermineValidPathCharacters()
        {
            const string basePathCharacters = "/:'()!*[]$";

            var sb = new StringBuilder();
            foreach (var c in basePathCharacters)
            {
                var escaped = Uri.EscapeUriString(c.ToString());
                if (escaped.Length == 1 && escaped[0] == c)
                    sb.Append(c);
            }
            return sb.ToString();
        }

        ObjectKey KeyForName(string name) => new ObjectKey(this.subdir, name);

        void LogOperation(
            string operation,
            ObjectKey key,
            string contentType,
            long? objectSize,
            long? storageSize,
            Stopwatch timer,
            OperationStatus status,
            string details
        )
        {
            this.logger.Information(
                "{Operation} {Key} ({ContentType} {ObjectBytes}bytes/{StorageBytes}compressed): {ElapsedMs}ms: {Status}: {Details}",
                operation, key.ToString(), contentType, objectSize, storageSize, timer.ElapsedMilliseconds, status.ToString(), details);
        }

        Task<HttpResponseMessage> SendAsync(Func<HttpRequestMessage> request)
        {
            return HttpPolicy.ExecuteAsync(async () =>
            {
                HttpRequestMessage message = AuthenticateRequest(request());
                HttpResponseMessage response = await Client.SendAsync(message);
                if (!response.IsSuccessStatusCode) { await response.Content.LoadIntoBufferAsync(); }
                return response;
            });
        }

        static bool ShouldRetryFromResponse(HttpResponseMessage response)
        {
            if (response.IsSuccessStatusCode) { return false; }
            S3Error error = DecodeError(response).Result;

            if (error.Code == "RequestTimeout") { return true; }

            return false;
        }

        static string UrlEncode(string data, bool path)
        {
            StringBuilder encoded = new StringBuilder(data.Length * 2);
            const string validUrlCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~";

            string unreservedChars = String.Concat(validUrlCharacters, (path ? ValidPathCharacters : ""));

            foreach (char symbol in System.Text.Encoding.UTF8.GetBytes(data))
            {
                if (unreservedChars.IndexOf(symbol) != -1)
                {
                    encoded.Append(symbol);
                }
                else
                {
                    encoded.Append("%").Append(string.Format(CultureInfo.InvariantCulture, "{0:X2}", (int)symbol));
                }
            }

            return encoded.ToString();
        }

        public struct ObjectKey
        {
            public string Key;

            public ObjectKey(string key) { Key = key; }

            public ObjectKey(string subdir, string name) { Key = subdir + "/" + name; }

            public override string ToString() => Key;
        }

        class S3Error
        {
            public string Code { get; set; }
            public string ResponseBody { get; set; }
        }

        class S3Exception : Exception
        {
            public S3Exception(string operation, ObjectKey key, S3Error error)
                : base(String.Format("An error occurred accessing S3 {0} {1}: {2}", operation, key, error.Code))
            {
                Operation = operation;
                Key = key;
                Error = error;
            }

            public string Operation { get; }
            public ObjectKey Key { get; }
            public S3Error Error { get; }
        }

        enum OperationStatus
        {
            OK,
            Error,
            Exception
        }
    }

    public static class DocumentStore
    {
        public static void InitializeLogging()
        {
            NpgsqlLogManager.Provider = new Provider();
        }

        class Provider : INpgsqlLoggingProvider
        {
            public NpgsqlLogger CreateLogger(string name) => new Logger(name);
        }

        class Logger : NpgsqlLogger
        {
            readonly ILogger logger;

            public Logger(string name)
            {
                this.logger = Serilog.Log.ForContext("NPGSQL_SOURCE", name);
            }

            static LogEventLevel ConvertLevel(NpgsqlLogLevel level)
            {
                switch (level)
                {
                    case NpgsqlLogLevel.Trace:
                        return LogEventLevel.Verbose;
                    case NpgsqlLogLevel.Debug:
                        return LogEventLevel.Debug;
                    case NpgsqlLogLevel.Info:
                        return LogEventLevel.Information;
                    case NpgsqlLogLevel.Warn:
                        return LogEventLevel.Warning;
                    case NpgsqlLogLevel.Error:
                        return LogEventLevel.Error;
                    case NpgsqlLogLevel.Fatal:
                        return LogEventLevel.Fatal;
                    default:
                        throw new ArgumentOutOfRangeException("level");
                }
            }

            public override bool IsEnabled(NpgsqlLogLevel level) => this.logger.IsEnabled(ConvertLevel(level));
            public override void Log(NpgsqlLogLevel level, int connectorId, string msg, Exception exception = null)
                => this.logger.Write(ConvertLevel(level), exception, "{ConnectorId}: {Message}", connectorId, msg);
        }
    }

    public abstract class DocumentStore<TDocumentID, TDocument> : DAL.DalBase where TDocument : class
    {
        readonly BlobStore blobStore;
        readonly string table;

        protected DocumentStore(BlobStore blobStore, string table) : base(table)
        {
            this.blobStore = blobStore;
            this.table = table;
        }

        protected abstract string GetObjectID(TDocumentID id);
        protected abstract TDocument GetDefaultValue(TDocumentID id);

        protected async Task<bool> DocumentExists(TDocumentID docid)
        {
            string id = GetObjectID(docid);
            return
                (await DocumentExistsInDatabase(id)) ||
                (await DocumentExistsInBlobStore(id));
        }

        protected async Task<TDocument> GetDocument(TDocumentID docid)
        {
            string id = GetObjectID(docid);

            TDocument document = await GetDocumentFromDatabase(id);
            if (document == null) { document = await GetDocumentFromBlobStore(id); }
            if (document == null) { document = GetDefaultValue(docid); }
            return document;
        }

        private async Task<TDocument> GetDocumentFromBlobStore(string id)
        {
            byte[] blob = await this.blobStore.GetObject(id);
            if (blob == null) { return null; }

            using (var memoryStream = new MemoryStream(blob))
            using (var reader = new StreamReader(memoryStream, Encoding.UTF8))
            {
                string text = reader.ReadToEnd();
                return JsonConvert.DeserializeObject<TDocument>(text, Policies.SerializerSettings);
            }
        }

        async Task<bool> DocumentExistsInBlobStore(string id)
        {
            byte[] blob = await this.blobStore.GetObject(id);
            return blob != null;
        }

        async Task<TDocument> GetDocumentFromDatabase(string id)
        {
            TDocument document = null;
            await DoOperation("read", id, async () =>
            {
                using (var connection = await OpenConnection())
                {
                    using (NpgsqlCommand cmd = connection.CreateCommand())
                    {
                        cmd.CommandText = String.Format("SELECT document FROM {0} WHERE id = @id", this.table);
                        cmd.Parameters.AddWithValue("id", NpgsqlDbType.Varchar, id);
                        cmd.Prepare();
                        using (var reader = await cmd.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                string text = reader.GetString(0);
                                document = JsonConvert.DeserializeObject<TDocument>(text, Policies.SerializerSettings);
                            }
                        }
                    }
                }
                return (document == null) ? "not found" : null;
            });

            return document;
        }

        protected async Task WriteDocument(TDocumentID docid, TDocument document)
        {
            string id = GetObjectID(docid);
            await DoOperation("write", id, async () =>
            {
                string text = JsonConvert.SerializeObject(document, Policies.SerializerSettings);

                using (var connection = await OpenConnection())
                {
                    using (NpgsqlCommand cmd = connection.CreateCommand())
                    {
                        cmd.CommandText = String.Format(
                            @"INSERT INTO {0} (id, document)
                          VALUES (@id, @text)
                          ON CONFLICT (id) DO UPDATE
                            SET document = @text
                        ",
                            this.table
                        );
                        cmd.Parameters.AddWithValue("id", NpgsqlDbType.Varchar, id);
                        cmd.Parameters.AddWithValue("text", NpgsqlDbType.Json, text);
                        cmd.Prepare();
                        await cmd.ExecuteNonQueryAsync();
                    }
                }

                return null;
            });
        }

        async Task<bool> DocumentExistsInDatabase(string id)
        {
            long count = 0;
            await DoOperation("read", id, async () =>
            {
                using (var connection = await OpenConnection())
                {
                    using (NpgsqlCommand cmd = connection.CreateCommand())
                    {
                        cmd.CommandText = String.Format("SELECT COUNT(1) FROM {0} WHERE id = @id", this.table);
                        cmd.Parameters.AddWithValue("id", NpgsqlDbType.Varchar, id);
                        cmd.Prepare();
                        count = (long)(await cmd.ExecuteScalarAsync());
                        return null;
                    }
                }
            });

            return count != 0;
        }
    }
}
