class Foo
{
    void Bar() {
        Func<HttpRequestMessage> request = () => new HttpRequestMessage
        {
            RequestUri = GetObjectUri(key),
            Method = HttpMethod.Get,
            Headers = { { "Date", DateTimeOffset.UtcNow.ToString("r") } }
        };
    }
}
