class Foo
{
    void Bar()
    {
        Func<HttpRequestMessage> request =
            () =>
                new HttpRequestMessage
                {Headers = {{"Date", DateTimeOffset.UtcNow.ToString("r")}}};
    }
}
