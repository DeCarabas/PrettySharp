namespace Foo
{
    class Bar
    {
        public static FeedSegment ParseFeed(Uri feedUrl, XElement element, out FeedFormat format) { }
        public static FeedSegment ParseFeed(Uri feedUrl, XElement element, XElement someOtherElement, out FeedFormat format) { }
    }
}