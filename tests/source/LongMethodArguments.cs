namespace OnceAndFuture.Syndication
{
    public class FeedParser
    {
        static void Foo()
        {
            string title = null;
            Uri url0 = SyndicationUtil.TryParseUrl(element.Attribute(XNames.Media.Url)?.Value, null);
            Uri url1 = SyndicationUtil.TryParseUrl(element.Attribute(XNames.Media.Url)?.Value, null, element);
            Uri url2 = SyndicationUtil.TryParseUrl(element.Attribute(XNames.Media.Url)?.Value, null, element, element, element);
            return item.With(enclosures: item.Enclosures.Add(new Enclosure(length: element.Attribute(XNames.RSS.Length)?.Value, type: element.Attribute(XNames.RSS.Type)?.Value, url: SyndicationUtil.ParseLink(element.Attribute(XNames.RSS.Url)?.Value, element))));
        }
    }
}
