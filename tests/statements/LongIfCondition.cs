namespace OnceAndFuture.Syndication
{
    public class FeedParser
    {
        static Item HandleThumbnail(Item item, XElement element)
        {
            if (element.Name == XNames.Media.Content && element.Attribute(XNames.Media.Medium)?.Value == "image")
            {
            }
        }
    }
}
