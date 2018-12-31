public class FeedParser
{
    static readonly Dictionary<XName, Func<FeedSegment, XElement, FeedSegment>> FeedElements =
        new Dictionary<XName, Func<FeedSegment, XElement, FeedSegment>>
        {
            { XNames.RSS.Title,       (rf, xe) => rf.With(feedTitle: SyndicationUtil.ParseBody(xe)) },
            { XNames.RSS10.Title,     (rf, xe) => rf.With(feedTitle: SyndicationUtil.ParseBody(xe)) },
            { XNames.Atom.Title,      (rf, xe) => rf.With(feedTitle: SyndicationUtil.ParseBody(xe)) },

            { XNames.RSS.Link,        (rf, xe) => rf.With(websiteUrl: xe.Value) },
            { XNames.RSS10.Link,      (rf, xe) => rf.With(websiteUrl: xe.Value) },
            { XNames.Atom.Link,       (rf, xe) => HandleAtomLink(rf, xe) },

            { XNames.RSS.Description,   (rf, xe) => rf.With(feedDescription: SyndicationUtil.ParseBody(xe)) },
            { XNames.RSS10.Description, (rf, xe) => rf.With(feedDescription: SyndicationUtil.ParseBody(xe)) },

            { XNames.RSS.Item,        (rf, xe) => rf.With(items: rf.Items.Add(LoadItem(xe))) },
            { XNames.RSS10.Item,      (rf, xe) => rf.With(items: rf.Items.Add(LoadItem(xe))) },
            { XNames.Atom.Entry,      (rf, xe) => rf.With(items: rf.Items.Add(LoadItem(xe))) },
        };
}