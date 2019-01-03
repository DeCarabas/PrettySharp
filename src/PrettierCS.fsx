#r "netstandard"
#r "System.Text.Encoding"
#r "../packages/Microsoft.CodeAnalysis.Common/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../packages/Microsoft.CodeAnalysis.CSharp/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

// TEST SETUP

let path = @"./tests/source/FeedParser.cs.ignore"
let parseOpts = CSharpParseOptions.Default.WithKind(SourceCodeKind.Script)
//let tree = CSharpSyntaxTree.ParseText (System.IO.File.ReadAllText path)
let tree = CSharpSyntaxTree.ParseText ("
    namespace OnceAndFuture.Syndication
    {
        public class FeedParser
        {
            static void Foo()
            {
               return
                   item.With(
                       enclosures:
                           item.Enclosures.Add(
                               new
                               Enclosure
                               (
                                   length:
                                       element.Attribute(XNames.RSS.Length)?.Value,
                                   type: element.Attribute(XNames.RSS.Type)?.Value,
                                   url:
                                       SyndicationUtil.ParseLink(
                                           element.Attribute(XNames.RSS.Url)
                                           ?.Value,
                                           element))));
            }
        }
    }
", parseOpts)
let root = tree.GetRoot()


#load "../src/Core.fs"

#load "../src/PrettierCS.fs"

open PrettySharp.Core
open PrettySharp.CS

let doc = visit root

printfn "%s" (pretty 80 doc)

printfn "%s" (pretty 20 doc)