module PrettySharp.Core

/// The logical input type to the pretty-printer. Don't construct this directly:
/// instead, use the various constructor functions below instead. They're safer.
///
/// (In particular, stay away from `UNION` unless you know what you're doing:
/// `UNION`s must obey the invariant that the first line on the left is longer
/// than the first line on the right.)
type DOC =
    | NIL
    | CONCAT of DOC list
    | NEST of int*DOC
    | TEXT of string
    | LINE of string
    | UNION of DOC*DOC
    | BREAKPARENT


// (See https://github.com/prettier/prettier/blob/master/commands.md for the
// set of these that the 'prettier' formatter supports. We should support
// similar, as we have similar needs.)

/// A document with no contents.
let nil = NIL


/// Concatenate two documents.
///
/// (Wadler uses <> but F# doesn't want me to override that one, probably for
/// good reason.)
let ( <+> ) x y  =
    let left =
        match x with
        | CONCAT(xi) -> xi
        | NIL -> []
        | _ -> [x]

    let right =
        match y with
        | CONCAT(yi) -> yi
        | NIL -> []
        | _ -> [y]

    CONCAT (left @ right)


/// Nest the given document `x` by `i` spaces.
let nest i x = NEST (i,x)


/// A text document with content `str`.
let text str = TEXT str


/// A newline.
///
/// This is a soft newline; it will be replaced by a ' ' if we're flattening.
let line = LINE " "


/// A newline.
///
/// This is a soft newline; it will be replaced by a '' if we're flattening.
let softline = LINE ""


/// Include this anywhere to force all parent groups to break.
let breakParent = BREAKPARENT

/// A group of documents: either on one line or on multiple lines.
///
/// This is the core line-breaking thing; if you have something that might be
/// together on one line then use `group`. The printer will try to fit
/// everything on one line, and if that doesn't work it will break the outer
/// group and try again.
let group x =
    let rec hasForce x =
        match x with
        | NIL -> false
        | CONCAT (xi) -> List.exists (hasForce) xi
        | NEST (_, xi) -> hasForce xi
        | TEXT _ -> false
        | LINE _ -> false
        | UNION _ -> false
        | BREAKPARENT -> true

    // Replace the line breaks in a document with spaces. (Note the neat trick
    // of UNION here: we only need to keep the left side, since they should both
    // flatten to the same value.)
    let rec flatten x =
        match x with
        | NIL -> NIL
        | CONCAT (xi) -> List.map (flatten) xi |> List.reduce (<+>)
        | NEST (i, xi) -> NEST (i, flatten xi)
        | TEXT str -> TEXT str
        | LINE str -> TEXT str
        | UNION (xi, _) -> xi
        | BREAKPARENT -> failwith "Should not try to flatten with a break!"

    if hasForce x then x else UNION(flatten x, x)

/// A physical rendering of a document: just line breaks (with indentation) and
/// strings, nothing fancy like unions or nests or anything like that. This form
/// is simple to transform into a string, and is not optimized for further
/// analysis. (That's what `DOC` is for.)
///
/// N.B.: Wadler's paper defines these as nested recursive types; with an
/// explicit `Nil` terminator. Once we've done the linearization work in `best`,
/// though, that's really kinda silly. Also, Wadler's paper relies on Haskell's
/// laziness in order to achieve acceptable performance. In order to get similar
/// performance here in F#, we need to be lazy, and working with Seq<Doc> is the
/// best way to achieve laziness. Therefore, we get sequencing by putting these
/// in a Seq<>, and no longer need `Nil` nor the contents.
type Doc =
    | Text of string
    | Line of int


/// Interpret the physical document `x` as a string.
let layout x =
    let rec layoutRec result x =
        match x with
        | [] -> result
        | Text (s)::z -> layoutRec (result + s) z
        | Line (i)::z ->
            // Take this opportunity to strip trailing spaces.
            layoutRec (result.TrimEnd(' ') + "\n" + (new string(' ', i))) z

    layoutRec "" x

/// `true` if the first line of physical document `x` fits in width `w`.
let rec fits w x =
    if w < 0 then false
    else match Seq.tryHead x with
         | None -> true
         | Some (Text (s)) -> fits (w - s.Length) (Seq.tail x)
         | Some (Line (_)) -> true

/// Returns the better of `x` or `y`, depending on whether `x` fits or not.
let better w k x y = if fits (w-k) x then x else y

/// Given a width `w` and an amount of space consumed `k`, produce the best
/// physical representation of the document `x`.
let best w k x =
    // This is the heart: `x` is a list of int*DOC, where the int is the current
    // indentation level. (That is, if we were to break a line now how many
    // spaces would go before the first character?) We simultaneously convert
    // from the logical DOC to the physical Doc, and use Doc to answer questions
    // about whether things fit on the current line, always returning the best
    // answer for Doc.
    let rec be w k x =
        match x with
        | [] -> Seq.empty
        | (_, NIL)::z | (_, BREAKPARENT)::z -> be w k z
        | (i, CONCAT(xi))::z -> be w k ([for xii in xi -> (i,xii)] @ z)
        | (i, NEST (j, xi))::z -> be w k ((i + j, xi)::z)
        | (_, TEXT s)::z -> seq {yield Text s; yield! be w (k+s.Length) z}
        | (i, LINE _)::z -> seq {yield Line i; yield! be w i z}
        | (i, UNION(xi, yi))::z ->
            better w k (be w k ((i,xi)::z)) (be w k ((i,yi)::z))

    be w k [(0,x)] |> Seq.toList


/// Pretty-print a document 'x' while fitting in 'w' characters, as best you can.
let pretty w x =  best w 0 x |> layout

// Helpers

let ifNotNil x y xy =
    match x,y with
    | x, NIL -> x
    | NIL, y -> y
    | x, y -> xy

/// Concatenate two documents with a space in between them.
let ( <++> ) x y = ifNotNil x y (x <+> text " " <+> y)

let bracket i l r x =
    group (
        text l <+>
        nest i (softline <+> x) <+>
        softline <+>
        text r
    )
