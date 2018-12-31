module PrettySharp.Core

/// The logical input type to the pretty-printer. Don't construct this directly:
/// instead, use the various constructor functions below instead. They're safer.
///
/// (In particular, stay away from `UNION` unless you know what you're doing:
/// `UNION`s must obey the invariant that the first line on the left is longer
/// than the first line on the right.)
type DOC =
    | NIL
    | CONCAT of DOC*DOC
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
let ( <+> ) x y  = CONCAT (x,y)


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


/// Uh.
let breakParent = BREAKPARENT

/// A group of documents: either on one line or on multiple lines.
///
/// This is the core line-breaking thing; if you have something that might be
/// together on one line then use `group`.
let rec group x =
    let rec hasForce x =
        match x with
        | NIL -> false
        | CONCAT (xi, yi) -> hasForce xi || hasForce yi
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
        | CONCAT (xi, yi) -> CONCAT (flatten xi, flatten yi)
        | NEST (i, xi) -> flatten xi
        | TEXT str -> TEXT str
        | LINE str -> TEXT str
        | UNION (xi, _) -> xi
        | BREAKPARENT -> failwith "Should not try to flatten with a break!"

    if hasForce x then x else UNION(flatten x, x)

/// A physical rendering of a document: just line breaks (with indentation) and
/// strings, nothing fancy like unions or nests or anything like that. This form
/// is simple to transform into a string, and is not optimized for further
/// analysis. (That's what `DOC` is for.)
type Doc =
    | Nil
    | Text of string*Doc
    | Line of int*Doc


/// `true` if the first line of physical document `x` fits in width `w`.
let rec fits w x =
    if w < 0 then false
    else match x with
         | Nil -> true
         | Text (s, xi) -> fits (w - s.Length) xi
         | Line (_) -> true


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
        | [] -> Nil
        | (_, NIL)::z -> be w k z
        | (i, CONCAT(xi, yi))::z -> be w k ((i,xi)::(i,yi)::z)
        | (i, NEST (j, xi))::z -> be w k ((i + j, xi)::z)
        | (_, TEXT s)::z -> Text (s, (be w (k+s.Length) z))
        | (i, LINE _)::z -> Line (i, (be w i z))
        | (_, BREAKPARENT)::z -> be w k z
        | (i, UNION(xi, yi))::z ->
            // N.B.: In Wadler's paper he has a function called `better` which
            // abstracts this computation, but his code is in Haskell and so he
            // gets the extra efficiency of lazy calculation. We need to do this
            // by hand in F# since F# is eager. Note that we know that `xi` is
            // flattened, and longer than the first line of `yi`, by
            // construction.
            let bestXi = (be w k ((i, xi)::z)) in
            if fits (w - k) bestXi then bestXi else (be w k ((i, yi)::z))

    be w k [(0,x)]


/// Interpret the physical document `x` as a string.
let rec layout x =
    match x with
    | Text (s,xi) -> s + layout xi
    | Line (i,xi) -> "\n" + (new string(' ', i)) + layout xi
    | Nil -> ""


/// Pretty-print a document 'x' while fitting in 'w' characters, as best you can.
let pretty w x =  layout (best w 0 x)

// Helpers

/// Concatenate two documents with a space in between them.
let ( <++> ) x y = x <+> text " " <+> y

let bracket i l r x =
    group (
        text l <+>
        nest i (softline <+> x) <+>
        softline <+>
        text r
    )
