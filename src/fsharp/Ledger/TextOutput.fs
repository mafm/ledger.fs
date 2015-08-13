module TextOutput

/// Format our types for the user.

/// For now the only type of formatting we do is plain text.
/// At least this gives a place to put the text-formatting code.

open InputTypes
open InternalTypes

type Text =
    static member fmt (x:AccountNameComponents) =
        match x with
        | last::[] ->
            (sprintf "%s" last.input)
        | first::rest ->
            (sprintf "%s:%s" first.input (Text.fmt rest))
        | [] -> raise EmptyAccountNameComponents
    static member fmt (x: Amount) =
        match x with
        | AUD 0 -> "-"
        | AUD x -> System.String.Format("${0:n}",((float x) * 0.01))
    // XXX/TODO: The fmtXyz methods have unique names, because the
    // argument types to each are identical as far as the compiler
    // is concerned, so we use method name to disambiguate.
    static member fmtDate (x: Date) = (sprintf "%s" x)
    static member fmtAccountName (x: AccountName) = (sprintf "%s" x)
    static member fmtTxnId (i: int) = (sprintf "txn:%d" i)
