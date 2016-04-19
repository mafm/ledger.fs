module TextOutput

/// Format our types for the user.

/// For now the only type of formatting we do is plain text.
/// At least this gives a place to put the text-formatting code.

open InputTypes
open InternalTypes

type Text =
    static member fmt (x:InternalNameAccount) =
        match x with
        | last::[] ->
            match last.Input with
                (Input name) -> (sprintf "%s" name)
        | first::rest ->
            match first.Input with
                (Input name) -> (sprintf "%s:%s" name (Text.fmt rest))
        | [] -> raise EmptyAccountNameComponentsException
    static member fmt (x: Amount) =
        match x with
        | AUD 0 -> "-"
        | AUD x -> System.String.Format("${0:n}",((float x) * 0.01))
    // XXX/TODO: The fmtXyz methods have unique names, because the
    // argument types to each are identical as far as the compiler
    // is concerned, so we use method name to disambiguate.
    static member fmtDate (x: Date) = (sprintf "%s" x)
    static member fmtAccountName (x: InputNameAccount) = (x.AsString)
    static member fmtTxnId (i: int) = (sprintf "txn:%d" i)
