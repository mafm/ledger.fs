module Parse

open FParsec
open Types
open Misc

type ConfigParse =
    | ParseError of string
    | ParseSuccess of TransactionFile

let nonEolWhiteSpace = " \t"
// At least one space
let pMandatorySpace =
    (skipMany1 (anyOf nonEolWhiteSpace)) <?> "space"
// Optional space
let pOptionalSpace =
    (skipMany (anyOf nonEolWhiteSpace)) <?> "space"

let pAccount =
       let isAccountFirstChar c = isLetter c
       let isAccountChar c = isLetter c || isDigit c || c = '-' || c = '_' || c = '/' || c = ':' || c = '.'
       many1Satisfy2L isAccountFirstChar isAccountChar "account"

let pAudAmount =
    let isAudAmountFirstChar c = isDigit c || c = '-' || c = '$' || c = '+'
    let isAudAmountChar c = isDigit c || c = '-' || c = '$' || c = '.' || c = ','
    (many1Satisfy2L isAudAmountFirstChar isAudAmountChar "amount")  .>> (opt ((pOptionalSpace) .>> (pstring "AUD")))
    |>> fun s ->
            let s = (stripChars "$," s) in
            AUD (int (round (100.0 * (float s))))

let pAmount =
    pAudAmount

let pYear =
    (pipe4 digit digit digit digit (fun a b c d -> System.String.Concat(Array.ofList([a;b;c;d])))) <?> "year"
let pMonth =
    (pipe2 digit digit (fun a b -> System.String.Concat(Array.ofList([a;b])))) <?> "month"
let pDay =
    (pipe2 digit digit (fun a b -> System.String.Concat(Array.ofList([a;b])))) <?> "day"
let pDate =
    (pipe3 (pYear .>> (pchar '-'))
          (pMonth .>> (pchar '-'))
          (pDay)
          (fun y m d -> (sprintf "%s-%s-%s") y m d)) <?> "date"

let pBlankLine =
    newline
    |>> fun _ -> BlankLine

let pCommentLine =
    (pchar '#') >>. (restOfLine true)
    |>> fun c -> Comment c

let pVerifyBalance =
    pipe3 ((pstring "VERIFY-BALANCE") >>. pMandatorySpace >>. pDate)
          (pMandatorySpace >>. pAccount)
          (pMandatorySpace >>. pAmount .>> pOptionalSpace .>> newline)
        (fun date account amount -> BalanceVerfication { BalanceVerfication.date = date
                                                         BalanceVerfication.account = account
                                                         BalanceVerfication.amount = amount})
let pPosting =
    pipe2 (pOptionalSpace >>. pAccount)
          (pMandatorySpace >>. pAmount .>> pOptionalSpace .>> newline)         
        (fun account amount -> { Posting.account = account;
                                 Posting.amount = amount})

let pPostings =
    (many1 (attempt pPosting))

let pTransaction =
    pipe3 pDate
          (pMandatorySpace >>. (restOfLine false) .>> newline)         
          pPostings
        (fun date description postings ->
           Transaction { Transaction.date = date;
                         Transaction.description = description;
                         Transaction.postings = postings})
    
let pItem =
    pOptionalSpace >>. (pCommentLine <|> pBlankLine <|> pVerifyBalance <|> pTransaction)

let pItems =
    (many pItem)

// Stuff we need to parse a transaction file
let pTransactionFile =
    pItems .>> eof

// Top-level parsing routine(s).
let parseTransactionFile str =
    match run pTransactionFile str with
        | Success(result, _, _) -> ParseSuccess(result)
        | Failure(errorMessage, _, _) -> ParseError(errorMessage)