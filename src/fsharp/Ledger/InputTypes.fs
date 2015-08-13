module InputTypes

// Will start this quick and dirty, and upgrade types as practically required.
//
// This began life as a rewrite of python code, so any static typing here is better than that.

exception BadAccountName of name: string * problem: string

type Date = string
type Description = string

type AccountName = string

type Amount =
    /// AUD amounts are stored as cents, and converted to dollars on input/output.
    | AUD of int

let zeroAmount = AUD 0

/// Add two amounts
let addAmounts (a: Amount) (b: Amount) =
   match (a, b) with
    | (AUD a, AUD b) -> AUD (a+b)

let multAmount (i: int) (a: Amount) =
    match a with
    | AUD a -> AUD (i * a)

let absAmount (a: Amount) =
    match a with
    | AUD a -> AUD (abs a)

type Posting = { account: AccountName
                 amount:  Amount}

type Transaction = { date:    Date
                     description: Description
                     postings: Posting list
                     id: int}

type BalanceVerfication = { date:    Date
                            account: AccountName
                            amount: Amount}

type Input =
    | Transaction of Transaction
    | BalanceVerfication of BalanceVerfication
    | BlankLine
    | Comment of string

type InputFile = Input list
