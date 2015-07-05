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

type Posting = { account: AccountName
                 amount:  Amount}               

type Transaction = { date:    Date
                     description: Description
                     postings: Posting list}                     

type BalanceVerfication = { date:    Date
                            account: AccountName
                            amount: Amount}

type Item =
    | Transaction of Transaction
    | BalanceVerfication of BalanceVerfication
    | BlankLine
    | Comment of string
                                 
type TransactionFile = Item list

