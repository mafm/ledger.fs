module Types

// Will start this quick and dirty, and upgrade types as practically required.
//
// This began life as a rewrite of python code, so any static typing here is better than that.

type Date = string
type Description = string
type Account = string

type Amount =
    /// AUD amounts are stored as cents, and converted to dollars on input/output. 
    | AUD of int

type Posting = { account: Account
                 amount:  Amount}               

type Transaction = { date:    Date
                     description: Description
                     postings: Posting list}                     

type BalanceVerfication = { date:    Date
                            account: Account
                            amount: Amount}

type Item =
    | Transaction of Transaction
    | BalanceVerfication of BalanceVerfication
    | BlankLine
    | Comment of string
                                 
type TransactionFile = Item list
