module Calculations

open InputTypes
open InternalTypes
open Misc

/// Extract Transaction items
let transactions inputs =
    let rec helper items soFar =
        match items with
        | (Transaction t) :: tail -> helper tail (t :: soFar)
        | (BalanceVerfication _) :: tail -> helper tail soFar
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper inputs [])

/// Extract BalanceVerfication items    
let balanceVerifications inputs =
    let rec helper items soFar =
        match items with
        | (BalanceVerfication b) :: tail -> helper tail (b :: soFar)
        | (Transaction _) :: tail -> helper tail soFar       
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper inputs [])

type DateOrderCheck =
    | OK 
    | Problem of previous: Transaction * next: Transaction

/// Check transactions are in date order. Give two problem transactions if not.
let checkDateOrder (transactions : Transaction list) =
    let rec helper (previous : Transaction) (transactions : Transaction list) =
        match transactions with 
            | [] -> DateOrderCheck.OK
            | (t :: tail) -> 
                if t.date < previous.date then
                    DateOrderCheck.Problem(previous, t)
                else
                    (helper t tail)                                                
    match transactions with
        | [] -> DateOrderCheck.OK
        | (t :: tail) -> (helper t tail)            

/// Filter transactions by optional (inclusive) dates.
let filter (transactions : Transaction list) (first : Date option) (last : Date option) =
    let transactions = match first with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date >= date) transactions
    let transactions = match last with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date <= date) transactions
    transactions

// Is a a sub-account of b?
let isSubAccountOf (a: AccountName) (b: AccountName) =
    startsWith (canonicalAccountName a) (canonicalAccountName b)

/// XXX: affectedBy(Posting/Transaction) should be a method on AccountName, which should be a class. Do we even need these at all?

let postingAffects (p:Posting) (a: AccountName) =
    (isSubAccountOf p.account a)
/// XXX: affectedBy(Posting/Transaction) should be a method on AccountName, which should be a class. Do we even need these at all?
let transactionAffects (t: Transaction) (a: AccountName) =
    let rec helper postings =
        match postings with
            | p::postings -> match (postingAffects p a) with
                                | true -> true
                                | false -> (helper postings)
            | [] -> false
    (helper t.postings)

