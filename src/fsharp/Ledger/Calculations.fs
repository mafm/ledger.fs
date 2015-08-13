module Calculations

open InputTypes
open InternalTypes
open Misc
open PersistentCollections

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

/// Is transaction unbalanced?
let balance (t:Transaction) =
    let signedAmount (p: Posting) =
        (multAmount (sign (accountType p.account)) p.amount)
    (List.reduce addAmounts (List.map signedAmount t.postings))

let unbalanced (t:Transaction) =
    (balance t) <> zeroAmount

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

/// A tree-structured set of account names. The reason we want this is sometimes we want to deal
/// with a sequence of accounts more or less in parallel, and to visit a particular subset of
/// them. Typically the last set of accounts will contain all the ones we want to visit. Here's
/// a way of specifying the map of accounts we want to traverse.
/// (There must be a better name for this type.)
type AccountNameTree = { name: AccountNameComponents;
                         children: AccountNameTree list;
                         postings: PostingDetail List}

// Given an account (and it's canonical name), construct the tree of account names rooted at the account.
// Here's the only real trick in the whole system:
// If an account has no postings of its own and exactly one sub-account, we treat the subaccount as the
// account. This produces neater reports - especially in text.
let rec constructAccountNameTree (a: Account) (canonicalName: string) =
    let subAccountsWithCNames = [for KeyValue(cName, subAccount) in
                                    (a.subAccounts |> Seq.sortBy (fun (KeyValue(k,v)) -> v.name))
                                 -> (subAccount, cName)]
    match subAccountsWithCNames with
        | [(onlyChild, onlyChildCName)] when (a.postings = PersistentQueue.Empty)
            -> let childTree = (constructAccountNameTree onlyChild onlyChildCName)
               {childTree with name = ({canonical = canonicalName; input = a.name}::childTree.name)}
        | _ -> {name = [{canonical = canonicalName; input = a.name}];
                children = [for (subAccount, cName) in subAccountsWithCNames -> (constructAccountNameTree subAccount cName)];
                postings =  (List.ofSeq a.postings)}

// Construct map where keys are the given dates and values are the "Accounts" at that date.
let accountsByDate (input: InputFile) (dates: Date list) =
    let rec helper (accounts: Accounts) (dates : Date list) (transactions: Transaction list) (soFar:PersistentDictionary<Date,Accounts>) =
        match dates with
        | [] -> soFar
        | date::dates ->
            let accountsAtDate = (accounts.Book (List.filter (fun (t:Transaction) -> t.date <= date) transactions))
            let soFar = soFar.Add(date, accountsAtDate)
            (helper accountsAtDate
                    dates
                    (List.filter (fun (t:Transaction) -> t.date > date) transactions)
                    soFar)
    (helper (new Accounts()) (List.sort dates) (transactions input) PersistentDictionary.Empty)

(* XXX: When I add tests for this module, want to ensure that on the account tree constructed from
   the sample transactions we get the following result:

  let a = Accounts(transactions input)

  let tryit rootAccName =
    match a.find(rootAccName) with
    | None -> None
    | Some a -> Some (constructAccountNameTree a rootAccName)

  (tryit "expenses") ==  Some (Node
                                ([{canonical = "expenses";
                                   input = "EXPENSES";}],
                                 [Node ([{canonical = "ELECTRICITY";
                                          input = "Electricity";}],
                                        []);
                                  Node ([{canonical = "FOOD";
                                          input = "Food";};
                                         {canonical = "GROCERIES";
                                          input = "Groceries";}],
                                        []);
                                  Node ([{canonical = "MOTOR";
                                          input = "Motor";};
                                         {canonical = "FUEL";
                                          input = "Fuel";}],
                                        [])]))
 ie: expenses:motor gets converted to motor:fuel, expenses:food gets converted to food:groceries,
 because in both of these, there is only a single sub-account and that sub-account contains all
 the postings to the original account.
 *)
