module Calculations

open InputTypes
open InternalTypes

/// Extract Transaction items
let transactions items =
    let rec helper items soFar =
        match items with
        | (Transaction t) :: tail -> helper tail (t :: soFar)
        | (BalanceVerfication _) :: tail -> helper tail soFar
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper items [])

/// Extract BalanceVerfication items    
let balanceVerifications items =
    let rec helper items soFar =
        match items with
        | (BalanceVerfication b) :: tail -> helper tail (b :: soFar)
        | (Transaction _) :: tail -> helper tail soFar       
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper items [])

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

(* XXX: reinstate
let rec bookPosting (p: Posting) (accounts: Accounts) =
    let rootAccountName = (canonicalRootName p.account)
    let account = if (accounts.ContainsKey rootAccountName) then
                    accounts.[rootAccountName]
                  else
                    failwith (sprintf "Badly initialised accounts object - unable to find root account '%s'" rootAccountName)
    let newAccount = {account with postings=(account.postings.Enqueue p)}
    accounts.Add(rootAccountName,newAccount)

let rec bookPostings (p: Posting list) (accounts: Accounts) =
    match p with
    | [] -> accounts
    | first::rest -> bookPostings rest (bookPosting first accounts)
    
let bookTransaction (t: Transaction) (accounts: Accounts) =
    bookPostings t.postings accounts
*)
        

(*(_name: string) = 
    let name = _name
    do
        if name.Length < 1 then
            raise (BadAccountName(name, "empty account name"))
        if not Account.signMap.ContainsKey (Account.Root name) then
            raise (BadAccountName(name, "invalid top-level account"))
    
    static member signMap = Map.empty.Add("ASSETS", 1)
                                     .Add("LIABILITIES",-1)
                                     .Add("INCOME",-1)
                                     .Add("EXPENSES",-1)
                                     .Add("EQUITY",-1)
    static member canonicalRootNameMap = Map.empty.Add("EXPENSE", "EXPENSES")
                                                  .Add("ASSET", "ASSETS")
                                                  .Add("LIABILITY", "LIABILITIES")
                                                  .Add("REVENUE", "INCOME")
                                                  .Add("REVENUES", "INCOME")
    /// Return regularised version of root account's name (INCOME/EXPENSES/ASSETS/LIABILITIES).
    ///
    /// Basically, we convert to upper case, but we also make sure that
    /// root name is a plural if the singular was used instead. We also
    /// treat 'revenue' or 'revenues' accounts as 'income' accounts.
    static member Root (name: string) =
        let nameComponents = name.ToUpper().Split(':') |> Array.toList
        let root = match nameComponents with
                    | root::tail -> root
                    | [] -> raise (BadAccountName(name, "empty account name"))
        if Account.canonicalRootNameMap.ContainsKey root then
            Account.canonicalRootNameMap.[root]
        else
            root        


*)
