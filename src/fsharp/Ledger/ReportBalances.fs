module ReportBalances

/// Report showing final balance in all accounts

/// XXX/TODO: Maybe allow optional account name to restrict report to a single (sub-) account????

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open PersistentCollections

type BalanceReportLine = { account: AccountName
                           balance: Amount
                           subAccounts: BalanceReportLine list}

type BalanceReport = {lines: BalanceReportLine list}

let rec accountBalanceReport (name:string)  (a: Account) =
    let subAccounts = [for KeyValue(name, account) in (a.subAccounts|>Seq.sortBy (fun (KeyValue(k,v)) -> k) ) -> account]
    // Here's the only real trick in the whole system.
    // If an account has no postings of its own and exactly one sub-account, we treat that subaccount as the
    // account. This produces much neater reports - if you like the text reports.
    match subAccounts with
    | [(onlyChild)] when (a.postings = onlyChild.postings) 
        -> (accountBalanceReport (name + ":" + onlyChild.name) onlyChild)    
    | _ -> { account = name;
             balance = a.balance;
             subAccounts = [for account in subAccounts -> (accountBalanceReport account.name account)]}

let balanceReport (input: InputFile) =    
    let accounts = Accounts(transactions input)
    let addLine account lines =
        match (accounts.find account) with
        | None -> lines
        | Some a -> (accountBalanceReport a.fullName a)::lines    
    {lines = (addLine "Income"
             (addLine "Expenses"
             (addLine "Assets"
             (addLine "Liabilities"
             (addLine "Equity" [])))))}
    
let rec printBalanceReportLine indent (line : BalanceReportLine) =    
    printf "%A\t" line.balance     
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.account
    for subLine in line.subAccounts do
        printBalanceReportLine (indent+1) subLine

let printBalanceReport report =
    for line in report.lines do
        printBalanceReportLine 0 line
