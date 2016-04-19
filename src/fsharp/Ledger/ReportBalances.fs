module ReportBalances

/// Report showing final balance in all accounts

/// XXX/TODO: Maybe allow optional account name to restrict report to a single (sub-) account????

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Line =
    { Account : InputNameAccount
      Balance : Amount
      SubAccounts : Line list }

type Report = {lines: Line list}

let rec accountBalanceReport (name:InputNameAccount)  (a: Account) =
    let subAccounts = [for KeyValue(name, account) in (a.SubAccounts|>Seq.sortBy (fun (KeyValue(k,_)) -> k) ) -> account]
    // Here's the only real trick in the whole system.
    // If an account has no postings of its own and exactly one sub-account, we treat that subaccount as the
    // account. This produces much neater reports - if you like the text reports.
    match subAccounts with
    | [(onlyChild)] when (a.Postings = PersistentQueue.Empty)
        -> (accountBalanceReport (joinInputNames name onlyChild.LastName.Input) onlyChild)
    | _ -> { Account = name;
             Balance = a.Balance;
             SubAccounts = [for account in subAccounts -> (accountBalanceReport account.LastName.Input.AsInputName account)]}

let generateReport (input: InputFile) =
    let accounts = Accounts(transactions input)
    let addLine (account:InputNameAccount) lines =
        match (accounts.find account) with
        | None -> lines
        | Some a -> (accountBalanceReport a.FullInputName a)::lines
    {lines = (addLine (InputName "Assets")
             (addLine (InputName "Liabilities")
             (addLine (InputName "Income")
             (addLine (InputName "Expenses")
             (addLine (InputName "Equity") [])))))}

let rec printBalanceReportLine indent (line : Line) =
    printf "%s\t" (Text.fmt line.Balance)
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.Account.AsString
    for subLine in line.SubAccounts do
        printBalanceReportLine (indent+2) subLine

let printBalanceReport report =
    printf "Balance\tAccount\n-------\t-------\n"
    for line in report.lines do
        printBalanceReportLine 0 line
