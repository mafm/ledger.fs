module ReportChartOfAccounts

/// Report showing structure of accounts.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Line = { Account: InputNameAccount
              SubAccounts: Line list}

type Report = { Lines: Line list }

let rec constructLine (account: Account) =
    let subAccounts = account.SubAccountsOrderedByInputName
    { Account = (toInputName [account.LastName]);
      SubAccounts = [for child in subAccounts ->
                        (constructLine child)]}
                                                                                                                                        
let addLine (name: InputNameAccount) (accounts: Accounts) linesSoFar =
    match accounts.find(name)  with
        | Some account -> ((constructLine account) :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) =
    let accounts = (Accounts (transactions input))
    {Lines = (addLine (InputName "Assets") accounts
             (addLine (InputName "Liabilities") accounts
             (addLine (InputName "Income") accounts
             (addLine (InputName "Expenses") accounts
             (addLine (InputName "Equity") accounts [])))))}

let rec printLine indent (line : Line) =
    for i in 1 .. indent do
        printf " "
    match line.Account with
        (InputName str) -> printf "%s\n" str
    for subLine in line.SubAccounts do
        printLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    printf "Account\n"
    printf "-------\n"         
    for line in report.Lines do
        printLine 0 line