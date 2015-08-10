module ReportChartOfAccounts

/// Report showing structure of accounts.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open ReportFormatting
open PersistentCollections

type Line = { account: AccountName     
              subAccounts: Line list}

type Report = { lines: Line list }

let rec constructLine (account: Account) =
    let subAccounts = [for KeyValue(name, account) in (account.subAccounts|>Seq.sortBy (fun (KeyValue(k,v)) -> k) ) -> account]
    { account = (Text.fmtAccountName account.name);              
              subAccounts = [for child in subAccounts ->
                                 (constructLine child)]}
                                                                                                                                        
let addLine (name: AccountName) (accounts: Accounts) linesSoFar =   
    match accounts.find(name)  with
        | Some account -> ((constructLine account) :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) =
    let accounts = (Accounts (transactions input))
    {lines = (addLine "Assets" accounts
             (addLine "Liabilities" accounts
             (addLine "Income" accounts
             (addLine "Expenses" accounts
             (addLine "Equity" accounts [])))))}

let rec printLine indent (line : Line) =    
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.account
    for subLine in line.subAccounts do
        printLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)    
    printf "Account\n"
    printf "-------\n"         
    for line in report.lines do
        printLine 0 line