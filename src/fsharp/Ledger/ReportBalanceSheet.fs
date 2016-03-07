module ReportBalanceSheet

/// Report showing balances at various dates. No changes in this report.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Balances = {
        // balances will have one entry for each element in the report's date list.
        balances : Amount list}

type Line = { account: AccountName
              amounts: Balances
              subAccounts: Line list
              postings: PostingDetail list}

type Report = {dates: Date list
               lines: Line list}

type DatedAccounts = PersistentDictionary<Date, Accounts>

/// Can't I do this without a helper function?
/// XXX: duplicate code.
let extractBalance (a: Account option) =
    match a with
    | None -> zeroAmount
    | Some a -> a.balance

/// Can't I do this without a helper function?
/// XXX: duplicate code.
let extractSubAccount (a: Account option) (subAccountName: AccountNameComponents)=
    match a with
    | None -> None
    | Some a -> (a.find subAccountName)

/// Diffences between consecutive amounts
/// XXX: duplicate code
let rec differences (amounts: Amount list) =
    let difference (first: Amount) (second: Amount) =
        match first with
            | AUD a -> match second with
                        | AUD b -> (AUD (b - a))
    match amounts with
        | first::second::rest -> (difference first second):: (differences (second::rest))
        | _ -> []

let rec constructReportBalanceSheetLine (accounts : Account option List) (accountTree : AccountNameTree) =
    let balances =
        (List.map (fun (a : Account option) ->
                        match a with
                        | Some account -> account.balance
                        | None -> zeroAmount)
                  accounts)
    { account = (Text.fmt accountTree.name)
      amounts =
          { balances = balances }
      subAccounts =
          [ for child in accountTree.children ->
                (constructReportBalanceSheetLine [ for a in accounts -> (extractSubAccount a child.name) ] child) ]
      postings = accountTree.postings }

let addLine (name: AccountName) (accounts: DatedAccounts) (dates: Date list) linesSoFar =
    let lastDate = (List.max dates)
    let finalAccounts = accounts.[lastDate] in
    match finalAccounts.find(name)  with
        | Some finalAccount -> ((constructReportBalanceSheetLine (List.map (fun date -> accounts.[date].find(name)) dates)
                                                                   (constructAccountNameTree finalAccount finalAccount.name))
                                :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    {dates = dates;
     lines = (addLine "Assets" datedAccounts dates
             (addLine "Liabilities" datedAccounts dates
             (addLine "Equity" datedAccounts dates [])))}

let rec printReportLine indent (line : Line) =
    for balance in line.amounts.balances do
        printf "%s\t" (Text.fmt balance)    
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.account
    for subLine in line.subAccounts do
        printReportLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    if (report.dates.Length > 0) then
        printf "Balance\t"
        for i in 1 .. (report.dates.Length-1) do
            printf "\t"    
    (* date/"Account" headings line*)
    for date in report.dates do
        printf "%s\t" (Text.fmtDate date)
    match report.dates with
        | first::rest ->
            for date in rest do
                printf "%s\t" date
        | _ -> ()
    printf "Account\n"
    (printf "%s%s-------\n"
        (String.replicate report.dates.Length "----------\t")
        (String.replicate (report.dates.Length-1) "----------\t"))
    for line in report.lines do
        printReportLine 0 line
