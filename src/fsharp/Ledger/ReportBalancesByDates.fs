module ReportBalancesByDate

/// Report showing balances at various dates and changes between those dates.

/// XXX/TODO: Maybe allow optional account name to restrict report to a single (sub-) account????

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open ReportFormatting
open PersistentCollections

type BalancesAndDifferences = {
        // balances will have one entry for each element in the report's date list,
        // differences will have an entry for every date but the first.
        balances : Amount list     
        differences : Amount list}

type ReportBalancesByDateLine = { account: AccountName
                                  amounts: BalancesAndDifferences
                                  subAccounts: ReportBalancesByDateLine list}

type ReportBalancesByDate = {dates: Date list
                             lines: ReportBalancesByDateLine list}

type DatedAccounts = PersistentDictionary<Date, Accounts>



/// Can't I do this without a helper function?
let extractBalance (a: Account option) =
    match a with
    | None -> zeroAmount
    | Some a -> a.balance

/// Can't I do this without a helper function?
let extractSubAccount (a: Account option) (subAccountName: AccountNameComponents)=
    match a with
    | None -> None
    | Some a -> (a.find subAccountName)

/// Diffences between consecutive amounts
let rec differences (amounts: Amount list) =
    let difference (first: Amount) (second: Amount) =
        match first with
            | AUD a -> match second with
                        | AUD b -> (AUD (b - a))
    match amounts with
        | first::second::rest -> (difference first second):: (differences (second::rest))
        | _ -> []

let rec constructReportBalancesByDateLine (accounts: Account option List) (accountTree: AccountNameTree) =
    match accountTree with
        | Node(name, children) ->            
            let balances = (List.map (fun (a:Account option) -> match a with
                                                                | Some account -> account.balance
                                                                | None -> zeroAmount)                                                
                                     accounts)
            { account = (Text.fmt name);
              amounts = {balances = balances; differences = (differences balances)};
              subAccounts = [for child in children ->
                                    (match child with 
                                        | Node(childName, grandChildren) -> (constructReportBalancesByDateLine
                                                                                [for a in accounts -> (extractSubAccount a childName)]
                                                                                child))]}

let addLine (name: AccountName) (accounts: DatedAccounts) (dates: Date list) linesSoFar =
    let lastDate = (List.max dates)
    let finalAccounts = accounts.[lastDate] in        
    match finalAccounts.find(name)  with
        | Some finalAccount -> ((constructReportBalancesByDateLine (List.map (fun date -> accounts.[date].find(name)) dates)
                                                                   (constructAccountNameTree finalAccount finalAccount.name))
                                :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    {dates = dates;
     lines = (addLine "Assets" datedAccounts dates
             (addLine "Liabilities" datedAccounts dates
             (addLine "Income" datedAccounts dates
             (addLine "Expenses" datedAccounts dates
             (addLine "Equity" datedAccounts dates [])))))}

let rec printReportLine indent (line : ReportBalancesByDateLine) =
    for balance in line.amounts.balances do    
        printf "%s\t" (Text.fmt balance)
    for difference in line.amounts.differences do    
        printf "%s\t" (Text.fmt difference)
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
    if (report.dates.Length > 1) then
        printf "Change"
        for i in 1 .. (report.dates.Length-2) do
            printf "\t"
        printf "\n"                            
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