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

let accountBalancesByDateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    {dates = dates;
     lines = (addLine "Income" datedAccounts dates
             (addLine "Expenses" datedAccounts dates
             (addLine "Assets" datedAccounts dates
             (addLine "Liabilities" datedAccounts dates
             (addLine "Equity" datedAccounts dates [])))))}





(* XXX - up to here - finish below *)
(*
(* scrap code *)
(printReport (accountBalancesByDateReport input dates))
let dates = ["2013-01-05";"2013-01-15"]
let accountsByDate = (accountsByDate input dates)

let latestAccounts = accountsByDate.["2013-01-15"]
let latestExpenseAccount = match accountsByDate.["2013-01-15"].find("expenses") with
                           | Some a -> a
                           | None -> failwith "disaster"
let latestAssetAccount = match accountsByDate.["2013-01-15"].find("assets") with
                           | Some a -> a
                           | None -> failwith "disaster"
(constructReportBalancesByDateLine [accountsByDate.["2013-01-05"].find("expenses");accountsByDate.["2013-01-15"].find("expenses")]
    (constructAccountNameTree latestExpenseAccount "EXPENSES"))
(constructReportBalancesByDateLine [accountsByDate.["2013-01-05"].find("assets");accountsByDate.["2013-01-15"].find("assets")]
    (constructAccountNameTree latestAssetAccount "assets"))

(addLine "EXPENSE" accountsByDate dates [])

(* scrap code above here *)

*)
    
let rec printReportLine indent (line : ReportBalancesByDateLine) =
    for balance in line.amounts.balances do    
        printf "%s\t" (Text.fmt balance)
    for difference in line.amounts.differences do    
        printf "%s\t" (Text.fmt difference)
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.account
    for subLine in line.subAccounts do
        printReportLine (indent+1) subLine

let printReport report =
    for date in report.dates do    
        printf "%s\t" (Text.fmtDate date)
    match report.dates with        
        | first::rest ->
            for date in rest do    
                printf "change(%s)\t" date
        | _ -> ()
    printf "Account\n"
    for line in report.lines do
        printReportLine 0 line