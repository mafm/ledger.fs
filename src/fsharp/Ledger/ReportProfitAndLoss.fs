module ReportProfitAndLoss

/// Report showing changes between (revenue/expense) balances at various dates.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Amounts = {
        // differences will have an entry for every date.
        // First difference is just the first amount, after that, it's the change between amounts.
        Differences : Amount list}

type Line = { Account: InputNameAccount
              Amounts: Amounts
              SubAccounts: Line list
              Postings: PostingDetail list}

type Report = { Dates: Date list
                Lines: Line list}

type DatedAccounts = PersistentDictionary<Date, Accounts>

/// Can't I do this without a helper function?
/// XXX: duplicate code!
let extractBalance (a: Account option) =
    match a with
    | None -> zeroAmount
    | Some a -> a.Balance

/// Can't I do this without a helper function?
/// XXX: duplicate code!
let extractSubAccount (a: Account option) (subAccountName: InternalNameAccount)=
    match a with
    | None -> None
    | Some a -> (a.find subAccountName)

/// Differences between consecutive amounts
/// NOT duplicate code - keeps last one
let rec differences (amounts: Amount list) =
    let difference (first: Amount) (second: Amount) =
        match first with
            | AUD a -> match second with
                        | AUD b -> (AUD (b - a))
    let rec helper previous amounts =
        match amounts with 
            | first::rest -> (difference previous first) :: (helper first rest)
            | [] -> []
    match amounts with
        | first::rest -> first :: (helper first rest)
        | [] -> []

let rec constructReportProfitAndLossLine (accounts : Account option List) (accountTree : AccountNameTree) =
    let balances =
        (List.map (fun (a : Account option) ->
                        match a with
                        | Some account -> account.Balance
                        | None -> zeroAmount)
                  accounts)
    { Account = (InputName (Text.fmt accountTree.Name))
      Amounts =
          { Differences = (differences balances) }
      SubAccounts =
          [ for child in accountTree.Children ->
                (constructReportProfitAndLossLine [ for a in accounts -> (extractSubAccount a child.Name) ] child) ]
      Postings = accountTree.Postings }

let addLine (name: InputNameAccount) (accounts: DatedAccounts) (dates: Date list) linesSoFar =
    let lastDate = (List.max dates)
    let finalAccounts = accounts.[lastDate] in
    match finalAccounts.find(name)  with
        | Some finalAccount -> ((constructReportProfitAndLossLine (List.map (fun date -> accounts.[date].find(name)) dates)
                                                                  (constructAccountNameTree finalAccount))
                                :: linesSoFar)
        | None -> linesSoFar

let generateReport (input: InputFile) (dates: Date list)  =
    let datedAccounts = (accountsByDate input dates)
    {Dates = dates;
     Lines = (addLine (InputName "Income") datedAccounts dates
             (addLine (InputName "Expenses") datedAccounts dates []))}

let rec printReportLine indent (line : Line) =
    for difference in line.Amounts.Differences do
        printf "%s\t" (Text.fmt difference)
    for i in 1 .. indent do
        printf " "
    printf "%s\n" line.Account.AsString
    for subLine in line.SubAccounts do
        printReportLine (indent+2) subLine

let printReport report =
    (* Balance/Change headings line *)
    if (report.Dates.Length > 0) then
        printf "Balance\t"
        for i in 1 .. (report.Dates.Length-1) do
            printf "\t"
    if (report.Dates.Length > 1) then
        printf "Change"
        for i in 1 .. (report.Dates.Length-2) do
            printf "\t"
        printf "\n"
    (* date/"Account" headings line*)
    for date in report.Dates do
        printf "%s\t" (Text.fmtDate date)
    match report.Dates with
        | first::rest ->
            for date in rest do
                printf "%s\t" date
        | _ -> ()
    printf "Account\n"
    (printf "%s%s-------\n"
        (String.replicate report.Dates.Length "----------\t")
        (String.replicate (report.Dates.Length-1) "----------\t"))
    for line in report.Lines do
        printReportLine 0 line
