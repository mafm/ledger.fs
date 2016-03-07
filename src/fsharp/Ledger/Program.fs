// This file is part of ledger.fs.
//
// ledger.fs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// ledger.fs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with ledger.fs.  If not, see <http://www.gnu.org/licenses/>.

/// Command-line, double-entry accounting in F#.
///
/// Inspired by John Wiegley's Ledger:
///   http://www.ledger-cli.org/
///
/// This is a F# rewrite of almost the same thing in python:
///   https://github.com/mafm/ledger.py

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput

open FormatExceptionForDisplay

exception UnableToParseFile of filename: string * message: string

let fatal message =
    printfn "\nFatal error: %s" message
    // XXX/TODO: Should generally not delay, unless we know this was started outside
    //           of a shell window - which should not usually be the case.
    System.Environment.Exit 1

let nonFatal message =
    printfn "error: %s" message

let parseInputFile filename =
    let parseResult = Parse.readInputFile filename
    match parseResult with
        | ParseSuccess items -> items
        | ParseError message -> raise (UnableToParseFile(filename, message))

let validateAccountNames (transactions: Transaction List) =
    let mutable allOk = true
    for t in transactions do
        for p in t.postings do
            if not (validAccountName p.account) then
                (nonFatal (sprintf "Invalid account name '%s' in transaction dated %s (%s)."
                                p.account t.date t.description))
                allOk <- false
    if not allOk then
        (fatal "Error in input file - invalid account name(s).")

let validateBalanceAssertions input =
    let mutable allOk = true
    let assertions = (balanceVerifications input)
    let dates = (List.ofSeq (Seq.sort (Set.ofSeq [for a in assertions -> a.date])))
    let accountsByDate = (accountsByDate input dates) in
        for assertion in assertions do
            if not (validAccountName assertion.account) then
                (nonFatal (sprintf "Error in verify-balance dated %s - invalid account '%s'." assertion.date assertion.account))
                allOk <- false
            else
                match accountsByDate.[assertion.date].find(assertion.account) with
                | None ->
                        (nonFatal (sprintf "Error in verify-balance. Account '%s' has no bookings at date %s."
                                                    assertion.account assertion.date))
                        allOk <- false
                | Some account ->
                       if account.balance <> assertion.amount then
                            (nonFatal (sprintf "Error in verify-balance. Expected balance of '%s' at %s: %s actual balance: %s"
                                            assertion.account
                                            assertion.date
                                            (Text.fmt assertion.amount)
                                            (Text.fmt account.balance)))
                            allOk <- false
            if not allOk then
                (fatal "Error in input file - incorrect verify-balance assertion(s).")

let validate (input: InputFile) =
    let transactions = (transactions input)
    (validateAccountNames transactions)
    match checkDateOrder transactions with
            | OK -> ()
            | Problem(prev, next) ->
                fatal (sprintf "Transactions not in date order.\n\
                                Transaction dated %s\n\
                                \t%s\n\
                                after transaction dated %s\n\
                                \t%s"
                               next.date next.description prev.date prev.description)
    match (List.filter unbalanced transactions) with
        | [] -> ()
        | unbalanced ->
            for t in unbalanced do
                (nonFatal (sprintf "Imbalance of %s in transaction dated %s (%s)."
                                (Text.fmt (absAmount (balance t))) t.date t.description))
            (fatal "Error in input file - unbalanced transactions.")
    (validateBalanceAssertions input)

let demo () =
    try
        let timer = new System.Diagnostics.Stopwatch()
        let input = parseInputFile "c:/Users/mafm/Desktop/working-directories/ledger.fs/examples/sample.transactions" in do
            (validate input)
            printfn "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
            let report = (ReportRegister.generateReport input "Expenses") in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: EXPENSES REGISTER\n")
                (ReportRegister.printRegisterReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: ALL BALANCES\n")
                let report = (ReportBalances.generateReport input) in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (ReportBalances.printBalanceReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: BALANCES BY DATE\n")
                let dates = ["2013-01-05";"2013-01-15"]
                let report = (ReportBalancesByDate.generateReport input dates) in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (ReportBalancesByDate.printReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
    with
    | UnableToParseFile(f,m) ->
        fatal (sprintf "Error in parsing input file: '%s'\n%s" f m)

let usage = """Ledger.fs: simple command-line double-entry accounting.

            Usage:
            Ledger.fs <input-filename> [--excel-output=<filename>] running-balance <account>
            Ledger.fs <input-filename> [--excel-output=<filename>] balances
            Ledger.fs <input-filename> [--excel-output=<filename>] balances-by-date <date>...
            Ledger.fs <input-filename> [--excel-output=<filename>] chart-of-accounts
            Ledger.fs <input-filename> [--excel-output=<filename>] transactions [<first-date>] [<last-date>]
            Ledger.fs <input-filename> --excel-output=<filename> summary <date>...


            Options:
            -h --help                   Show this help.
            --excel-output=<filename>   Generate Excel-readable report.

            The summary option produces:
            - balances-by-date
            - transaction list
            - chart-of-accounts

            in a single excel file. Note that in this case, output to excel is NOT optional.
            """

[<EntryPoint>]
let main argv =
  try
    let arguments = DocoptNet.Docopt().Apply(usage, argv, exit=true)
    let inputFileName = (string arguments.["<input-filename>"])
    let dates = ([for d in arguments.["<date>"].AsList -> (string d)] |> List.sort)
    let firstDate = match (string arguments.["<first-date>"]) with | "" -> None | date -> Some date
    let lastDate = match (string arguments.["<last-date>"]) with | "" -> None | date -> Some date
    let input = (parseInputFile inputFileName)
    (validate input)
    let destination = ExcelOutput.destination(string arguments.["--excel-output"])

    if (arguments.["running-balance"].IsTrue) then
        let report = (ReportRegister.generateReport input (string arguments.["<account>"]))
        (ReportRegister.printRegisterReport report)
        ExcelOutput.Excel.write(report, destination)

    if (arguments.["balances"].IsTrue) then
      let report = (ReportBalances.generateReport input)
      (ReportBalances.printBalanceReport report)
      ExcelOutput.Excel.write(report, destination)

    if (arguments.["balances-by-date"].IsTrue) then
        if dates.Length < 1 then
            fatal("balances-by-date requires at least one date.")
        let report = (ReportBalancesByDate.generateReport input dates)
        (ReportBalancesByDate.printReport report)
        ExcelOutput.Excel.write(report, destination)

    if (arguments.["chart-of-accounts"].IsTrue) then
        let report = (ReportChartOfAccounts.generateReport input)
        (ReportChartOfAccounts.printReport report)
        ExcelOutput.Excel.write(report, destination)

    if (arguments.["transactions"].IsTrue) then
        let report = (ReportTransactionList.generateReport input firstDate lastDate)
        (ReportTransactionList.printReport report)
        ExcelOutput.Excel.write(report, destination)

    if (arguments.["summary"].IsTrue) then
        if dates.Length < 1 then
            fatal("summary requires at least one date.")
        let lastDate = Some (List.max dates)
        ExcelOutput.Excel.write((ReportProfitAndLoss.generateReport input dates), destination)
        ExcelOutput.Excel.write((ReportBalanceSheet.generateReport input dates), destination)
        ExcelOutput.Excel.write((ReportBalancesByDate.generateReport input dates), destination)
        ExcelOutput.Excel.write((ReportChartOfAccounts.generateReport input), destination)
        ExcelOutput.Excel.write((ReportTransactionList.generateReport input None lastDate), destination)

    ExcelOutput.save(destination)
    0
  with
    |  UnableToParseFile(filename, message) ->
        fatal(sprintf "Error parsing input file '%s' : %s" filename message)
        -1
    | :? System.IO.IOException as e ->
        fatal(sprintf "IO error: %s" e.Message)
        -1
    | e ->
        fatal (sprintf "\n%s" (e|>formatDisplayMessage))
        -1



    //demo()
