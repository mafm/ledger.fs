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
open ReportRegister
open ReportBalances
open ReportFormatting

exception UnableToParseFile of filename: string * message: string

let fatal message =
    printfn "Fatal error: %s" message
    // XXX/TODO: Should generally not delay, unless we know this was started outside
    //           of a shell window - which should not usually be the case.
    printfn "Pausing for 60s to allow you to read this message."
    System.Threading.Thread.Sleep(60 * 1000)
    System.Environment.Exit 1

let nonFatal message =
    printfn "error: %s" message

let parseInputFile filename =
    let parseResult = Parse.readInputFile filename
    match parseResult with
        | ParseSuccess items -> items
        | ParseError message -> raise (UnableToParseFile(filename, message))

/// XXX: To validate input file we need to (at least) check:
///      - transactions are in date order
///      - transactions balance
///      - all account names in transactions are valid.
///      - all balance-verification assertions are true.
let validate (input: InputFile) =
    let transactions = (transactions input)
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
                (nonFatal (sprintf "Imbalance of %s in transaction dated %s (%s).\n"
                                (Text.fmt (absAmount (balance t))) t.date t.description))
                (fatal "Error in input file - unbalanced transactions.")

let demo () =
    try
        let timer = new System.Diagnostics.Stopwatch()
        let input = parseInputFile "c:/Users/mafm/Desktop/working-directories/ledger.fs/examples/sample.transactions" in do
            (validate input)
            printfn "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
            let report = (registerReport input "Expenses") in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: EXPENSES REGISTER\n")
                (printRegisterReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: ALL BALANCES\n")
                let report = (balanceReport input) in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printBalanceReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (printf "\nDEMO: BALANCES BY DATE\n")
                let dates = ["2013-01-05";"2013-01-15"]
                let report = (ReportBalancesByDate.accountBalancesByDateReport input dates) in do
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
                (ReportBalancesByDate.printReport report)
                printf "Elapsed Time: %i ms.\n" timer.ElapsedMilliseconds
    with
    | UnableToParseFile(f,m) ->
        fatal (sprintf "Error in parsing input file: '%s'\n%s" f m)

let usage = """Ledger.fs: simple command-line double-entry accounting.

            Usage:
            Ledger.fs <input-filename> running-balance <account>
            Ledger.fs <input-filename> balances
            Ledger.fs <input-filename> balances-by-date <date>...


            Options:
            -h --help     Show this help."""

[<EntryPoint>]
let main argv =
    let arguments = DocoptNet.Docopt().Apply(usage, argv, exit=true)
    let inputFileName = (string arguments.["<input-filename>"])
    let input = (parseInputFile inputFileName)
    (validate input)

    if (arguments.["running-balance"].IsTrue) then
        (printRegisterReport (registerReport input (string arguments.["<account>"])))

    if (arguments.["balances"].IsTrue) then
        (printBalanceReport (balanceReport input))

    if (arguments.["balances-by-date"].IsTrue) then
                //printf "dates: '%s'"  arguments.["<date>"].AsList()
        (ReportBalancesByDate.printReport
            (ReportBalancesByDate.accountBalancesByDateReport input
            [for d in arguments.["<date>"].AsList -> (string d)]))



    //demo()
    //printfn "%A" argv
    0 // return an integer exit code
