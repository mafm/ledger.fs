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

exception UnableToParseFile of filename: string * message: string

let parseTransactionFile filename =
    let parseResult = Parse.readTransactionFile filename
    match parseResult with
        | ParseSuccess items -> items
        | ParseError message -> raise (UnableToParseFile(filename, message))

let fatal message =
    printfn "Fatal error: %s" message
    // XXX/TODO: Should generally not delay, unless we know this was started outside
    //           of a shell window - which should not usually be the case.
    printfn "Pausing for 60s to allow you to read this message."
    System.Threading.Thread.Sleep(60 * 1000)
    System.Environment.Exit 1

[<EntryPoint>]
let main argv =
    try
        let input = parseTransactionFile "c:/Users/mafm/Desktop/working-directories/ledger.fs/examples/sample.transactions" in do
        match checkDateOrder (transactions input) with
            | OK -> ()
            | Problem(prev, next) ->
                fatal (sprintf "Transactions not in date order.\n\
                                Transaction dated %s\n\
                                \t%s\n\
                                after transaction dated %s\n\
                                \t%s"
                               next.date next.description prev.date prev.description)
    with
    | UnableToParseFile(f,m) ->
        fatal (sprintf "Error in parsing input file: '%s'\n%s" f m)
    printfn "%A" argv
    0 // return an integer exit code
