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

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
