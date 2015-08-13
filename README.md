# Ledger.fs: _Simple_ command-line double-entry accounting in F#.

**Ledger.fs** is a _simple_, command-line, double-entry accounting
system. It reads transactions written in a simple format from a text
file and produces summary reports as text or an Excel spreadsheet.

Because transaction data is stored as text, it can be managed
using a version control system like git. This makes it easy
to maintain an audit trail.

Ledger.fs is like John Wiegley's
[Ledger](http://www.ledger-cli.org/), but [simpler](https://github.com/mafm/ledger.py/blob/master/doc/Ledger.md).

This code is basically a rewrite of an earlier python implementation:
[ledger.py](https://github.com/mafm/ledger.py/). The F# implementation
is simpler, easier to extend, and runs at least an order of magnitude faster.

**Warning - currently under construction.** This _does_ calculate the
reports described in this README, but doesn't yet do useful extra stuff like:
- multi-currency input-files
- multi-entity input-files

## What it does

Ledger.fs reads an input file that's written in a simple format like this:
```
2013-01-01 I began the year with $1000 in my cheque account.
  Assets:Bankwest:Cheque      $1,000
  Equity:OpeningBalances      $1,000

2013-01-05 I bought some groceries and paid using the cheque account.
  Expenses:Food:Groceries    $98.53
  Assets:Bankwest:Cheque    -$98.53

2013-01-10 I bought some petrol, and paid using a credit card.
  Expenses:Motor:Fuel    $58.01
  Liabilities:Bankwest:Visa   $58.01

2013-01-15 I paid my electricity bill.
  Expenses:Electricity    $280.42
  Assets:Bankwest:Cheque  -$280.42

# I checked my bank statement on the 1st of Feb, and this is what it said.
VERIFY-BALANCE 2013-02-01 Assets:Bankwest:Cheque 621.05
```
and produces reports like this:
```
> Ledger.fs examples/sample.transactions balances
  Balance Account
  ------- -------
  $621.05 Assets:Bankwest:Cheque
   $58.01 Liabilities:Bankwest:Visa
  $436.96 Expenses
  $280.42   Electricity
   $98.53   Food:Groceries
   $58.01   Motor:Fuel
$1,000.00 Equity:OpeningBalances
```
or this:
```
> Ledger examples/sample.transactions balances-by-date 2013-01-05 2013-01-15
     Balance                  Change
  2013-01-05 2013-01-15   2013-01-15 Account
  ---------- ----------   ---------- -------
     $901.47    $621.05     -$280.42 Assets:Bankwest:Cheque
           -     $58.01       $58.01 Liabilities:Bankwest:Visa
      $98.53    $436.96      $338.43 Expenses
           -    $280.42      $280.42   Electricity
      $98.53     $98.53        $0.00   Food:Groceries
           -     $58.01       $58.01   Motor:Fuel
   $1,000.00  $1,000.00        $0.00 Equity:OpeningBalances
```
or this:
```
> Ledger.fs examples/sample.transactions running-balance expenses
      Date       Amount Balance Account                 Description
      ----       ------ ------- -------                 -----------
2013-01-05       $98.53  $98.53 Expenses:Food:Groceries I bought some groceries and paid using the cheque account.
2013-01-10       $58.01 $156.54 Expenses:Motor:Fuel     I bought some petrol, and paid using a credit card.
2013-01-15      $280.42 $436.96 Expenses:Electricity    I paid my electricity bill.
```
## Getting started
The program is currently a visual studio solution. You'll need something from microsoft to build it.
I used visual studio community 2013.
```
# Grab the code
git clone git://github.com/mafm/ledger.fs
cd ledger.fs
# <Whatever you need to do to build this .... it should be straightforward ....>

# Check out the usage
Ledger.fs --help

# Generate a simple report - just the account balances.
# Notice that accounts are organised hierarchically....
Ledger.fs examples/sample.transactions balances

# Calculate balances at two dates, and the changes between them.
Ledger.fs examples/sample.transactions balances-by-date 2013-01-05 2013-01-15

# We can specify *more* than two dates, and see the changes in balances between them!
Ledger.fs examples/sample.transactions balances-by-date 2013-01-05 2013-01-10 2013-01-15

# And see the running-balance balance of an account
Ledger.fs examples/sample.transactions running-balance Expenses
Ledger.fs examples/sample.transactions running-balance Expenses:Electricity
```

There is documentation outlining how to use the ledger.py in the
[Introduction](https://github.com/mafm/ledger.py/blob/master/doc/Introduction.md)
file in its doc folder. Most of that applies to ledger.fs.

### Requirements

I used visual studio community 2013 to build this.

There are a couple of nuget libraries needed to build this. As far as
I know, the package manager in visual studio should fetch them for
you.

#### Testing

I haven't written as many tests as I would like. I should probably
port the old ones over from python.

## Status

I'm now using this software on a regular basis to do real work. I used
[ledger.py](https://github.com/mafm/ledger.py/) for real work on a
regular basis for about two years. I believe that what _has_ been
implemented is more or less correct. However, this program hasn't been
extensively tested, so use it at your own risk.

Lots of useful easy-to-implement features have not yet been
implemented. I am currently (August 2015) working on this and
plan to add the following features asap:
- multi-currency support
- multi-entity support

## Origins

Ledger.fs is an F# rewrite of
[ledger.py](https://github.com/mafm/ledger.py/) which was inspired by John Wiegley's
[Ledger](http://www.ledger-cli.org/)

[Ledger.py](https://github.com/mafm/ledger.py/) is also similar to
some older double-entry accounting software I wrote using wxPython in
2004. Although that program had a GUI, and I used it for nearly ten
years, it was more complex than
[ledger.py](https://github.com/mafm/ledger.py/), and I found it less
convenient to use.

I basically gave up on [ledger.py](https://github.com/mafm/ledger.py/)
because it is pointlessly difficult to refactor python code, and I
needed to add multi-currency and multi-entity support. Now that
Microsoft give away F# development tools, it's hard to justify writing
more python code.

Ledger.fs is simpler than
[ledger.py](https://github.com/mafm/ledger.py/), mainly because it's
easier to restructure F# code without breakage than it is to
restructure python code.
