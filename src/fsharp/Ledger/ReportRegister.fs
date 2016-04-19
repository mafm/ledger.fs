/// Report showing running balance in an account

/// XXX/TODO: Should allow from/until dates to be specified.

module ReportRegister

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput

open PersistentCollections

type Line = {date: Date
             amount: Amount
             balance: Amount
             description: Description
             // In transactions often post to a sub-account of target a/c.
             // Record the (sub-)account actually posted to.
             account: InputNameAccount}

type Report = {account: InputNameAccount
               from: Date option
               until: Date option
               lines: Line list}

let helpPosting (p:Posting) (account:InputNameAccount) openingBalance (date: Date) (description : Description) (linesSoFar: PersistentQueue<Line>) =
    if   (isSubAccountOf p.account account) then
        let newBalance = (addAmounts openingBalance p.amount) in
            ((linesSoFar.Enqueue {date=date;
                                  account=p.account;
                                  amount = p.amount;
                                  balance = newBalance;
                                  description=description}),
             newBalance)

    else
        (linesSoFar, openingBalance)

let rec helpPostings (p:Posting list) (account:InputNameAccount) openingBalance (date: Date) (description : Description) (linesSoFar: PersistentQueue<Line>) =
    match p with
    | [] -> (linesSoFar, openingBalance)
    | p::rest ->
        let (newLines, newBalance) = helpPosting p account openingBalance date description linesSoFar in
        helpPostings rest account newBalance date description newLines

let helpTransaction (t: Transaction) (account:InputNameAccount) openingBalance (linesSoFar: PersistentQueue<Line>) =
    helpPostings t.postings (account:InputNameAccount) openingBalance t.date t.description (linesSoFar: PersistentQueue<Line>)

let rec helpTransactions (t: Transaction list) (account:InputNameAccount) openingBalance (linesSoFar: PersistentQueue<Line>) =
    match t with
    | [] -> (linesSoFar, openingBalance)
    | t::rest -> let (newLines, newBalance) = helpTransaction t account openingBalance linesSoFar in
                 helpTransactions rest account newBalance newLines

let generateReport (input: InputFile) (account: InputNameAccount) =
    let (lines, finalBalance) = helpTransactions (transactions input) account zeroAmount PersistentQueue.Empty
    {account = account;
     from = None;
     until = None;
     lines = (List.ofSeq lines)}

let printRegisterReportLine line =
    printf "%s\t" line.date
    printf "%s\t" (Text.fmt line.amount)
    printf "%s\t" (Text.fmt line.balance)
    printf "%s\t" line.account.AsString
    printf "%s\n" line.description

let printRegisterReport report =
    printf "Date\tAmount\tBalance\tAccount\tDescription\n"
    printf "----\t------\t-------\t-------\t-----------\n"
    for line in report.lines do
        printRegisterReportLine line
