/// Report showing running balance in an account

/// XXX/TODO: Should allow start/end dates to be specified.

module ReportRegister

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open PersistentCollections

type RegisterReportLine = {
    date: Date
    amount: Amount
    description: Description
    account: AccountName       // In general, we will book to sub-accounts. Lines show (sub) account we booked to
    balance: Amount
    }

type RegisterReport = {account: AccountName
                       first: Date option
                       last: Date option
                       lines: RegisterReportLine list}

let helpPosting (p:Posting) (account:AccountName) openingBalance (date: Date) (description : Description) (linesSoFar: PersistentQueue<RegisterReportLine>) =
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

let rec helpPostings (p:Posting list) (account:AccountName) openingBalance (date: Date) (description : Description) (linesSoFar: PersistentQueue<RegisterReportLine>) =
    match p with
    | [] -> (linesSoFar, openingBalance)
    | p::rest ->
        let (newLines, newBalance) = helpPosting p account openingBalance date description linesSoFar in
        helpPostings rest account newBalance date description newLines
    
let helpTransaction (t: Transaction) (account:AccountName) openingBalance (linesSoFar: PersistentQueue<RegisterReportLine>) =
    helpPostings t.postings (account:AccountName) openingBalance t.date t.description (linesSoFar: PersistentQueue<RegisterReportLine>)

let rec helpTransactions (t: Transaction list) (account:AccountName) openingBalance (linesSoFar: PersistentQueue<RegisterReportLine>) =
    match t with
    | [] -> (linesSoFar, openingBalance)
    | t::rest -> let (newLines, newBalance) = helpTransaction t account openingBalance linesSoFar in
                 helpTransactions rest account newBalance newLines

let registerReport (input: InputFile) (account: AccountName) = 
    let (lines, finalBalance) = helpTransactions (transactions input) account zeroAmount PersistentQueue.Empty
    {account = account;
     first = None;
     last = None;
     lines = (List.ofSeq lines)}

let printRegisterReportLine line =
    printf "Date: %s " line.date
    printf "Amount: %A " line.amount
    printf "Balance: %A " line.balance
    printf "Account: %s " line.account
    printf "Description: %s\n" line.description

let printRegisterReport report =
    printf "Account: %s " report.account    
    match report.first with
    | Some date -> printf "From: %s " date
    | None -> ()
    match report.last with
    | Some date -> printf "Until: %s " date
    | None -> ()
    printf "\n"
    for line in report.lines do
        printRegisterReportLine line


