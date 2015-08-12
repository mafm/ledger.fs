module ReportTransactionList

/// Report showing transactions in a date range.

open Parse
open Calculations
open InputTypes
open InternalTypes
open Misc
open TextOutput
open PersistentCollections

type Line = 
    { id: int
      transaction: Transaction }

type Report = { first: Date Option
                last:  Date Option
                lines: Line list}

let filter (transactions : Transaction list) (first : Date option) (last : Date option) =
    let transactions = match first with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date >= date) transactions
    let transactions = match last with
                        | None -> transactions
                        | Some date -> List.filter (fun t -> t.date <= date) transactions
    transactions

let generateReport (input: InputFile) firstDate lastDate =   
    let addTransaction (t: Transaction) (nextId: int) (resultSoFar: PersistentCollections.PersistentQueue<Line>) =
        match firstDate with
            | Some firstDate when (firstDate > t.date) -> resultSoFar
            | _ -> match lastDate with
                    | Some lastDate when (lastDate < t.date) -> resultSoFar
                    | _ -> (resultSoFar.Enqueue {id=nextId; transaction=t})
            
    let rec addLines (t: Transaction List) (nextId: int) (resultSoFar: PersistentCollections.PersistentQueue<Line>) =
        match t with
        | [] -> resultSoFar
        | first::rest -> (addLines rest (nextId+1) (addTransaction first nextId resultSoFar))
    {first = firstDate
     last  = lastDate
     lines = (List.ofSeq (addLines (transactions input) 1 PersistentCollections.PersistentQueue.Empty))}

let rec printLine (line : Line) =
    printf "%d\t" line.id
    printf "%s\t" (Text.fmtDate line.transaction.date)
    printf "\t" 
    printf "%s\t" line.transaction.description
    for p in line.transaction.postings do
        printf "\t\t%s\t" (Text.fmt p.amount)
        printf "%s\n" (Text.fmtAccountName p.account)    

let printReport report =        
    match report.first with
        | None -> printf "\t\t" 
        | Some date -> printf "From\t%s\t" (Text.fmtDate date)
    match report.last with
        | None -> printf "\t\t" 
        | Some date -> printf "To\t%s\t" (Text.fmtDate date)
    match (report.first, report.last) with
        | (None,None) -> ()
        | _ -> printf "\n"
    printf "Transaction#\tDate\tAmount\tDescription/Account\n"
    for line in report.lines do
        printLine line
