module Calculations

open Types

/// Extract Transaction items
let transactions items =
    let rec helper items soFar =
        match items with
        | (Transaction t) :: tail -> helper tail (t :: soFar)
        | (BalanceVerfication _) :: tail -> helper tail soFar
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper items [])

/// Extract BalanceVerfication items    
let balanceVerifications items =
    let rec helper items soFar =
        match items with
        | (BalanceVerfication b) :: tail -> helper tail (b :: soFar)
        | (Transaction _) :: tail -> helper tail soFar       
        | (BlankLine :: tail) -> helper tail soFar
        | (Comment _) :: tail -> helper tail soFar    
        | [] -> soFar
    List.rev (helper items [])

type DateOrderCheck =
    | OK 
    | Problem of Date * Description * Date * Description

let checkDateOrder (transactions : Transaction list) =
    let rec helper (previous : Transaction) (transactions : Transaction list) =
        match transactions with 
            | [] -> DateOrderCheck.OK
            | (t :: tail) -> 
                if t.date < previous.date then
                    DateOrderCheck.Problem(previous.date, previous.description,
                                            t.date, t.description)
                else
                    (helper t tail)                                                
    match transactions with
        | [] -> DateOrderCheck.OK
        | (t :: tail) -> (helper t tail)            
    
            
    