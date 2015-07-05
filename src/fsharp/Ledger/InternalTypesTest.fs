module InternalTypesTest

open NUnit.Framework
open FsUnit
open Parse
open InputTypes
open InternalTypes
open PersistentCollections


[<TestFixture>]
type ``Test Internal Types`` () =
    [<Test>]
    member test.``splitAccountName.`` () =
        splitAccountName "Expenses:BankFees:AccountServiceFee"
        |> should equal [{canonical = "EXPENSE"; input = "Expenses";};
                         {canonical = "BANKFEES"; input = "BankFees";};
                         {canonical = "ACCOUNTSERVICEFEE"; input = "AccountServiceFee";}]
    [<Test>]
    member test.``Book posting to Account.``() =
        /// I think this might even work ... It does, on the first attempt.
        // It's a lot easier to write working code in F# than it is in python.
        let a = Account("Assets")
        let p = {Posting.account = "Assets:Bankwest:Cheque";
                 Posting.amount = AUD 100000;}
        let a2 = a.Book(p, (List.tail (splitAccountName p.account)))
        a2.subAccounts.ContainsKey("BANKWEST") |> should be True
        a2.subAccounts.["BANKWEST"].subAccounts.ContainsKey("CHEQUE") |> should be True
        a2.balance |> should equal (AUD 100000)
        a2.subAccounts.["BANKWEST"].balance |> should equal (AUD 100000)
        a2.subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].balance |> should equal (AUD 100000)
        a2.postings |> should equal (PersistentQueue.Empty.Enqueue p)
        a2.subAccounts.["BANKWEST"].postings |> should equal (PersistentQueue.Empty.Enqueue p)
        a2.subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].postings |> should equal (PersistentQueue.Empty.Enqueue p)
