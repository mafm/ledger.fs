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
        /// I think this might even work ... It did - on the first attempt.
        // It's a lot easier to write working code in F# than it is in python.
        let a = Account("Assets")
        let p = {Posting.account = "Assets:Bankwest:Cheque";
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t, (List.tail (splitAccountName p.account)))
        a2.subAccounts.ContainsKey("BANKWEST") |> should be True
        a2.subAccounts.["BANKWEST"].subAccounts.ContainsKey("CHEQUE") |> should be True
        a2.balance |> should equal (AUD 100000)
        a2.subAccounts.["BANKWEST"].balance |> should equal (AUD 100000)
        a2.subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].balance |> should equal (AUD 100000)
        a2.postings |> should equal (PersistentQueue.Empty.Enqueue detail)
        a2.subAccounts.["BANKWEST"].postings |> should equal (PersistentQueue.Empty.Enqueue detail)
        a2.subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book posting to Accounts.``() =
        /// And this also seems to work on first attempt.
        let a = Accounts()
        let p = {Posting.account = "Assets:Bankwest:Cheque";
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t)
        a2.Accounts.ContainsKey("ASSETS") |> should be True
        a2.Accounts.["ASSETS"].subAccounts.ContainsKey("BANKWEST") |> should be True
        a2.Accounts.["ASSETS"].subAccounts.["BANKWEST"].subAccounts.ContainsKey("CHEQUE") |> should be True
        a2.Accounts.["ASSETS"].balance |> should equal (AUD 100000)
        a2.Accounts.["ASSETS"].subAccounts.["BANKWEST"].balance |> should equal (AUD 100000)
        a2.Accounts.["ASSETS"].subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].balance |> should equal (AUD 100000)
        a2.Accounts.["ASSETS"].postings |> should equal (PersistentQueue.Empty.Enqueue detail)
        a2.Accounts.["ASSETS"].subAccounts.["BANKWEST"].postings |> should equal (PersistentQueue.Empty.Enqueue detail)
        a2.Accounts.["ASSETS"].subAccounts.["BANKWEST"].subAccounts.["CHEQUE"].postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book transaction to Accounts.``() =
        // Maybe this is also right first time.
        let t = {Transaction.date = "2013-01-01";
                 description = "I began the year with $1000 in my cheque account.";
                 // NB: Top-level account "Asset" gets created as "ASSET_S_"
                 postings = [{account = "Asset:Bankwest:Cheque";
                              amount = AUD 100000;};
                             {account = "Equity:OpeningBalances";
                              amount = AUD 100000;}];
             id=1}
        let detail0 = {posting=t.postings.[0];transaction=t}
        let detail1 = {posting=t.postings.[1];transaction=t}
        let a = Accounts()
        let a2 = a.Book(t)
        a2.Accounts.ContainsKey("ASSETS") |> should be True
        a2.Accounts.ContainsKey("EQUITY") |> should be True
        a2.Accounts.["ASSETS"].balance |> should equal (AUD 100000)
        a2.Accounts.["EQUITY"].balance |> should equal (AUD 100000)
        a2.Accounts.["ASSETS"].postings |> should equal (PersistentQueue.Empty.Enqueue detail0)
        a2.Accounts.["EQUITY"].postings |> should equal (PersistentQueue.Empty.Enqueue detail1)
