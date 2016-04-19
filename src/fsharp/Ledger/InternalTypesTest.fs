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
        splitAccountName (InputName "Expenses:BankFees:AccountServiceFee")
        |> should equal [{Canonical = (Canonical "EXPENSE"); Input = (Input "Expenses");};
                         {Canonical = (Canonical "BANKFEES"); Input = (Input "BankFees");};
                         {Canonical = (Canonical "ACCOUNTSERVICEFEE"); Input = (Input "AccountServiceFee");}]
    [<Test>]
    member test.``Book posting to Account.``() =
        /// I think this might even work ... It did - on the first attempt.
        // It's a lot easier to write working code in F# than it is in python.
        let a = Account(InputName "Assets")
        let p = {Posting.account = (InputName "Assets:Bankwest:Cheque");
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t, (List.tail (splitAccountName p.account)))
        a2.SubAccounts.ContainsKey(Canonical "BANKWEST") |> should be True
        a2.SubAccounts.[Canonical "BANKWEST"].SubAccounts.ContainsKey(Canonical "CHEQUE") |> should be True
        a2.Balance |> should equal (AUD 100000)
        a2.SubAccounts.[Canonical "BANKWEST"].Balance |> should equal (AUD 100000)
        a2.SubAccounts.[Canonical "BANKWEST"].SubAccounts.[Canonical "CHEQUE"].Balance |> should equal (AUD 100000)
        a2.Postings |> should equal PersistentQueue.Empty
        a2.SubAccounts.[Canonical "BANKWEST"].Postings |> should equal PersistentQueue.Empty
        a2.SubAccounts.[Canonical "BANKWEST"].SubAccounts.[Canonical "CHEQUE"].Postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book posting to Accounts.``() =
        /// And this also seems to work on first attempt.
        let a = Accounts()
        let p = {Posting.account = (InputName "Assets:Bankwest:Cheque");
                 Posting.amount = AUD 100000;}
        let t = {Transaction.id = 1;
                 Transaction.date = "1999-12-31";
                 Transaction.postings = [p];
                 Transaction.description = "dummy"}
        let detail = {posting=p;transaction=t}
        let a2 = a.Book(p, t)
        a2.Accounts.ContainsKey(Canonical "ASSETS") |> should be True
        a2.Accounts.[Canonical "ASSETS"].SubAccounts.ContainsKey(Canonical "BANKWEST") |> should be True
        a2.Accounts.[Canonical "ASSETS"].SubAccounts.[Canonical "BANKWEST"].SubAccounts.ContainsKey(Canonical "CHEQUE") |> should be True
        a2.Accounts.[Canonical "ASSETS"].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical "ASSETS"].SubAccounts.[Canonical "BANKWEST"].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical "ASSETS"].SubAccounts.[Canonical "BANKWEST"].SubAccounts.[Canonical "CHEQUE"].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical "ASSETS"].Postings |> should equal PersistentQueue.Empty
        a2.Accounts.[Canonical"ASSETS"].SubAccounts.[Canonical "BANKWEST"].Postings |> should equal PersistentQueue.Empty
        a2.Accounts.[Canonical"ASSETS"].SubAccounts.[Canonical "BANKWEST"].SubAccounts.[Canonical "CHEQUE"].Postings |> should equal (PersistentQueue.Empty.Enqueue detail)
    [<Test>]
    member test.``Book transaction to Accounts.``() =
        // Maybe this is also right first time.
        let t = {Transaction.date = "2013-01-01";
                 description = "I began the year with $1000 in my cheque account.";
                 // NB: Top-level account "Asset" gets created as "ASSET_S_"
                 postings = [{account = (InputName "Asset:Bankwest:Cheque");
                              amount = AUD 100000;};
                             {account = (InputName "Equity:OpeningBalances");
                              amount = AUD 100000;}];
             id=1}
        let detail0 = {posting=t.postings.[0];transaction=t}
        let detail1 = {posting=t.postings.[1];transaction=t}
        let a = Accounts()
        let a2 = a.Book(t)
        a2.Accounts.ContainsKey(Canonical "ASSETS") |> should be True
        a2.Accounts.ContainsKey(Canonical "EQUITY") |> should be True
        a2.Accounts.[Canonical "ASSETS"].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical "EQUITY"].Balance |> should equal (AUD 100000)
        a2.Accounts.[Canonical "ASSETS"].Postings |> should equal (PersistentQueue.Empty)
        a2.Accounts.[Canonical "EQUITY"].Postings |> should equal (PersistentQueue.Empty)
        a2.Accounts.[Canonical "ASSETS"].SubAccounts.[Canonical "BANKWEST"].SubAccounts.[Canonical "CHEQUE"].Postings |> should equal (PersistentQueue.Empty.Enqueue detail0)
        a2.Accounts.[Canonical "EQUITY"].SubAccounts.[Canonical "OPENINGBALANCES"].Postings |> should equal (PersistentQueue.Empty.Enqueue detail1)
