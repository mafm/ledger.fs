module RegisterReportTest

open NUnit.Framework
open FsUnit
open InputTypes
open InternalTypes
open Misc
open PersistentCollections
open ReportRegister


[<TestFixture>]
type ``Test Register Report`` () =
    [<Test>]
    member test.``Test Expenses.`` () =
        let input = [Transaction {date = "2013-01-01";
                                  description = "I began the year with $1000 in my cheque account.";
                                  postings = [{account = (InputName "Assets:Bankwest:Cheque");
                                               amount = AUD 100000;};
                                             {account = (InputName "Equity:OpeningBalances");
                                              amount = AUD 100000;}];
                                  id=1};
                     BlankLine;
                     Transaction {date = "2013-01-05";
                                  description = "I bought some groceries and paid using the cheque account.";
                                  postings = [{account = (InputName "Expenses:Food:Groceries");
                                               amount = AUD 9853;};
                                              {account = (InputName "Assets:Bankwest:Cheque");
                                               amount = AUD -9853;}];
                                  id=2};
                     BlankLine;
                     Transaction {date = "2013-01-10";
                                  description = "I bought some petrol, and paid using a credit card.";
                                  postings = [{account = (InputName "Expenses:Motor:Fuel");
                                               amount = AUD 5801;}; {account = (InputName "Liabilities:Bankwest:Visa");
                                               amount = AUD 5801;}];
                                  id=3};
                     BlankLine;
                     Transaction {date = "2013-01-15";
                                  description = "I paid my electricity bill.";
                                  postings = [{account = (InputName "Expenses:Electricity");
                                               amount = AUD 28042;};
                                              {account = (InputName "Assets:Bankwest:Cheque");
                                               amount = AUD -28042;}];
                                  id=4};
                     BlankLine;
                     Comment " I checked my bank statement on the 1st of Feb, and this is what it said.";
                     BalanceVerfication {date = "2013-02-01";
                                         account = (InputName "Assets:Bankwest:Cheque");
                                         amount = AUD 62105;}]
        let expected = {Report.account = (InputName "Expenses");
                        from = None;
                        until = None;
                        lines = [{date = "2013-01-05";
                                  amount = AUD 9853;
                                  description = "I bought some groceries and paid using the cheque account.";
                                  account = (InputName "Expenses:Food:Groceries");
                                  balance = AUD 9853;};
                                 {date = "2013-01-10";
                                  amount = AUD 5801;
                                  description = "I bought some petrol, and paid using a credit card.";
                                  account = (InputName "Expenses:Motor:Fuel");
                                  balance = AUD 15654;};
                                 {date = "2013-01-15";
                                  amount = AUD 28042;
                                  description = "I paid my electricity bill.";
                                  account = (InputName "Expenses:Electricity");
                                  balance = AUD 43696;}];}
        (generateReport input (InputName "Expenses")) |> should equal expected
