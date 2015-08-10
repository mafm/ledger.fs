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
                                  postings = [{account = "Assets:Bankwest:Cheque";
                                               amount = AUD 100000;};
                                             {account = "Equity:OpeningBalances";
                                              amount = AUD 100000;}];};
                     BlankLine;
                     Transaction {date = "2013-01-05";
                                  description = "I bought some groceries and paid using the cheque account.";
                                  postings = [{account = "Expenses:Food:Groceries";
                                               amount = AUD 9853;};
                                              {account = "Assets:Bankwest:Cheque";
                                               amount = AUD -9853;}];};
                     BlankLine;
                     Transaction {date = "2013-01-10";
                                  description = "I bought some petrol, and paid using a credit card.";
                                  postings = [{account = "Expenses:Motor:Fuel";
                                               amount = AUD 5801;}; {account = "Liabilities:Bankwest:Visa";
                                               amount = AUD 5801;}];};
                     BlankLine;
                     Transaction {date = "2013-01-15";
                                  description = "I paid my electricity bill.";
                                  postings = [{account = "Expenses:Electricity";
                                               amount = AUD 28042;};
                                              {account = "Assets:Bankwest:Cheque";
                                               amount = AUD -28042;}];};
                     BlankLine;
                     Comment " I checked my bank statement on the 1st of Feb, and this is what it said.";
                     BalanceVerfication {date = "2013-02-01";
                                         account = "Assets:Bankwest:Cheque";
                                         amount = AUD 62105;}]
        let expected = {Report.account = "Expenses";
                        from = None;
                        until = None;
                        lines = [{date = "2013-01-05";
                                  amount = AUD 9853;
                                  description = "I bought some groceries and paid using the cheque account.";
                                  account = "Expenses:Food:Groceries";
                                  balance = AUD 9853;};
                                 {date = "2013-01-10";
                                  amount = AUD 5801;
                                  description = "I bought some petrol, and paid using a credit card.";
                                  account = "Expenses:Motor:Fuel";
                                  balance = AUD 15654;};
                                 {date = "2013-01-15";
                                  amount = AUD 28042;
                                  description = "I paid my electricity bill.";
                                  account = "Expenses:Electricity";
                                  balance = AUD 43696;}];}
        (generateReport input "Expenses") |> should equal expected
