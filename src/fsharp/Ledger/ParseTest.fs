module ParseTest

open NUnit.Framework
open FsUnit
open Parse
open Types

[<TestFixture>]
type ``Test Parsing of transaction text data`` () =
    [<Test>]
    member test.``parseTransactionData.`` () =
        let parse = (parseTransactionFile "2122-22-01 foo\n foo 10AUD\n bar 11\n baz 12\n2012-12-22 foo  sss\nacc $10\n") in do
            parse |> should equal (ParseSuccess ([Transaction {date = "2122-22-01";
                                                              description = "foo";
                                                              postings = [{account = "foo";
                                                                           amount = AUD 1000;};
                                                                          {account = "bar";
                                                                           amount = AUD 1100;};
                                                                          {account = "baz";
                                                                           amount = AUD 1200;}];};
                                                 Transaction {date = "2012-12-22";
                                                              description = "foo  sss";
                                                              postings = [{account = "acc";
                                                                           amount = AUD 1000;}];}]))
    [<Test>]
    member test.``Example from readme.md.`` () =
        let parse = (parseTransactionFile ("""2013-01-01 I began the year with $1000 in my cheque account.
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
                                              VERIFY-BALANCE 2013-02-01 Assets:Bankwest:Cheque 621.05""" + "\n"))
        let expected = (ParseSuccess [Transaction {date = "2013-01-01";
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
                                                                           amount = AUD 5801;};
                                                                          {account = "Liabilities:Bankwest:Visa";
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
                                                                     amount = AUD 62105;}]) in do
            parse |> should equal expected




(*
/////////
run pTransactionFile "#foo\n\n  #Bar\nVERIFY-BALANCE  2015-01-02  account123 $123 AUD"
run pVerifyBalance "VERIFY-BALANCE  2015-01-02  account123 $123 AUD"
run pAmount "$123"
run pAmount " $123.00"
run pAmount "$123.00"
run pOptionalSpace ""
run pMandatorySpace "1 1"

run pOptionalSpace "1"
run pMandatorySpace "1"
run pYear "1985"
run pMonth " 1985"
run pDay " 1985"
run pDate "1985-01-01"
run pCommentLine"#123\n"
run pOptionalSpace ""
run pBlankLine ""
run pCommentLine"#123\n"
run pVerifyBalance ""
run pBlankLine"\n"
run pCommentLine"#123\n"
run pItem "#123\n"
run pItem "  \n#123\n"
run pItem " A#  \n#123\n"
run pCommentLine "#FooBarBaz"
run pBlankLine  "   \n#ABC\n\n#Def"
run pItems  "   \n#ABC\n\n#Def"
run pItem  "   \n#ABC\n\n#Def"
run pOptionalSpace  "   \n#ABC\n\n#Def"


run pTransactionFile  "#ABC\n\n#Def"

run pEol "\n#FooBarBaz"
run pCommentStart "#FooBarBaz"
run (restOfLine true) "ABC\n"
// run pSymbol " foo bar baz"


parseTransactionFile "2122-22-01 foo\n foo 10AUD\n bar 11\nbaz 12\n2012-12-22 foo  sss\nacc $10\n"

run pTransaction "2122-22-01 foo\n foo 10AUD\n bar 11\nbaz 12\n2012-12-22"
run pPosting "foo -$12\n"
run pPostings "foo -$12\nbar 10\n"


let foo () =
     { Posting.account = "456";
       Posting.amount = AUD 10}



*)
