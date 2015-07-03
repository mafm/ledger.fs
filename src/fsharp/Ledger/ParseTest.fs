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
