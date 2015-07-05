module InternalTypesTest

open NUnit.Framework
open FsUnit
open Parse
open InternalTypes

[<TestFixture>]
type ``Test Internal Types`` () =
    [<Test>]
    member test.``splitAccountName.`` () =
        splitAccountName "Expenses:BankFees:AccountServiceFee" 
        |> should equal [{canonical = "EXPENSE"; input = "Expenses";};
                         {canonical = "BANKFEES"; input = "BankFees";};
                         {canonical = "ACCOUNTSERVICEFEE"; input = "AccountServiceFee";}]
        