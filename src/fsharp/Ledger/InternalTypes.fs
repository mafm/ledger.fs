module InternalTypes

open InputTypes
open PersistentCollections

type AccountType =
    | Asset
    | Liability
    | Income
    | Expense
    | Equity

/// An increase/decrease to an account type with +ve sign is
/// balanced by an increase/decrease to an account with -ve sign.
let sign (accountType: AccountType) =
    match accountType with 
    | Asset -> 1
    | Liability -> -1
    | Income -> -1
    | Expense -> 1
    | Equity -> -1

/// Which of the five basic account types is this?
let accountType (name : AccountName) =
    let components = name.ToUpper().Split(':') |> Array.toList
    let root = match components with
                    | root::tail -> root
                    | _ -> raise (BadAccountName(name, "Empty name"))
    match root with 
    | "ASSET"       -> Asset
    | "ASSETS"      -> Asset
    | "LIABILITY"   -> Liability
    | "LIABILITIES" -> Liability
    | "REVENUE"     -> Income
    | "REVENUES"    -> Income
    | "INCOME"      -> Income
    | "EXPENSE"     -> Expense
    | "EXPENSES"    -> Expense
    | "EQUITY"      -> Equity
    | _             -> raise (BadAccountName(name, "Unable to determine account type"))

let canonicalRootName name =
    let accountType = accountType name
    match accountType with
    | Asset -> "ASSET"
    | Liability -> "LIABILITY"
    | Income -> "INCOME"
    | Expense -> "EXPENSE"
    | Equity -> "EQUITY"

/// An account contains:
/// - a balance
/// - sub-accounts
/// - postings
/// The balance of an account is the sum of the postings and the sum of the balances
/// of its sub-accounts.
type Account = struct
      /// The full name of this account as shown to user in reports.
      ///
      /// For example, an Account with fullName "Expenses:BankFees:AccountServiceFee",
      /// will have fullName "Expenses:BankFees:AccountServiceFee".
      ///
      /// This might vary depending on details of the input file.
      /// Several account names may be by mapped to a single
      /// canonical name. For example, accounts named "Expense:BankFees"
      /// and "EXPENSES:BANKFEES" will get mapped to a single account.
      ///
      /// When generating reports, we aim to use the first spelling of
      /// the name seen in the input file.            
      val fullName: string
      /// The short (sub-account) name of this account as presented to user.
      ///
      /// For example, an Account with fullName "Expenses:BankFees:AccountServiceFee",
      /// will have name "AccountServiceFee".
      val name: string
      // How do balances in this account count against balances in other accounts?
      val sign: int
      /// Balances of sub-accounts contribute to balance of containing account.
      val subAccounts: PersistentDictionary<string, Account>
      /// Amounts in postings contribute to balance of account they are posted to.
      val postings: PersistentQueue<Posting>
      /// Balance is the sum of direct postings and balances of sub-accounts.
      val balance: Amount
      private new (fullName: string,
                   name: string,
                   sign: int,
                   subAccounts: PersistentDictionary<string, Account>,
                   postings: PersistentQueue<Posting>,
                   balance: Amount) = {fullName = fullName;
                                       name = name;
                                       sign = sign;
                                       subAccounts = subAccounts;
                                       postings = postings;
                                       balance = balance}
      new (fullName: string) =
        let name = match (fullName.ToUpper().Split(':') |> Array.toList |> List.rev) with
                | name :: _ -> name
                | [] -> raise (BadAccountName(fullName, "Empty name"))
        let sign = sign (accountType fullName)
        new Account(fullName, name, sign,
                    PersistentDictionary<string, Account>.Empty,
                    PersistentQueue<Posting>.Empty,
                    AUD 0)
      member this.Foo() =
            match this.balance with
                AUD x -> x+10
    end
        
/// A set of accounts - ie
/// - a tree structured set of accounts and their sub-accounts
/// each node contains:
/// - the preferred spelling of the account's name
/// - the set of postings affecting the account, and
/// - the account's balance
/// (In the python code, this was called account-tree or something like that.)
type Accounts () =
    let value = PersistentDictionary<string, Account>.Empty                    

