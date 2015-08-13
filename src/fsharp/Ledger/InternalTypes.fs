module InternalTypes

open InputTypes
open PersistentCollections

exception EmptyAccountNameComponents

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

/// Do we have a valid account name?
let validAccountName (a:AccountName) =
    try
        match (accountType a) with _ -> true
    with BadAccountName(name, problem) -> false

let canonicalRootName name =
    let accountType = accountType name
    match accountType with
    | Asset -> "ASSETS"
    | Liability -> "LIABILITY"
    | Income -> "INCOME"
    | Expense -> "EXPENSE"
    | Equity -> "EQUITY"



type AccountNameComponent = {
    canonical: string;
    input: string}

type AccountNameComponents = AccountNameComponent list

/// Break AccountName into ordered list of components.
/// For each level of the account, we produce canonical & input components.
/// Checkout out unit test for an example of what this does.
let splitAccountName (name: AccountName) =
        let components = name.Split(':') |> Array.toList
        let rec helper (components: string list) =
            match components with
            | [] -> []
            | first::rest -> {canonical = first.ToUpper(); input = first} :: (helper rest)
        match components with
            | root::rest -> {canonical = (canonicalRootName name); input = root} :: (helper rest)
            | [] -> raise (BadAccountName(name, "Empty name"))
/// Canonical parts of splitAccountName
let canonicalAccountName (name: AccountName) =
    (List.map (fun x -> x.canonical) (splitAccountName name))

/// When we record a posting, we will often want the transaction it was part of for later reports.
type PostingDetail = { posting: Posting
                       transaction: Transaction}

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
      ///
      /// XXX: The name field is redundant - we can easily get it from fullName.
      ///      Is it actually useful?
      val name: string
      // How do balances in this account count against balances in other accounts?
      val sign: int
      /// Balances of sub-accounts contribute to balance of containing account.
      val subAccounts: PersistentDictionary<string, Account>
      /// Amounts in postings contribute to balance of account they are posted to.
      val postings: PersistentQueue<PostingDetail>
      /// Balance is the sum of direct postings and balances of sub-accounts.
      val balance: Amount
      private new (fullName: string,
                   name: string,
                   sign: int,
                   subAccounts: PersistentDictionary<string, Account>,
                   postings: PersistentQueue<PostingDetail>,
                   balance: Amount) = {fullName = fullName;
                                       name = name;
                                       sign = sign;
                                       subAccounts = subAccounts;
                                       postings = postings;
                                       balance = balance}
      new (fullName: string) =
        let name = match (fullName.Split(':') |> Array.toList |> List.rev) with
                    | name :: _ -> name
                    | [] -> raise (BadAccountName(fullName, "Empty name"))
        let sign = sign (accountType fullName)
        new Account(fullName, name, sign,
                    PersistentDictionary.Empty,
                    PersistentQueue.Empty,
                    AUD 0)
      /// Add posting to this.postings,
      /// add posting.amount to this.balance, and
      /// book posting to relevant sub-account.
      member this.Book ((p: Posting), (t: Transaction), (subAccountDetails: AccountNameComponent list)) =
        new Account(this.fullName,
                    this.name,
                    this.sign,
                    (match subAccountDetails with
                            | [] -> this.subAccounts
                            | subAccountName::subSubAccountDetails ->
                                    let subAccount = if this.subAccounts.ContainsKey(subAccountName.canonical) then
                                                        this.subAccounts.[subAccountName.canonical]
                                                     else
                                                        new Account(this.fullName + ":" + subAccountName.input,
                                                                    subAccountName.input,
                                                                    this.sign,
                                                                    PersistentDictionary.Empty,
                                                                    PersistentQueue.Empty,
                                                                    AUD 0)
                                    let subAccount = subAccount.Book(p, t, subSubAccountDetails)
                                    this.subAccounts.Add(subAccountName.canonical, subAccount)),

                    this.postings.Enqueue({posting=p; transaction=t}), /// XXX: Will change this to book only to relevant subaccount, not to both parent and subaccount.
                    (addAmounts this.balance p.amount))
      member this.find (accountDetails: AccountNameComponent list) : Account option =
        match accountDetails with
            | []  -> Some this
            | subAccountName::subSubAccountDetails ->
                if this.subAccounts.ContainsKey(subAccountName.canonical) then
                    (this.subAccounts.[subAccountName.canonical].find subSubAccountDetails)
                else
                    None
    end

/// A "set" of accounts - ie
/// - a tree structured collection of accounts and their sub-accounts
///
/// Each node contains:
/// - the preferred spelling of the account's name
/// - the set of postings affecting the account, and
/// - the account's balance
///
/// In the python code, this was called account-tree or something like that.
///
/// Perhaps when we create an empty Accounts object, it probably should
/// contain a top-level account for each of the five basic account types.
/// On the other hand, if a set of transactions doesn't use one of the basic
/// account types, why do we need to create it when we process those transactions?
/// When generating reports, we should should be able to cope with some of the
/// basic account types being absent - it's not really a big issue...

type Accounts private (accounts: PersistentDictionary<string, Account>) =
    let accounts = accounts
    member this.Accounts = accounts
    /// Book postings to relevant account.
    member this.Book ((p: Posting), (t: Transaction)) =
        let accountDetails = (splitAccountName p.account)
        match accountDetails with
            | []  -> raise (BadAccountName(p.account, "Empty name"))
            | accountName::subAccountDetails ->
                            let account = if accounts.ContainsKey(accountName.canonical) then
                                            accounts.[accountName.canonical]
                                          else
                                            new Account(accountName.input)
                            new Accounts(accounts.Add(accountName.canonical, account.Book(p, t, subAccountDetails)))
    /// Book all postings in transaction.
    member this.Book (t: Transaction) =
        (List.fold (fun (accounts : Accounts) (p: Posting) -> accounts.Book(p, t))
                   this
                   t.postings)
    /// Book a bunch of transactions to a pre-existing Accounts object
    member this.Book (transactions: Transaction list) =
        let rec helper (accounts:Accounts) (transactions: Transaction list)  =
            match transactions with
            | [] -> accounts
            | t::transactions -> (helper (accounts.Book t) transactions)
        (helper this transactions)
    // Given an account name, find the account
    member this.find (name: AccountNameComponents) =
        match name with
            | []  -> raise EmptyAccountNameComponents
            | accountName::subAccountDetails ->
                if accounts.ContainsKey(accountName.canonical) then
                    (accounts.[accountName.canonical].find subAccountDetails)
                else
                    None
    // Given an account name, find the account
    member this.find (account: AccountName) =
        (this.find (splitAccountName account))
    // NB: primary constructor is private. The public constructors are below.
    new () = Accounts(PersistentDictionary.Empty)
    new (transactions: Transaction list) =
        Accounts(((new Accounts()).Book transactions).Accounts)
