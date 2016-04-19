module InternalTypes

open InputTypes
open PersistentCollections

exception EmptyAccountNameComponentsException

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
let accountType (InputName name) =
    let components = name.ToUpper().Split(':') |> Array.toList
    let root = match components with
                    | root::tail -> root
                    | _ -> raise (BadAccountNameException((InputName name), "Empty name"))
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
    | _             -> raise (BadAccountNameException((InputName name), "Unable to determine account type"))

/// Do we have a valid account name?
let validAccountName (a:InputNameAccount) =
    try
        match (accountType a) with _ -> true
    with BadAccountNameException(name, problem) -> false

type CanonicalNameComponent = Canonical of string
type InputNameComponent = Input of string
    with
        member this.AsInputName =
            match this with (Input str) -> (InputName str)

type AccountNameComponent = {
    Canonical: CanonicalNameComponent;
    Input: InputNameComponent}

type InternalNameAccount = AccountNameComponent list

let toInputName (name: InternalNameAccount) : InputNameAccount =
    let rec helper (name: InternalNameAccount) =
        match name with
        | [] -> 
            ""
        | only::[] -> match (only.Input) with
                            (Input only) -> only
        | first::rest -> match (first.Input) with
                            (Input first) -> first + ":"+(helper rest)
    (InputName (helper name))

let canonicalRootName name =
    let accountType = accountType name
    (Canonical (match accountType with
                | Asset -> "ASSETS"
                | Liability -> "LIABILITY"
                | Income -> "INCOME"
                | Expense -> "EXPENSE"
                | Equity -> "EQUITY"))

/// Break AccountName into ordered list of components.
/// For each level of the account, we produce canonical & input components.
/// Checkout out unit test for an example of what this does.
let splitAccountName (InputName name) : InternalNameAccount =
        let components = name.Split(':') |> Array.toList
        let rec helper (components: string list) =
            match components with
            | [] -> []
            | first::rest -> {Canonical = (Canonical (first.ToUpper())); Input = (Input first)} :: (helper rest)
        match components with
            | root::rest -> {Canonical = (canonicalRootName (InputName name)); Input = (Input root)} :: (helper rest)
            | [] -> raise (BadAccountNameException((InputName name), "Empty name"))


type InputNameAccount 
    with
        member this.LastName =
            let name = (splitAccountName this)
            let reversed = List.rev name
            match reversed with
            | last :: _ -> last
            | [] -> raise (BadAccountNameException(this, "Empty account name"))

let joinInputNames (InputName parentName) (Input childName) =
    (InputName (parentName + ":" + childName))


/// Canonical parts of splitAccountName
let canonicalAccountName (name: InputNameAccount) =
    (List.map (fun x -> x.Canonical) (splitAccountName name))

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
      val FullInputName: InputNameAccount

      /// The short (sub-account) name of this account as presented to user.
      ///
      /// For example, an Account with fullName "Expenses:BankFees:AccountServiceFee",
      /// will have name "AccountServiceFee".
      ///
      /// XXX: The name field is redundant - we can easily get it from fullName.
      ///      Is it actually useful?
      val LastName: AccountNameComponent

      // How do balances in this account count against balances in other accounts?
      val Sign: int

      /// Balances of sub-accounts contribute to balance of containing account.
      val SubAccounts: PersistentDictionary<CanonicalNameComponent, Account>

      /// Amounts in postings contribute to balance of account they are posted to.
      val Postings: PersistentQueue<PostingDetail>

      /// Balance is the sum of direct postings and balances of sub-accounts.
      val Balance: Amount

      private new (fullName:  InputNameAccount,
                   sign: int,
                   subAccounts: PersistentDictionary<CanonicalNameComponent, Account>,
                   postings: PersistentQueue<PostingDetail>,
                   balance: Amount) =
        { FullInputName = fullName;
          LastName = fullName.LastName;
          Sign = sign;
          SubAccounts = subAccounts;
          Postings = postings;
          Balance = balance}
      new (fullName: InputNameAccount) =
        let sign = sign (accountType fullName)
        new Account(fullName,
                    sign,
                    PersistentDictionary.Empty,
                    PersistentQueue.Empty,
                    AUD 0)
      /// Add posting to this.postings,
      /// add posting.amount to this.balance, and
      /// book posting to relevant sub-account.

      member this.HasChild(name : CanonicalNameComponent) = this.SubAccounts.ContainsKey(name)

      member this.GetChild(name : CanonicalNameComponent) = this.SubAccounts.[name]

      member this.AddSubAccount ((name : CanonicalNameComponent), (a: Account)) =
        this.SubAccounts.Add(name, a)

      member this.Book ((p: Posting), (t: Transaction), (subAccountDetails: AccountNameComponent list)) =
        new Account(this.FullInputName,
                    this.Sign,
                    (match subAccountDetails with
                            | [] -> this.SubAccounts
                            | subAccountName::subSubAccountDetails ->
                                    let subAccount = if this.HasChild(subAccountName.Canonical) then
                                                        this.GetChild(subAccountName.Canonical)
                                                     else
                                                        new Account((joinInputNames this.FullInputName subAccountName.Input),                                                                    
                                                                    this.Sign,
                                                                    PersistentDictionary.Empty,
                                                                    PersistentQueue.Empty,
                                                                    AUD 0)
                                    let subAccount = subAccount.Book(p, t, subSubAccountDetails)
                                    this.AddSubAccount(subAccountName.Canonical, subAccount)),
                    (match subAccountDetails with
                            | [] -> this.Postings.Enqueue({posting=p; transaction=t})
                            | _ ->  this.Postings),
                    (addAmounts this.Balance p.amount))
      member this.find (accountDetails: AccountNameComponent list) : Account option =
        match accountDetails with
            | []  -> Some this
            | subAccountName::subSubAccountDetails ->
                if this.HasChild(subAccountName.Canonical) then
                    (this.GetChild(subAccountName.Canonical).find subSubAccountDetails)
                else
                    None
      member this.SubAccountsOrderedByInputName =
        let namesAndAccounts = (this.SubAccounts |> Seq.sortBy (fun (KeyValue(_,account)) -> account.LastName.Input))
        [for KeyValue(_, account) in namesAndAccounts -> account]        
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

type Accounts private (accounts: PersistentDictionary<CanonicalNameComponent, Account>) =
    let accounts = accounts
    member this.HasChild(name : CanonicalNameComponent) = accounts.ContainsKey(name)
    member this.GetChild(name : CanonicalNameComponent) = accounts.[name]
    member this.AddSubAccount ((name : CanonicalNameComponent), (a: Account)) = accounts.Add(name, a)
    member this.Accounts = accounts
    /// Book postings to relevant account.
    member this.Book ((p: Posting), (t: Transaction)) =
        let accountDetails = (splitAccountName p.account)
        match accountDetails with
            | []  -> raise (BadAccountNameException(p.account, "Empty name"))
            | accountName::subAccountDetails ->
                            let account = if this.HasChild(accountName.Canonical) then
                                            this.GetChild(accountName.Canonical)
                                          else
                                            new Account((toInputName [accountName]))
                            new Accounts(this.AddSubAccount(accountName.Canonical, account.Book(p, t, subAccountDetails)))
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
    member this.find (name: InternalNameAccount) =
        match name with
            | []  -> raise EmptyAccountNameComponentsException
            | accountName::subAccountName ->
                if this.HasChild(accountName.Canonical) then
                    (this.GetChild(accountName.Canonical).find subAccountName)
                else
                    None
    // Given an account name, find the account
    member this.find (account: InputNameAccount) =
        (this.find (splitAccountName account))
    // NB: primary constructor is private. The public constructors are below.
    new () = Accounts(PersistentDictionary.Empty)
    new (transactions: Transaction list) =
        Accounts(((new Accounts()).Book transactions).Accounts)
