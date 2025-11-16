import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Value "../util/motoko/Value";
import Result "../util/motoko/Result";
import Error "../util/motoko/Error";
import Token "../icrc1_canister/Types";

module {
  public let AVAILABLE = "constant:available";
  public let TX_WINDOW = "constant:tx_window";
  public let PERMITTED_DRIFT = "constant:permitted_drift";
  public let FEE_COLLECTOR = "constant:fee_collector";
  public let MAX_TAKE = "constant:max_take_value";
  public let MAX_QUERY_BATCH = "constant:max_query_batch_size";

  public type ArgType = {
    #Deposit : TransferArg;
    #Withdraw : TransferArg;
  };
  public type Environment = {
    meta : Value.Metadata;
    now : Nat64;
    tx_window : Nat64;
    permitted_drift : Nat64;
  };
  type Constant = { value : Value.Type; expires_at : Nat64 };
  public type Balance = { unlocked : Nat; locked : Nat };
  public type User = {
    balances : RBTree.Type<Principal, Balance>;
    constants : RBTree.Type<Text, Constant>;
  };
  public type TransferArg = {
    token : Principal;
    amount : Nat;
    fee : ?Nat;
    created_at : ?Nat64;
  };
  public type Dedupes = RBTree.Type<(Principal, TransferArg), Nat>;
  public type DepositError = {
    #GenericError : Error.Type;
    #AmountTooLow : { minimum_amount : Nat };
    #BadFee : { expected_fee : Nat };
    #InsufficientBalance : { balance : Nat };
    #InsufficientAllowance : { allowance : Nat };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
    #Duplicate : { duplicate_of : Nat };
    #TransferFailed : Token.TransferFromError;
  };
  public type WithdrawError = {
    #GenericError : Error.Type;
    #InsufficientBalance : { balance : Nat };
    #BadFee : { expected_fee : Nat };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
    #InsufficientFunds : { balance : Nat };
    #Duplicate : { duplicate_of : Nat };
    #TransferFailed : Token.TransferError;
  };
  public type Fee = { token : Principal; amount : Nat };
  public type ReserveArg = {
    key : Text;
    value : Value.Type;
    duration : Nat64; // nano
    fee : ?Fee;
  };
  public type ReserveError = {
    #GenericError : Error.Type;

  };
  public type ExtendArg = {
    owner : Principal;
    key : Text;
    duration : Nat64; // nano
    fee : ?Fee;
  };
  public type ExtendError = {
    #GenericError : Error.Type;
  };
};
