import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Value "../util/motoko/Value";
import Result "../util/motoko/Result";
import Error "../util/motoko/Error";
import Token "../icrc1_canister/Types";

module {
  public let AVAILABLE = "kv:available";
  public let TX_WINDOW = "vault:tx_window";
  public let PERMITTED_DRIFT = "vault:permitted_drift";
  public let FEE_COLLECTOR = "kv:fee_collector";
  public let MAX_TAKE = "kv:max_take_value";
  public let MAX_QUERY_BATCH = "kv:max_query_batch_size";
  public let MIN_DURATION = "kv:min_duration";

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
  public type Constant = {
    description : Text;
    value : Value.Type;
    expires_at : Nat64;
    created_at : Nat64;
  };
  public type Variable = {
    description : Text;
    value : Value.Type;
    expires_at : Nat64;
    updated_at : Nat64;
    update_credits : Nat;
    created_at : Nat64;
  };
  public type Balance = { unlocked : Nat; locked : Nat };
  public type User = {
    balances : RBTree.Type<Principal, Balance>;
    constants : RBTree.Type<Nat, ()>;
    variables : RBTree.Type<Nat, ()>;
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
  public type ConstantReserveArg = {
    description : Text;
    value : Value.Type;
    duration : Nat64; // nano
    fee : ?Fee;
  };
  public type ConstantReserveErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
    #DurationTooShort : { minimum_duration : Nat64 };
  };
  public type ConstantExtendArg = {
    id : Nat;
    duration : Nat64; // nano
    fee : ?Fee;
  };
  public type ConstantExtendErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
  };

  public type VariableReserveErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;

  };
  public type VariableExtendErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;

  };
  public type VariableUpdateErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;

  };
  public type VariableSponsorErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;

  };
};
