import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Value "../util/motoko/Value";
import Result "../util/motoko/Result";
import Error "../util/motoko/Error";
import Token "../icrc1_canister/Types";
import CMC "../util/motoko/CMC/types";
import ICRC1 "../icrc1_canister/main";

module {
  public let AVAILABLE = "echo:available";
  public let TX_WINDOW = "echo:tx_window";
  public let PERMITTED_DRIFT = "echo:permitted_drift";
  public let FEE_COLLECTOR = "echo:fee_collector";
  public let MAX_TAKE = "echo:max_take_value";
  public let MAX_UPDATE_BATCH = "echo:max_update_batch_size";
  public let MAX_QUERY_BATCH = "echo:max_query_batch_size";
  public let MIN_DURATION = "echo:min_duration";
  public let WITHDRAWAL_FEE_MULTIPLIER = "echo:withdrawal_fee_multiplier";
  // public let OWNER_FEE_PCT = "echo:owner_fee_percent";
  public let PREMIUM_PCT = "echo:premium_percent";

  public type ArgType = {
    #Deposit : TransferArg;
    #Withdraw : TransferArg;
    #ConstantReserve : ConstantReserveArg;
    #ConstantExtend : ConstantExtendArg;
    // #VariableReserve : VariableReserveArg;
    // #VariableExtend : VariableExtendArg;
    // #VariableUpdate : VariableUpdateArg;
    // #VariableSponsor : VariableSponsorArg;
  };
  public type Environment = {
    now : Nat64;
    tx_window : Nat64;
    permitted_drift : Nat64;
    cmc_p : Text;
    icp_p : Text;
    tcycles_p : Text;
    withdrawal_fee_multiplier : Nat;
    min_duration : Nat64;
    max_update_batch : Nat;
    fee_collector : Principal;
    // owner_fee_pct : Nat;
    premium_pct : Nat;
  };
  public type Constant = {
    owner : Principal;
    sub : Blob;
    about : Text;
    value : Value.Type;
    expires_at : Nat64;
    created_at : Nat64;
  };
  public type Variable = {
    owner : Principal;
    sub : Blob;
    about : Text;
    value : Value.Type;
    expires_at : Nat64;
    updated_at : Nat64;
    update_credits : Nat;
    created_at : Nat64;
  };
  public type Balance = { unlocked : Nat; locked : Nat };
  public type Subacc = {
    // last_active : Nat64;
    balances : RBTree.Type<Principal, Balance>;
    constants : RBTree.Type<Nat, ()>;
    variables : RBTree.Type<Nat, ()>;
  };
  public type User = RBTree.Type<Blob, Subacc>;
  public type TransferArg = {
    subaccount : ?Blob;
    token : Principal;
    amount : Nat;
    fee : ?Nat;
    created_at : ?Nat64;
  };
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
    #Duplicate : { duplicate_of : Nat };
    #TransferFailed : Token.TransferError;
  };
  public type Fee = { token : Principal; amount : Nat };
  public type ConstantReserveArg = {
    subaccount : ?Blob;
    about : Text;
    value : Value.Type;
    duration : Nat64; // nano
    fee : ?Fee;
    created_at : ?Nat64;
  };
  public type ConstantReserveErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
    #DurationTooShort : { minimum_duration : Nat64 };
    #BadFee : { expected_fee : Nat };
    #InsufficientBalance : { balance : Nat; amount_required : Nat };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
    #Duplicate : { duplicate_of : Nat };
  };
  public type ConstantExtendArg = {
    subaccount : ?Blob;
    id : Nat;
    duration : Nat64; // nano
    fee : ?Fee;
    created_at : ?Nat64;
  };
  public type ConstantExtendErr = {
    #GenericError : Error.Type;
    #GenericBatchError : Error.Type;
    #DurationTooShort : { minimum_duration : Nat64 };
    #NotFound;
    #BadFee : { expected_fee : Nat };
    #InsufficientBalance : { balance : Nat; amount_required : Nat };
    #CreatedInFuture : { ledger_time : Nat64 };
    #TooOld;
    #Duplicate : { duplicate_of : Nat };
  };
  public type Nats = RBTree.Type<Nat, ()>;
  public type Expiries = RBTree.Type<Nat64, (orders : Nats)>;
  public type Accounts = RBTree.Type<Principal, RBTree.Type<Blob, ()>>;
  public type UserExpiries = RBTree.Type<Nat64, Accounts>;
  // public type VariableReserveArg = {

  // };
  // public type VariableReserveErr = {
  //   #GenericError : Error.Type;
  //   #GenericBatchError : Error.Type;

  // };
  // public type VariableExtendArg = {

  // };
  // public type VariableExtendErr = {
  //   #GenericError : Error.Type;
  //   #GenericBatchError : Error.Type;

  // };
  // public type VariableUpdateArg = {

  // };
  // public type VariableUpdateErr = {
  //   #GenericError : Error.Type;
  //   #GenericBatchError : Error.Type;

  // };
  // public type VariableSponsorArg = {

  // };
  // public type VariableSponsorErr = {
  //   #GenericError : Error.Type;
  //   #GenericBatchError : Error.Type;

  // };
};
