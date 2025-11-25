import Result "../util/motoko/Result";
import Error "../util/motoko/Error";
import Text "mo:base/Text";
import Char "mo:base/Char";
import KVT "types";
import Value "../util/motoko/Value";
import Time64 "../util/motoko/Time64";
import Nat64 "mo:base/Nat64";
import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import Principal "mo:base/Principal";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Option "../util/motoko/Option";
import Subaccount "../util/motoko/Subaccount";

module {
  public func newConstant(owner : Principal, sub : Blob, arg : KVT.ConstantReserveArg, now : Nat64) : KVT.Constant = ({
    arg with expires_at = now + arg.duration;
    created_at = now;
    owner;
    sub;
  });

  func missingMeta(k : Text) : Text = "Metadata `" # k # "` is missing";
  public func getEnvironment(meta : Value.Metadata) : Result.Type<KVT.Environment, Error.Generic> {
    let tx_window = switch (Value.metaNat(meta, KVT.TX_WINDOW)) {
      case (?found) Time64.SECONDS(Nat64.fromNat(found));
      case _ return Error.text(missingMeta(KVT.TX_WINDOW));
    };
    let permitted_drift = switch (Value.metaNat(meta, KVT.PERMITTED_DRIFT)) {
      case (?found) Time64.SECONDS(Nat64.fromNat(found));
      case _ return Error.text(missingMeta(KVT.PERMITTED_DRIFT));
    };
    let withdrawal_fee_multiplier = switch (Value.metaNat(meta, KVT.WITHDRAWAL_FEE_MULTIPLIER)) {
      case (?found) found;
      case _ return Error.text(missingMeta(KVT.WITHDRAWAL_FEE_MULTIPLIER));
    };
    let min_duration = switch (Value.metaNat(meta, KVT.MIN_DURATION)) {
      case (?found) Time64.SECONDS(Nat64.fromNat(found));
      case _ return Error.text(missingMeta(KVT.MIN_DURATION));
    };
    let max_update_batch = switch (Value.metaNat(meta, KVT.MAX_UPDATE_BATCH)) {
      case (?found) found;
      case _ return Error.text(missingMeta(KVT.MAX_UPDATE_BATCH));
    };
    let fee_collector = switch (Value.metaPrincipal(meta, KVT.FEE_COLLECTOR)) {
      case (?found) found;
      case _ return Error.text(missingMeta(KVT.FEE_COLLECTOR));
    };
    // let owner_fee_pct = switch (Value.metaNat(meta, KVT.OWNER_FEE_PCT)) {
    //   case (?found) found;
    //   case _ return Error.text(missingMeta(KVT.OWNER_FEE_PCT));
    // };
    let premium_pct = switch (Value.metaNat(meta, KVT.PREMIUM_PCT)) {
      case (?found) found;
      case _ return Error.text(missingMeta(KVT.PREMIUM_PCT));
    };
    #Ok {
      now = Time64.nanos();
      tx_window;
      permitted_drift;
      cmc_p = "rkp4c-7iaaa-aaaaa-aaaca-cai";
      icp_p = "ryjl3-tyaaa-aaaaa-aaaba-cai";
      tcycles_p = "um5iw-rqaaa-aaaaq-qaaba-cai";
      withdrawal_fee_multiplier;
      min_duration;
      max_update_batch;
      fee_collector;
      // owner_fee_pct;
      premium_pct;
    };
  };
  public func getSubacc(u : KVT.User, sub : Blob) : KVT.Subacc = switch (RBTree.get(u, Blob.compare, sub)) {
    case (?found) found;
    case _ ({
      balances = RBTree.empty();
      constants = RBTree.empty();
      variables = RBTree.empty();
    });
  };
  public func saveSubacc(u : KVT.User, sub : Blob, s : KVT.Subacc) : KVT.User = if (RBTree.size(s.balances) > 0 or RBTree.size(s.constants) > 0 or RBTree.size(s.variables) > 0) RBTree.insert(u, Blob.compare, sub, s) else RBTree.delete(u, Blob.compare, sub);

  public func insertConstant(s : KVT.Subacc, id : Nat) : KVT.Subacc = {
    s with constants = RBTree.insert(s.constants, Nat.compare, id, ())
  };
  public func deleteConstant(s : KVT.Subacc, id : Nat) : KVT.Subacc = {
    s with constants = RBTree.delete(s.constants, Nat.compare, id)
  };

  public func getBalance(s : KVT.Subacc, token : Principal) : KVT.Balance = switch (RBTree.get(s.balances, Principal.compare, token)) {
    case (?found) found;
    case _ ({ unlocked = 0; locked = 0 });
  };
  public func saveBalance(s : KVT.Subacc, token : Principal, b : KVT.Balance) : KVT.Subacc = ({
    s with balances = if (b.unlocked > 0 or b.locked > 0) RBTree.insert(s.balances, Principal.compare, token, b) else RBTree.delete(s.balances, Principal.compare, token);
  });

  public func dedupe((ap : Principal, a : KVT.TransferArg), (bp : Principal, b : KVT.TransferArg)) : Order.Order {
    switch (Option.compare(a.created_at, b.created_at, Nat64.compare)) {
      case (#equal) ();
      case other return other;
    };
    switch (Principal.compare(ap, bp)) {
      case (#equal) ();
      case other return other;
    };
    switch (Principal.compare(a.token, b.token)) {
      case (#equal) ();
      case other return other;
    };
    switch (Option.compare(a.fee, b.fee, Nat.compare)) {
      case (#equal) ();
      case other return other;
    };
    Nat.compare(a.amount, b.amount);
  };
  public func valueTransfer(op : Text, owner : Principal, sub : Blob, fee : Nat, arg : KVT.TransferArg, xfer : Nat, now : Nat64, phash : ?Blob) : Value.Type {
    var tx = RBTree.empty<Text, Value.Type>();
    tx := Value.setAccountP(tx, "acct", ?{ owner; subaccount = Subaccount.opt(sub) });
    tx := Value.setPrincipal(tx, "token", ?arg.token);
    tx := Value.setNat(tx, "amt", ?arg.amount);
    switch (arg.created_at) {
      case (?t) tx := Value.setNat(tx, "ts", ?Nat64.toNat(t));
      case _ ();
    };
    tx := Value.setNat(tx, "xfer", ?xfer);
    var map = RBTree.empty<Text, Value.Type>();
    switch (arg.fee) {
      case (?defined) if (defined > 0) tx := Value.setNat(tx, "fee", arg.fee);
      case _ if (fee > 0) map := Value.setNat(map, "fee", ?fee);
    };
    map := Value.setNat(map, "ts", ?Nat64.toNat(now));
    map := Value.setText(map, "op", ?op);
    map := Value.setMap(map, "tx", tx);
    map := Value.setBlob(map, "phash", phash);
    #Map(RBTree.array(map));
  };
  public func valueConstantReserve(owner : Principal, sub : Blob, about_len : Nat, value_len : Nat, fee : KVT.Fee, arg : KVT.ConstantReserveArg, now : Nat64, phash : ?Blob) : Value.Type {
    var tx = RBTree.empty<Text, Value.Type>();
    tx := Value.setAccountP(tx, "acct", ?{ owner; subaccount = Subaccount.opt(sub) });
    tx := Value.setNat(tx, "about", ?about_len);
    tx := Value.setNat(tx, "value", ?value_len);
    tx := Value.setNat(tx, "duration", ?Nat64.toNat(arg.duration));
    switch (arg.created_at) {
      case (?t) tx := Value.setNat(tx, "ts", ?Nat64.toNat(t));
      case _ ();
    };
    var fee_map = RBTree.empty<Text, Value.Type>();
    fee_map := Value.setPrincipal(fee_map, "token", ?fee.token);
    fee_map := Value.setNat(fee_map, "amt", ?fee.amount);
    var map = RBTree.empty<Text, Value.Type>();
    switch (arg.fee) {
      case (?defined) tx := Value.setMap(tx, "fee", fee_map);
      case _ map := Value.setMap(map, "fee", fee_map);
    };
    map := Value.setNat(map, "ts", ?Nat64.toNat(now));
    map := Value.setText(map, "op", ?"constant_reserve");
    map := Value.setMap(map, "tx", tx);
    map := Value.setBlob(map, "phash", phash);
    #Map(RBTree.array(map));
  };
  public func valueConstantExtend(owner : Principal, sub : Blob, fee : KVT.Fee, arg : KVT.ConstantExtendArg, now : Nat64, phash : ?Blob) : Value.Type {
    var tx = RBTree.empty<Text, Value.Type>();
    tx := Value.setAccountP(tx, "acct", ?{ owner; subaccount = Subaccount.opt(sub) });
    tx := Value.setNat(tx, "id", ?arg.id);
    tx := Value.setNat(tx, "duration", ?Nat64.toNat(arg.duration));
    switch (arg.created_at) {
      case (?t) tx := Value.setNat(tx, "ts", ?Nat64.toNat(t));
      case _ ();
    };
    var fee_map = RBTree.empty<Text, Value.Type>();
    fee_map := Value.setPrincipal(fee_map, "token", ?fee.token);
    fee_map := Value.setNat(fee_map, "amt", ?fee.amount);
    var map = RBTree.empty<Text, Value.Type>();
    switch (arg.fee) {
      case (?defined) tx := Value.setMap(tx, "fee", fee_map);
      case _ map := Value.setMap(map, "fee", fee_map);
    };
    map := Value.setNat(map, "ts", ?Nat64.toNat(now));
    map := Value.setText(map, "op", ?"constant_extend");
    map := Value.setMap(map, "tx", tx);
    map := Value.setBlob(map, "phash", phash);
    #Map(RBTree.array(map));
  };

  public func incLock(b : KVT.Balance, amt : Nat) : KVT.Balance = {
    b with locked = b.locked + amt
  };
  public func decLock(b : KVT.Balance, amt : Nat) : KVT.Balance = {
    b with locked = if (b.locked > amt) b.locked - amt else 0
  };
  public func incUnlock(b : KVT.Balance, amt : Nat) : KVT.Balance = {
    b with unlocked = b.unlocked + amt
  };
  public func decUnlock(b : KVT.Balance, amt : Nat) : KVT.Balance = {
    b with unlocked = if (b.unlocked > amt) b.unlocked - amt else 0;
  };

  public func constBytes(c : KVT.Constant) : {
    owner : Nat;
    sub : Nat;
    about : Nat;
    value : Nat;
    expires_at : Nat;
    created_at : Nat;
  } = ({
    owner = Principal.toBlob(c.owner).size();
    sub = c.sub.size();
    about = Text.encodeUtf8(c.about).size();
    value = (to_candid (c.value)).size();
    expires_at = 8;
    created_at = 8;
  });

  type Fees = { cycles : Nat; icp : Nat };
  public func calculateFees(size : Nat, duration : Nat64, xdr_permyriad_per_icp : Nat, premium_pct : Nat) : Fees {
    let duration_sec = Nat64.toNat(duration / Time64.SECONDS(1));
    var cycles = (127_000 * size * duration_sec) / 1_073_741_824; // 1 GiB/s = 127k cycles
    cycles += 1_200_000; // cycles per update message received (sub -> canister)
    cycles += 5_000_000; // 5,000,000 cycles per update message
    // ingress byte reception is sponsored (2000 cycles per byte received)
    // wasm execution is sponsored (1 cycle per instruction)
    // intercanister call is sponsored (260k cycles base, 1k per byte outgoing) + reply byte transmision

    cycles += premium_pct * cycles / 100;

    // icp = (cycles * icp_decimals * per_myriad) / (1T per XDR * xdr_permyriad_per_icp)
    let icp = (cycles * 100_000_000 * 10_000) / (1_000_000_000_000 * xdr_permyriad_per_icp);
    { cycles; icp };
  };

  type ChargeOk = { token : Principal; balance : KVT.Balance; amount : Nat };
  public func checkFee(arg_fee : ?KVT.Fee, fee_amt : Fees, env : KVT.Environment, sub : KVT.Subacc, { icp_p : Principal; tcycles_p : Principal }) : Result.Type<ChargeOk, { #GenericError : Error.Type; #BadFee : { expected_fee : Nat }; #InsufficientBalance : { balance : Nat; amount_required : Nat } }> = switch arg_fee {
    case (?fee) {
      let amt_required = if (fee.token == icp_p) {
        if (fee.amount != fee_amt.icp) {
          return #Err(#BadFee { expected_fee = fee_amt.icp });
        } else fee_amt.icp;
      } else if (fee.token == tcycles_p) {
        if (fee.amount != fee_amt.cycles) {
          return #Err(#BadFee { expected_fee = fee_amt.cycles });
        } else fee_amt.cycles;
      } else return Error.text("Fee token must be ICP (" # env.icp_p # ") or TCYCLES (" # env.tcycles_p # ")");
      var balance = getBalance(sub, fee.token);
      if (balance.unlocked < amt_required) {
        return #Err(#InsufficientBalance { balance = balance.unlocked; amount_required = amt_required });
      } else #Ok { fee with balance; amount = amt_required };
    };
    case _ {
      var balance = getBalance(sub, tcycles_p);
      if (balance.unlocked < fee_amt.cycles) {
        let cycles_bal = balance.unlocked;
        balance := getBalance(sub, icp_p);
        if (balance.unlocked < fee_amt.icp) {
          return #Err(#InsufficientBalance { balance = cycles_bal; amount_required = fee_amt.cycles });
        } else #Ok { balance; token = icp_p; amount = fee_amt.icp };
      } else #Ok { balance; token = tcycles_p; amount = fee_amt.cycles };
    };
  };

  public func extendConstant(c : KVT.Constant, t : Nat64) : KVT.Constant = ({
    c with expires_at = c.expires_at + t;
  });

  public func dedupeConstReserve(a : (Principal, KVT.ConstantReserveArg), b : (Principal, KVT.ConstantReserveArg)) : Order.Order {
    #equal // todo: finish this
  };

  public func dedupeConstExtend(a : (Principal, KVT.ConstantExtendArg), b : (Principal, KVT.ConstantExtendArg)) : Order.Order {
    #equal // todo: finish this
  };

  public func dedupeVarReserve(a : (Principal, KVT.VariableReserveArg), b : (Principal, KVT.VariableReserveArg)) : Order.Order {
    #equal // todo: finish this
  };

  public func dedupeVarExtend(a : (Principal, KVT.VariableExtendArg), b : (Principal, KVT.VariableExtendArg)) : Order.Order {
    #equal // todo: finish this
  };

  public func dedupeVarUpdate(a : (Principal, KVT.VariableUpdateArg), b : (Principal, KVT.VariableUpdateArg)) : Order.Order {
    #equal // todo: finish this
  };

  public func dedupeVarSponsor(a : (Principal, KVT.VariableSponsorArg), b : (Principal, KVT.VariableSponsorArg)) : Order.Order {
    #equal // todo: finish this
  };
};
