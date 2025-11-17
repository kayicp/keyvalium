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
import Option "../util/motoko/Option";

module {
  public func newConstant(arg : KVT.ConstantReserveArg, now : Nat64) : KVT.Constant = ({
    arg with expires_at = now + arg.duration;
    created_at = now;
  });

  public func getEnvironment(_meta : Value.Metadata) : Result.Type<KVT.Environment, Error.Generic> {
    var meta = _meta;
    let now = Time64.nanos();
    let tx_window = Time64.SECONDS(Nat64.fromNat(Value.getNat(meta, KVT.TX_WINDOW, 0)));
    // if (tx_window < min_tx_window) {
    //   tx_window := min_tx_window;
    //   meta := Value.setNat(meta, KVT.TX_WINDOW, ?(Nat64.toNat(tx_window)));
    // };
    let permitted_drift = Time64.SECONDS(Nat64.fromNat(Value.getNat(meta, KVT.PERMITTED_DRIFT, 0)));
    // if (permitted_drift < min_permitted_drift) {
    //   permitted_drift := min_permitted_drift;
    //   meta := Value.setNat(meta, KVT.PERMITTED_DRIFT, ?(Nat64.toNat(permitted_drift)));
    // };
    #Ok {
      meta;
      now;
      tx_window;
      permitted_drift;
    };
  };

  public func getBalance(u : KVT.User, token : Principal) : KVT.Balance = switch (RBTree.get(u.balances, Principal.compare, token)) {
    case (?found) found;
    case _ ({ unlocked = 0; locked = 0 });
  };
  public func saveBalance(u : KVT.User, token : Principal, b : KVT.Balance) : KVT.User = ({
    u with balances = if (b.unlocked > 0 or b.locked > 0) RBTree.insert(u.balances, Principal.compare, token, b) else RBTree.delete(u.balances, Principal.compare, token);
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
  public func valueTransfer(op : Text, owner : Principal, fee : Nat, arg : KVT.TransferArg, xfer : Nat, now : Nat64, phash : ?Blob) : Value.Type {
    var tx = RBTree.empty<Text, Value.Type>();
    tx := Value.setPrincipal(tx, "caller", ?owner);
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
};
