import RBTree "../util/motoko/StableCollections/RedBlackTree/RBTree";
import KVT "types";
import KVL "lib";
import Result "../util/motoko/Result";
import Value "../util/motoko/Value";
import LEB128 "mo:leb128";
import MerkleTree "../util/motoko/MerkleTree";
import CertifiedData "mo:base/CertifiedData";
import ArchiveT "../util/motoko/Archive/Types";
import ArchiveL "../util/motoko/Archive";
import Archive "../util/motoko/Archive/Canister";
import ICRC1L "../icrc1_canister/ICRC1";
import ICRC1T "../icrc1_canister/Types";
import ICRC1 "../icrc1_canister/main";
import Principal "mo:base/Principal";
import Blob "mo:base/Blob";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Nat64 "mo:base/Nat64";
import Error "../util/motoko/Error";
import ICRC3T "../util/motoko/ICRC-3/Types";
import OptionX "../util/motoko/Option";
import Cycles "mo:core/Cycles";
import Time64 "../util/motoko/Time64";
import CMC "../util/motoko/CMC/types";

shared (install) persistent actor class Canister(
  // deploy : {
  //   #Init : {};
  //   #Upgrade;
  // }
) = Self {
  var meta : Value.Metadata = RBTree.empty();
  var users = RBTree.empty<Principal, KVT.User>();
  var constants = RBTree.empty<Nat, KVT.Constant>();
  var variables = RBTree.empty<Nat, KVT.Variable>();
  var tokens = RBTree.empty<Principal, { withdrawal_fee : Nat }>();
  var blocks = RBTree.empty<Nat, ArchiveT.Block>();
  var deposit_dedupes : KVT.Dedupes = RBTree.empty();
  var withdraw_dedupes : KVT.Dedupes = RBTree.empty();

  // switch deploy {

  // }
  var tip_cert = MerkleTree.empty();
  func updateTipCert() = CertifiedData.set(MerkleTree.treeHash(tip_cert)); // also call this on deploy.init
  system func postupgrade() = updateTipCert(); // https://gist.github.com/nomeata/f325fcd2a6692df06e38adedf9ca1877

  func getToken(p : Principal) : ?(ICRC1.Canister, { withdrawal_fee : Nat }) = switch (RBTree.get(tokens, Principal.compare, p)) {
    case (?found) ?(actor (Principal.toText(p)), found);
    case _ null;
  };
  func getUser(p : Principal) : KVT.User = switch (RBTree.get(users, Principal.compare, p)) {
    case (?found) found;
    case _ ({
      balances = RBTree.empty();
      constants = RBTree.empty();
      variables = RBTree.empty();
    });
  };
  func saveUser(p : Principal, u : KVT.User) = users := if (RBTree.size(u.constants) > 0 or RBTree.size(u.balances) > 0) RBTree.insert(users, Principal.compare, p, u) else RBTree.delete(users, Principal.compare, p);
  func checkIdempotency(caller : Principal, opr : KVT.ArgType, env : KVT.Environment, created_at : ?Nat64) : Result.Type<(), { #CreatedInFuture : { ledger_time : Nat64 }; #TooOld; #Duplicate : { duplicate_of : Nat } }> {
    let ct = switch (created_at) {
      case (?defined) defined;
      case _ return #Ok;
    };
    let start_time = env.now - env.tx_window - env.permitted_drift;
    if (ct < start_time) return #Err(#TooOld);
    let end_time = env.now + env.permitted_drift;
    if (ct > end_time) return #Err(#CreatedInFuture { ledger_time = env.now });
    let (map, arg) = switch opr {
      case (#Deposit depo) (deposit_dedupes, depo);
      case (#Withdraw draw) (withdraw_dedupes, draw);
    };
    switch (RBTree.get(map, KVL.dedupe, (caller, arg))) {
      case (?duplicate_of) return #Err(#Duplicate { duplicate_of });
      case _ #Ok;
    };
  };
  func newBlock(block_id : Nat, val : Value.Type) {
    let valh = Value.hash(val);
    let idh = Blob.fromArray(LEB128.toUnsignedBytes(block_id));
    blocks := RBTree.insert(blocks, Nat.compare, block_id, { val; valh; idh; locked = false });

    tip_cert := MerkleTree.empty();
    tip_cert := MerkleTree.put(tip_cert, [Text.encodeUtf8(ICRC3T.LAST_BLOCK_INDEX)], idh);
    tip_cert := MerkleTree.put(tip_cert, [Text.encodeUtf8(ICRC3T.LAST_BLOCK_HASH)], valh);
    updateTipCert();
  };
  func trim(env : KVT.Environment) : async* () {
    var round = 0;
    var max_round = 100;
    let start_time = env.now - env.tx_window - env.permitted_drift;
    label trimming while (round < max_round) {
      let (p, arg) = switch (RBTree.minKey(deposit_dedupes)) {
        case (?found) found;
        case _ break trimming;
      };
      round += 1;
      switch (OptionX.compare(arg.created_at, ?start_time, Nat64.compare)) {
        case (#less) deposit_dedupes := RBTree.delete(deposit_dedupes, KVL.dedupe, (p, arg));
        case _ break trimming;
      };
    };
    label trimming while (round < max_round) {
      let (p, arg) = switch (RBTree.minKey(withdraw_dedupes)) {
        case (?found) found;
        case _ break trimming;
      };
      round += 1;
      switch (OptionX.compare(arg.created_at, ?start_time, Nat64.compare)) {
        case (#less) withdraw_dedupes := RBTree.delete(withdraw_dedupes, KVL.dedupe, (p, arg));
        case _ break trimming;
      };
    };
    if (round <= max_round) ignore await* sendBlock();
  };
  func sendBlock() : async* Result.Type<(), { #Sync : Error.Generic; #Async : Error.Generic }> {
    var max_batch = Value.getNat(meta, ArchiveT.MAX_UPDATE_BATCH_SIZE, 0);
    if (max_batch == 0) max_batch := 1;
    if (max_batch > 100) max_batch := 100;
    meta := Value.setNat(meta, ArchiveT.MAX_UPDATE_BATCH_SIZE, ?max_batch);

    if (RBTree.size(blocks) <= max_batch) return #Err(#Sync(Error.generic("Not enough blocks to archive", 0)));
    var locks = RBTree.empty<Nat, ArchiveT.Block>();
    let batch_buff = Buffer.Buffer<ICRC3T.BlockResult>(max_batch);
    label collecting for ((b_id, b) in RBTree.entries(blocks)) {
      if (b.locked) return #Err(#Sync(Error.generic("Some blocks are locked for archiving", 0)));
      locks := RBTree.insert(locks, Nat.compare, b_id, b);
      batch_buff.add({ id = b_id; block = b.val });
      if (batch_buff.size() >= max_batch) break collecting;
    };
    for ((b_id, b) in RBTree.entries(locks)) blocks := RBTree.insert(blocks, Nat.compare, b_id, { b with locked = true });
    func reunlock<T>(t : T) : T {
      for ((b_id, b) in RBTree.entries(locks)) blocks := RBTree.insert(blocks, Nat.compare, b_id, { b with locked = false });
      t;
    };
    let root = switch (Value.metaPrincipal(meta, ArchiveT.ROOT)) {
      case (?exist) exist;
      case _ switch (await* createArchive(null)) {
        case (#Ok created) created;
        case (#Err err) return reunlock(#Err(#Async(err)));
      };
    };
    let batch = Buffer.toArray(batch_buff);
    let start = batch[0].id;
    var prev_redir : ArchiveT.Redirect = #Ask(actor (Principal.toText(root)));
    var curr_redir = prev_redir;
    var next_redir = try await (actor (Principal.toText(root)) : Archive.Canister).rb_archive_ask(start) catch ee return reunlock(#Err(#Async(Error.convert(ee))));

    label travelling while true {
      switch (ArchiveL.validateSequence(prev_redir, curr_redir, next_redir)) {
        case (#Err msg) return reunlock(#Err(#Async(Error.generic(msg, 0))));
        case _ ();
      };
      prev_redir := curr_redir;
      curr_redir := next_redir;
      next_redir := switch next_redir {
        case (#Ask cnstr) try await cnstr.rb_archive_ask(start) catch ee return reunlock(#Err(#Async(Error.convert(ee))));
        case (#Add cnstr) {
          let cnstr_id = Principal.fromActor(cnstr);
          try {
            switch (await cnstr.rb_archive_add(batch)) {
              case (#Err(#InvalidDestination r)) r;
              case (#Err(#UnexpectedBlock x)) return reunlock(#Err(#Async(Error.generic("UnexpectedBlock: " # debug_show x, 0))));
              case (#Err(#MinimumBlockViolation x)) return reunlock(#Err(#Async(Error.generic("MinimumBlockViolation: " # debug_show x, 0))));
              case (#Err(#BatchTooLarge x)) return reunlock(#Err(#Async(Error.generic("BatchTooLarge: " # debug_show x, 0))));
              case (#Err(#GenericError x)) return reunlock(#Err(#Async(#GenericError x)));
              case (#Ok) break travelling;
            };
          } catch ee #Create(actor (Principal.toText(cnstr_id)));
        };
        case (#Create cnstr) {
          let cnstr_id = Principal.fromActor(cnstr);
          try {
            let slave = switch (await* createArchive(?cnstr_id)) {
              case (#Err err) return reunlock(#Err(#Async(err)));
              case (#Ok created) created;
            };
            switch (await cnstr.rb_archive_create(slave)) {
              case (#Err(#InvalidDestination r)) r;
              case (#Err(#GenericError x)) return reunlock(#Err(#Async(#GenericError x)));
              case (#Ok new_root) {
                meta := Value.setPrincipal(meta, ArchiveT.ROOT, ?new_root);
                meta := Value.setPrincipal(meta, ArchiveT.STANDBY, null);
                #Add(actor (Principal.toText(slave)));
              };
            };
          } catch ee return reunlock(#Err(#Async(Error.convert(ee))));
        };
      };
    };
    for (b in batch.vals()) blocks := RBTree.delete(blocks, Nat.compare, b.id);
    #Ok;
  };

  func createArchive(master : ?Principal) : async* Result.Type<Principal, Error.Generic> {
    let archive_tcycles = Value.getNat(meta, ArchiveT.MIN_TCYCLES, 0);
    if (archive_tcycles < 3) return Error.text("Metadata `" # ArchiveT.MIN_TCYCLES # "` is lower than 3");
    let trillion = 10 ** 12;
    let cost = archive_tcycles * trillion;
    if (Cycles.balance() < cost) return Error.text("Insufficient cycles balance to create a new archive");
    switch (Value.metaPrincipal(meta, ArchiveT.STANDBY)) {
      case (?standby) return try switch (await (actor (Principal.toText(standby)) : Archive.Canister).rb_archive_initialize(master)) {
        case (#Err err) #Err err;
        case _ #Ok standby;
      } catch e #Err(Error.convert(e));
      case _ ();
    };
    try {
      let new_canister = await (with cycles = cost) Archive.Canister(master);
      #Ok(Principal.fromActor(new_canister));
    } catch e #Err(Error.convert(e));
  };
  public shared ({ caller }) func vault_deposit(depo : KVT.TransferArg) : async Result.Type<Nat, KVT.DepositError> {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return Error.text("Unavailable");

    let user_acct = { owner = caller; subaccount = null };
    if (not ICRC1L.validateAccount(user_acct)) return Error.text("Caller account is not valid");

    let (token_canister, token) = switch (getToken(depo.token)) {
      case (?found) found;
      case _ return Error.text("Unsupported token");
    };
    if (depo.amount < token.withdrawal_fee + 1) return #Err(#AmountTooLow { minimum_amount = token.withdrawal_fee + 1 });
    switch (depo.fee) {
      case (?defined) if (defined != 0) return #Err(#BadFee { expected_fee = 0 });
      case _ ();
    };
    let self_acct = { owner = Principal.fromActor(Self); subaccount = null };
    let (fee_res, balance_res, allowance_res) = (token_canister.icrc1_fee(), token_canister.icrc1_balance_of(user_acct), token_canister.icrc2_allowance({ account = user_acct; spender = self_acct }));
    let (fee, balance, approval) = (await fee_res, await balance_res, await allowance_res);
    let xfer_and_fee = depo.amount + fee;
    if (balance < xfer_and_fee) return #Err(#InsufficientBalance { balance });
    if (approval.allowance < xfer_and_fee) return #Err(#InsufficientAllowance approval);

    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err err) return #Err err;
    };
    meta := env.meta;
    switch (checkIdempotency(caller, #Deposit depo, env, depo.created_at)) {
      case (#Err err) return #Err err;
      case _ ();
    };
    let xfer_arg = {
      from = user_acct;
      to = self_acct;
      spender_subaccount = self_acct.subaccount;
      fee = ?fee;
      amount = depo.amount;
      memo = null;
      created_at_time = null;
    };
    let xfer_id = switch (await token_canister.icrc2_transfer_from(xfer_arg)) {
      case (#Ok ok) ok;
      case (#Err err) return #Err(#TransferFailed err);
    };
    var user = getUser(caller);
    var bal = KVL.getBalance(user, depo.token);
    bal := KVL.incUnlock(bal, depo.amount);
    user := KVL.saveBalance(user, depo.token, bal);
    saveUser(caller, user);

    let (block_id, phash) = ArchiveL.getPhash(blocks);
    if (depo.created_at != null) deposit_dedupes := RBTree.insert(deposit_dedupes, KVL.dedupe, (caller, depo), block_id);
    newBlock(block_id, KVL.valueTransfer("deposit", caller, 0, depo, xfer_id, env.now, phash));
    await* trim(env);
    #Ok block_id;
  };

  public shared ({ caller }) func vault_withdraw(draw : KVT.TransferArg) : async Result.Type<Nat, KVT.WithdrawError> {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return Error.text("Unavailable");

    let user_acct = { owner = caller; subaccount = null };
    if (not ICRC1L.validateAccount(user_acct)) return Error.text("Caller account is not valid");

    let (token_canister, token) = switch (getToken(draw.token)) {
      case (?found) found;
      case _ return Error.text("Unsupported token");
    };
    let xfer_fee = await token_canister.icrc1_fee();
    if (token.withdrawal_fee <= xfer_fee) return Error.text("Withdrawal fee must be larger than transfer fee");

    var user = getUser(caller);
    var bal = KVL.getBalance(user, draw.token);
    let to_lock = draw.amount + token.withdrawal_fee;
    if (bal.unlocked < to_lock) return #Err(#InsufficientBalance { balance = bal.unlocked });

    switch (draw.fee) {
      case (?defined) if (defined != token.withdrawal_fee) return #Err(#BadFee { expected_fee = token.withdrawal_fee });
      case _ ();
    };
    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err err) return #Err err;
    };
    meta := env.meta;
    switch (checkIdempotency(caller, #Withdraw draw, env, draw.created_at)) {
      case (#Err err) return #Err err;
      case _ ();
    };
    bal := KVL.decUnlock(bal, to_lock);
    bal := KVL.incLock(bal, to_lock);
    user := KVL.saveBalance(user, draw.token, bal);
    saveUser(caller, user);
    let xfer_arg = {
      amount = draw.amount;
      to = user_acct;
      fee = ?xfer_fee;
      memo = null;
      from_subaccount = null;
      created_at_time = null;
    };
    let xfer_res = await token_canister.icrc1_transfer(xfer_arg);
    user := getUser(caller);
    bal := KVL.getBalance(user, draw.token);
    bal := KVL.decLock(bal, to_lock); // release lock
    switch xfer_res {
      case (#Err _) bal := KVL.incUnlock(bal, to_lock); // recover fund
      case _ ();
    };
    user := KVL.saveBalance(user, draw.token, bal);
    saveUser(caller, user);
    let xfer_id = switch xfer_res {
      case (#Err err) return #Err(#TransferFailed err);
      case (#Ok ok) ok;
    };
    let this_canister = Principal.fromActor(Self); // todo: set this to fee collector
    user := getUser(this_canister); // give fee to canister
    bal := KVL.getBalance(user, draw.token);
    bal := KVL.incUnlock(bal, token.withdrawal_fee - xfer_fee); // canister sponsored the xfer_fee
    user := KVL.saveBalance(user, draw.token, bal);
    saveUser(this_canister, user);

    let (block_id, phash) = ArchiveL.getPhash(blocks);
    if (draw.created_at != null) withdraw_dedupes := RBTree.insert(withdraw_dedupes, KVL.dedupe, (caller, draw), block_id);
    newBlock(block_id, KVL.valueTransfer("withdraw", caller, token.withdrawal_fee, draw, xfer_id, env.now, phash));
    await* trim(env);
    #Ok block_id;
  };

  public shared ({ caller }) func constant_reserve(reserves : [KVT.ConstantReserveArg]) : async [Result.Type<Nat, KVT.ConstantReserveErr>] {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return [Error.textBatch("Unavailable")];
    let user_acct = { owner = caller; subaccount = null };
    if (not ICRC1L.validateAccount(user_acct)) return [Error.textBatch("Caller must not be Anonymous or Management")];

    let cmc = actor ("rkp4c-7iaaa-aaaaa-aaaca-cai") : CMC.Self;
    let xdr_permyriad_per_icp = 35_474; // todo: remove this for prod // (await cmc.get_icp_xdr_conversion_rate()).data.xdr_permyriad_per_icp;

    let minimum_duration = switch (Value.metaNat(meta, KVT.MIN_DURATION)) {
      case (?found) Time64.SECONDS(Nat64.fromNat(found));
      case _ return [Error.textBatch("Metadata `" # KVT.MIN_DURATION # "` missing")];
    };
    let caller_blob = Principal.toBlob(caller);
    let caller_size = caller_blob.size();
    let now = Time64.nanos();
    let res = Buffer.Buffer<Result.Type<Nat, KVT.ConstantReserveErr>>(reserves.size());
    label inserting for (const in reserves.vals()) {
      if (const.duration < minimum_duration) {
        res.add(#Err(#DurationTooShort { minimum_duration }));
        continue inserting;
      };
      let constant = KVL.newConstant(const, now);
      let constant_blob = to_candid (constant);
      let constant_size = constant_blob.size();

      let duration_sec = Nat64.toNat(const.duration / Time64.SECONDS(1));
      var cycles_required = (127_000 * constant_size * duration_sec) / 1_073_741_824; // 1 GiB/s = 127k cycles
      cycles_required += 1_200_000; // cycles per update message received (user -> canister)
      cycles_required += 5_000_000; // 5,000,000 cycles per update message
      // wasm execution is sponsored
      // intercanister call is sponsored
      cycles_required += 200 * cycles_required / 100; // todo: put premium_percent in metadata

      // (cycles_required * icp_decimals * per_myriad) / (1T per XDR * xdr_permyriad_per_icp)
      let icp_required = (cycles_required * 100_000_000 * 10_000) / (1_000_000_000_000 * xdr_permyriad_per_icp);

      // switch (reserve.fee) {
      //   case (?fee)
      // };
    };

    [];
  };

  public shared ({ caller }) func constant_extend(extend : KVT.ConstantExtendArg) : async Result.Type<Nat, KVT.ConstantExtendErr> {
    #Ok 1;
  };

  public shared ({ caller }) func variable_reserve() : async Result.Type<Nat, KVT.VariableReserveErr> {
    Error.text("Not implemented yet");
  };

  public shared ({ caller }) func variable_extend() : async Result.Type<Nat, KVT.VariableExtendErr> {
    Error.text("Not implemented yet");
  };

  public shared ({ caller }) func variable_update() : async Result.Type<Nat, KVT.VariableUpdateErr> {
    Error.text("Not implemented yet");
  };

  public shared ({ caller }) func variable_sponsor() : async Result.Type<Nat, KVT.VariableSponsorErr> {
    Error.text("Not yet implemented");
  };
};
