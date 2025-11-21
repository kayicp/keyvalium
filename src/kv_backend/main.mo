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
import Subaccount "../util/motoko/Subaccount";
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
  // var variables = RBTree.empty<Nat, KVT.Variable>();
  var blocks = RBTree.empty<Nat, ArchiveT.Block>();
  var deposit_dedupes = RBTree.empty<(Principal, KVT.TransferArg), Nat>();
  var withdraw_dedupes = RBTree.empty<(Principal, KVT.TransferArg), Nat>();
  var constant_reserve_dedupes = RBTree.empty<(Principal, KVT.ConstantReserveArg), Nat>();
  var constant_extend_dedupes = RBTree.empty<(Principal, KVT.ConstantExtendArg), Nat>();
  var variable_reserve_dedupes = RBTree.empty<(Principal, KVT.VariableReserveArg), Nat>();
  var variable_extend_dedupes = RBTree.empty<(Principal, KVT.VariableExtendArg), Nat>();
  var variable_update_dedupes = RBTree.empty<(Principal, KVT.VariableUpdateArg), Nat>();
  var variable_sponsor_dedupes = RBTree.empty<(Principal, KVT.VariableSponsorArg), Nat>();

  // switch deploy {

  // }
  var tip_cert = MerkleTree.empty();
  func updateTipCert() = CertifiedData.set(MerkleTree.treeHash(tip_cert)); // also call this on deploy.init
  system func postupgrade() = updateTipCert(); // https://gist.github.com/nomeata/f325fcd2a6692df06e38adedf9ca1877

  func getUser(p : Principal) : KVT.User = switch (RBTree.get(users, Principal.compare, p)) {
    case (?found) found;
    case _ RBTree.empty();
  };
  func saveUser(p : Principal, u : KVT.User) = users := if (RBTree.size(u) > 0) RBTree.insert(users, Principal.compare, p, u) else RBTree.delete(users, Principal.compare, p);

  func checkIdempotency(caller : Principal, opr : KVT.ArgType, env : KVT.Environment, created_at : ?Nat64) : Result.Type<(), { #CreatedInFuture : { ledger_time : Nat64 }; #TooOld; #Duplicate : { duplicate_of : Nat } }> {
    let ct = switch (created_at) {
      case (?defined) defined;
      case _ return #Ok;
    };
    let start_time = env.now - env.tx_window - env.permitted_drift;
    if (ct < start_time) return #Err(#TooOld);
    let end_time = env.now + env.permitted_drift;
    if (ct > end_time) return #Err(#CreatedInFuture { ledger_time = env.now });
    let find_dupe = switch opr {
      case (#Deposit arg) RBTree.get(deposit_dedupes, KVL.dedupe, (caller, arg));
      case (#Withdraw arg) RBTree.get(withdraw_dedupes, KVL.dedupe, (caller, arg));
      case (#ConstantReserve arg) RBTree.get(constant_reserve_dedupes, KVL.dedupeConstReserve, (caller, arg));
      case (#ConstantExtend arg) RBTree.get(constant_extend_dedupes, KVL.dedupeConstExtend, (caller, arg));
      case (#VariableReserve arg) RBTree.get(variable_reserve_dedupes, KVL.dedupeVarReserve, (caller, arg));
      case (#VariableExtend arg) RBTree.get(variable_extend_dedupes, KVL.dedupeVarExtend, (caller, arg));
      case (#VariableUpdate arg) RBTree.get(variable_update_dedupes, KVL.dedupeVarUpdate, (caller, arg));
      case (#VariableSponsor arg) RBTree.get(variable_sponsor_dedupes, KVL.dedupeVarSponsor, (caller, arg));
    };
    switch find_dupe {
      case (?duplicate_of) #Err(#Duplicate { duplicate_of });
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

    let user_acct = { owner = caller; subaccount = depo.subaccount };
    if (not ICRC1L.validateAccount(user_acct)) return Error.text("Caller account is not valid");

    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err err) return #Err err;
    };

    let token_txt = Principal.toText(depo.token);
    if (token_txt != env.icp_p and token_txt != env.tcycles_p) return Error.text("Unsupported token");

    let self_acct = { owner = Principal.fromActor(Self); subaccount = null };
    let token_canister = actor (token_txt) : ICRC1.Canister;
    let (fee_res, balance_res, allowance_res) = (token_canister.icrc1_fee(), token_canister.icrc1_balance_of(user_acct), token_canister.icrc2_allowance({ account = user_acct; spender = self_acct }));
    let (fee, balance, approval) = (await fee_res, await balance_res, await allowance_res);
    let withdrawal_fee = fee * env.withdrawal_fee_multiplier;
    if (depo.amount < withdrawal_fee + 1) return #Err(#AmountTooLow { minimum_amount = withdrawal_fee + 1 });
    switch (depo.fee) {
      case (?defined) if (defined != 0) return #Err(#BadFee { expected_fee = 0 });
      case _ ();
    };
    let xfer_and_fee = depo.amount + fee;
    if (balance < xfer_and_fee) return #Err(#InsufficientBalance { balance });
    if (approval.allowance < xfer_and_fee) return #Err(#InsufficientAllowance approval);

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
    let sub = Subaccount.get(depo.subaccount);
    var subacc = KVL.getSubacc(user, sub);
    var bal = KVL.getBalance(subacc, depo.token);
    bal := KVL.incUnlock(bal, depo.amount);
    subacc := KVL.saveBalance(subacc, depo.token, bal);
    user := KVL.saveSubacc(user, sub, subacc);
    saveUser(caller, user);

    let (block_id, phash) = ArchiveL.getPhash(blocks);
    if (depo.created_at != null) deposit_dedupes := RBTree.insert(deposit_dedupes, KVL.dedupe, (caller, depo), block_id);
    newBlock(block_id, KVL.valueTransfer("deposit", caller, 0, depo, xfer_id, env.now, phash));
    await* trim(env);
    #Ok block_id;
  };

  public shared ({ caller }) func vault_withdraw(draw : KVT.TransferArg) : async Result.Type<Nat, KVT.WithdrawError> {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return Error.text("Unavailable");

    let user_acct = { owner = caller; subaccount = draw.subaccount };
    if (not ICRC1L.validateAccount(user_acct)) return Error.text("Caller account is not valid");

    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err err) return #Err err;
    };

    let token_txt = Principal.toText(draw.token);
    if (token_txt != env.icp_p and token_txt != env.tcycles_p) return Error.text("Unsupported token");
    let token_canister = actor (token_txt) : ICRC1.Canister;

    let xfer_fee = await token_canister.icrc1_fee();
    let withdrawal_fee = xfer_fee * env.withdrawal_fee_multiplier;
    if (withdrawal_fee <= xfer_fee) return Error.text("Withdrawal fee must be larger than transfer fee");

    var user = getUser(caller);
    var sub = Subaccount.get(draw.subaccount);
    var subacc = KVL.getSubacc(user, sub);
    var bal = KVL.getBalance(subacc, draw.token);
    let to_lock = draw.amount + withdrawal_fee;
    if (bal.unlocked < to_lock) return #Err(#InsufficientBalance { balance = bal.unlocked });

    switch (draw.fee) {
      case (?defined) if (defined != withdrawal_fee) return #Err(#BadFee { expected_fee = withdrawal_fee });
      case _ ();
    };

    switch (checkIdempotency(caller, #Withdraw draw, env, draw.created_at)) {
      case (#Err err) return #Err err;
      case _ ();
    };
    bal := KVL.decUnlock(bal, to_lock);
    bal := KVL.incLock(bal, to_lock);
    subacc := KVL.saveBalance(subacc, draw.token, bal);
    user := KVL.saveSubacc(user, sub, subacc);
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
    subacc := KVL.getSubacc(user, sub);
    bal := KVL.getBalance(subacc, draw.token);
    bal := KVL.decLock(bal, to_lock); // release lock
    switch xfer_res {
      case (#Err _) bal := KVL.incUnlock(bal, to_lock); // recover fund
      case _ ();
    };
    subacc := KVL.saveBalance(subacc, draw.token, bal);
    user := KVL.saveSubacc(user, sub, subacc);
    saveUser(caller, user);
    let xfer_id = switch xfer_res {
      case (#Err err) return #Err(#TransferFailed err);
      case (#Ok ok) ok;
    };
    user := getUser(env.fee_collector);
    sub := Subaccount.get(null);
    subacc := KVL.getSubacc(user, sub);
    bal := KVL.getBalance(subacc, draw.token);
    bal := KVL.incUnlock(bal, withdrawal_fee - xfer_fee); // canister sponsored the xfer_fee
    subacc := KVL.saveBalance(subacc, draw.token, bal);
    user := KVL.saveSubacc(user, sub, subacc);
    saveUser(env.fee_collector, user);

    let (block_id, phash) = ArchiveL.getPhash(blocks);
    if (draw.created_at != null) withdraw_dedupes := RBTree.insert(withdraw_dedupes, KVL.dedupe, (caller, draw), block_id);
    newBlock(block_id, KVL.valueTransfer("withdraw", caller, withdrawal_fee, draw, xfer_id, env.now, phash));
    await* trim(env);
    #Ok block_id;
  };

  public shared ({ caller }) func constant_reserve(reserves : [KVT.ConstantReserveArg]) : async [Result.Type<Nat, KVT.ConstantReserveErr>] {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return [Error.textBatch("Unavailable")];
    if (not ICRC1L.validatePrincipal(caller)) return [Error.textBatch("Caller must not be Anonymous or Management")];

    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err(#GenericError err)) return [Error.textBatch(err.message)];
    };
    if (reserves.size() > env.max_update_batch) return [Error.textBatch("Batch too large! Current: " # debug_show reserves.size() # ", Max: " # debug_show env.max_update_batch)];
    let cmc = actor (env.cmc_p) : CMC.Self;
    let xdr_permyriad_per_icp = 35_474; // todo: remove this for prod // (await cmc.get_icp_xdr_conversion_rate()).data.xdr_permyriad_per_icp;
    let res = Buffer.Buffer<Result.Type<Nat, KVT.ConstantReserveErr>>(reserves.size());
    let icp_p = Principal.fromText(env.icp_p);
    let tcycles_p = Principal.fromText(env.tcycles_p);
    label working for (reserve in reserves.vals()) {
      if (not Subaccount.validate(reserve.subaccount)) {
        res.add(Error.text("Subaccount is invalid"));
        continue working;
      };
      if (reserve.duration < env.min_duration) {
        res.add(#Err(#DurationTooShort { minimum_duration = env.min_duration }));
        continue working;
      };
      let user_acct = { owner = caller; subaccount = reserve.subaccount };
      var user = getUser(caller);
      var sub = Subaccount.get(reserve.subaccount);
      var subacc = KVL.getSubacc(user, sub);
      let constant = KVL.newConstant(caller, sub, reserve, env.now);
      let fee_amt = KVL.calculateFees(to_candid (constant), reserve.duration, xdr_permyriad_per_icp);
      let fee_ready = switch (KVL.checkFee(reserve.fee, fee_amt, env, subacc, { icp_p; tcycles_p })) {
        case (#Err err) {
          res.add(#Err err);
          continue working;
        };
        case (#Ok ok) ok;
      };
      switch (checkIdempotency(caller, #ConstantReserve reserve, env, reserve.created_at)) {
        case (#Err err) {
          res.add(#Err err);
          continue working;
        };
        case _ ();
      };
      var bal = KVL.decUnlock(fee_ready.bal, fee_ready.amt);
      subacc := KVL.saveBalance(subacc, fee_ready.p, bal);
      let (block_id, phash) = ArchiveL.getPhash(blocks);
      subacc := KVL.insertConstant(subacc, block_id);
      user := KVL.saveSubacc(user, sub, subacc);
      saveUser(caller, user);

      constants := RBTree.insert(constants, Nat.compare, block_id, constant);
      if (reserve.created_at != null) constant_reserve_dedupes := RBTree.insert(constant_reserve_dedupes, KVL.dedupeConstReserve, (caller, reserve), block_id);
      // newBlock(block_id, KVL); todo: finish this

      user := getUser(env.fee_collector);
      sub := Subaccount.get(null);
      subacc := KVL.getSubacc(user, sub);
      bal := KVL.getBalance(subacc, fee_ready.p);
      bal := KVL.incUnlock(bal, fee_ready.amt);
      subacc := KVL.saveBalance(subacc, fee_ready.p, bal);
      user := KVL.saveSubacc(user, sub, subacc);
      saveUser(env.fee_collector, user);
    };
    await* trim(env);
    Buffer.toArray(res);
  };

  public shared ({ caller }) func constant_extend(extends : [KVT.ConstantExtendArg]) : async [Result.Type<Nat, KVT.ConstantExtendErr>] {
    if (not Value.getBool(meta, KVT.AVAILABLE, true)) return [Error.textBatch("Unavailable")];
    // let user_acct = { owner = caller; subaccount = null };
    if (not ICRC1L.validatePrincipal(caller)) return [Error.textBatch("Caller must not be Anonymous or Management")];

    let env = switch (KVL.getEnvironment(meta)) {
      case (#Ok ok) ok;
      case (#Err(#GenericError err)) return [Error.textBatch(err.message)];
    };
    if (extends.size() > env.max_update_batch) return [Error.textBatch("Batch too large! Current: " # debug_show extends.size() # ", Max: " # debug_show env.max_update_batch)];
    let cmc = actor (env.cmc_p) : CMC.Self;
    let xdr_permyriad_per_icp = 35_474; // todo: remove this for prod // (await cmc.get_icp_xdr_conversion_rate()).data.xdr_permyriad_per_icp;
    let res = Buffer.Buffer<Result.Type<Nat, KVT.ConstantExtendErr>>(extends.size());
    let icp_p = Principal.fromText(env.icp_p);
    let tcycles_p = Principal.fromText(env.tcycles_p);
    label working for (extend in extends.vals()) {

      if (extend.duration < env.min_duration) {
        res.add(#Err(#DurationTooShort { minimum_duration = env.min_duration }));
        continue working;
      };
      var constant = switch (RBTree.get(constants, Nat.compare, extend.id)) {
        case (?found) found;
        case _ {
          res.add(#Err(#NotFound));
          continue working;
        };
      };
      var user = getUser(caller);
      var sub = Subaccount.get(extend.subaccount);
      var subacc = KVL.getSubacc(user, sub);
      let fee_amt = KVL.calculateFees(to_candid (constant), extend.duration, xdr_permyriad_per_icp);
      let fee_ready = switch (KVL.checkFee(extend.fee, fee_amt, env, subacc, { icp_p; tcycles_p })) {
        case (#Err err) {
          res.add(#Err err);
          continue working;
        };
        case (#Ok ok) ok;
      };
      switch (checkIdempotency(caller, #ConstantExtend extend, env, extend.created_at)) {
        case (#Err err) {
          res.add(#Err err);
          continue working;
        };
        case _ ();
      };
      var bal = KVL.decUnlock(fee_ready.bal, fee_ready.amt);
      subacc := KVL.saveBalance(subacc, fee_ready.p, bal);
      user := KVL.saveSubacc(user, sub, subacc);
      saveUser(caller, user);

      constant := KVL.extendConstant(constant, extend.duration);
      constants := RBTree.insert(constants, Nat.compare, extend.id, constant);

      let (block_id, phash) = ArchiveL.getPhash(blocks);
      if (extend.created_at != null) constant_extend_dedupes := RBTree.insert(constant_extend_dedupes, KVL.dedupeConstExtend, (caller, extend), block_id);
      // newBlock(block_id, KVL); todo: finish this

      // let fee_take = (100 - env.owner_fee_pct) * fee_ready.amt / 100;
      user := getUser(env.fee_collector);
      sub := Subaccount.get(null);
      subacc := KVL.getSubacc(user, sub);
      bal := KVL.getBalance(subacc, fee_ready.p);
      bal := KVL.incUnlock(bal, fee_ready.amt);
      subacc := KVL.saveBalance(subacc, fee_ready.p, bal);
      user := KVL.saveSubacc(user, sub, subacc);
      saveUser(env.fee_collector, user);

      // user := getUser(constant.owner);
      // bal := KVL.getBalance(user, fee_ready.p);
      // bal := KVL.incUnlock(bal, fee_ready.amt - fee_take);
      // user := KVL.saveBalance(user, fee_ready.p, bal);
      // saveUser(constant.owner, user);
    };
    await* trim(env);
    Buffer.toArray(res);
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
