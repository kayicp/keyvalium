import { idlFactory } from 'declarations/icp_token';
import { genActor } from '../../../util/js/actor';
import { Principal } from '@dfinity/principal';
import { shortPrincipal } from '../../../util/js/principal';

export default class Token {
	anon = null;
  wallet = null;

	name = null;
  symbol = null;
  decimals = null;
  power = null;
  fee = null;

	balance = 0n;
  allowance = 0n;
  expires_at = null;

	constructor(id, wallet, spender) {
		this.id = id;
		this.id_p = Principal.fromText(id);
		this.wallet = wallet;
		this.notif = wallet.notif;
		this.spender_p = Principal.fromText(spender);
		this.get();
	}

	render() {
		this.wallet.render();
	};

	async get() {
		try {
			if (this.anon == null) this.anon = await genActor(idlFactory, this.id);
			if (this.power == null) {
				const [name, symbol, decimals, fee] = await Promise.all([
					this.anon.icrc1_name(),
					this.anon.icrc1_symbol(),
					this.anon.icrc1_decimals(),
					this.anon.icrc1_fee(),
				]);
				this.name = name;
				this.symbol = symbol;
				this.decimals = decimals;
				this.fee = fee;
				this.power = 10 ** Number(decimals);
			}
			this.render();
		} catch (cause) {
			return this.notif.errorToast(`Token ${shortPrincipal(this.id_p)} Meta Failed`, cause);
		};
		try {
			const account = { owner: this.wallet.principal, subaccount: [] };
			const spender = { owner: this.spender_p, subaccount: [] };
			if (account.owner != null) {
				const [balance, approval] = await Promise.all([
					this.anon.icrc1_balance_of(account),        
					this.anon.icrc2_allowance({ account, spender }),
				]);
				this.balance = balance;
				this.allowance = approval.allowance;
				this.expires_at = approval.expires_at[0];
			};
		} catch (cause) {
			this.notif.errorToast(`${this.symbol} Balance`, cause);
		};
	}

	clean(n){
    const res = Number(n) / this.power;
    return res.toFixed(this.decimals);
  }

  raw(n) {
    const res = Number(n) * this.power;
    return BigInt(Math.round(res));
  }

  price(quote, base) {
    const res = this.raw(Number(quote) / Number(base));
    return this.clean(res);
  }

	async approve(amt, spender_p) {
    const user = await genActor(idlFactory, this.id, this.wallet.agent);
    return user.icrc2_approve({
      from_subaccount: [],
      amount: amt + this.fee,
      spender: {
        owner: spender_p,
        subaccount: [],
      },
      fee: [this.fee],
      memo: [],
      created_at_time: [],
      expected_allowance: [],
      expires_at: [],
    })
  }

  async transfer(amt, to_p) {
    const user = await genActor(idlFactory, this.id, this.wallet.agent);
    return user.icrc1_transfer({
      amount: amt,
      to: { owner: to_p, subaccount: [] },
      fee: [this.fee],
      memo: [],
      from_subaccount : [],
      created_at_time : [],
    })
  }
} 