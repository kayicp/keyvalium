import { canisterId as shelf1, idlFactory } from 'declarations/shelf_backend';

export default class Shelf {
	constructor(wallet) {
		this.wallet = wallet;
	}

	id() {
		return shelf1;
	}

	get() {
		
	};
}