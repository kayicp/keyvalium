import { html, render } from 'lit-html';
import logo from './logo2.svg';
import PubSub from '../../util/js/pubsub';
import Notif from './model/Notif';
import Wallet from './model/Wallet';
import Token from './model/Token';
import Shelf from './model/Shelf';

Principal.prototype.toString = function () {
  return this.toText();
}
Principal.prototype.toJSON = function () {
  return this.toString();
}
BigInt.prototype.toJSON = function () {
  return this.toString();
};

const blob2hex = blob => Array.from(blob).map(byte => byte.toString(16).padStart(2, '0')).join('');
Uint8Array.prototype.toJSON = function () {
  return blob2hex(this) // Array.from(this).toString();
}

const pubsub = new PubSub();
const notif = new Notif(pubsub);
const wallet = new Wallet(notif);
const echo = new Shelf(wallet);
const icp_token = new Token("ryjl3-tyaaa-aaaaa-aaaba-cai", wallet, echo.id());
const tcycles_token = new Token("um5iw-rqaaa-aaaaq-qaaba-cai", wallet, echo.id());

class App {
  constructor() {
    // simple local state
    this.connected = false;
    this.address = null;
    this.busy = false;

    // initial render
    this.#render();
  }

  // small helper to create a mock address (replace with real wallet connection later)
  _mockAddress() {
    const hex = () =>
      Array.from({ length: 6 }, () =>
        Math.floor(Math.random() * 16).toString(16)
      ).join('');
    return `0x${hex()}${hex()}${hex()}`;
  }

  // format address like 0x1234…abcd
  _shortAddress(addr = '') {
    if (!addr) return '';
    return `${addr.slice(0, 6)}…${addr.slice(-4)}`;
  }

  // user actions
  async _connect() {
    if (this.busy) return;
    this.busy = true;
    this.#render();

    // simulate async wallet connection flow (replace with real wallet code)
    setTimeout(() => {
      this.address = this._mockAddress();
      this.connected = true;
      this.busy = false;
      this.#render();
    }, 450);
  }

  _disconnect() {
    this.address = null;
    this.connected = false;
    this.#render();
  }

  // keyboard accessible quick handler for nav items
  _onKeyActivate(fn, evt) {
    if (evt.key === 'Enter' || evt.key === ' ') {
      evt.preventDefault();
      fn();
    }
  }

  // main render
  #render() {
    const nav = html`
      <div class="min-h-screen bg-gray-900 text-gray-100 antialiased">
        <nav
          class="w-full border-b border-gray-800 bg-gradient-to-b from-gray-900/80 to-gray-900/60"
          role="navigation"
          aria-label="Main navigation"
        >
          <div class="max-w-4xl mx-auto px-3 py-3 flex items-center justify-between gap-4">
            <!-- Left: Logo + title -->
            <div class="flex items-center gap-3">
              <a
                href="#"
                class="flex items-center gap-3 focus:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-indigo-500 rounded"
                aria-label="Echo home"
                @click=${(e) => {
                  e.preventDefault();
                  // In a real app you'd route home; here we simply re-render.
                  this.#render();
                }}
              >
                <img src="${logo}" alt="Echo logo" class="h-8 w-8 rounded-md" />
                <div class="leading-tight">
                  <div class="text-sm font-semibold tracking-tight">Echo</div>
                  <div class="text-xs text-gray-400 -mt-0.5">Community Data</div>
                </div>
              </a>
            </div>

            <!-- Center: Compact nav -->
            <ul
              class="hidden sm:flex items-center gap-2"
              role="menu"
              aria-label="Primary"
            >
              <li role="none">
                <a
                  href="#balances"
                  role="menuitem"
                  class="inline-flex items-center px-3 py-1.5 text-sm rounded-md hover:bg-gray-800/60 focus:outline-none focus-visible:ring-2 focus-visible:ring-indigo-500"
                  @click=${(e) => {
                    e.preventDefault();
                    // placeholder for balances view
                    const main = document.querySelector('main');
                    if (main) {
                      main.innerHTML = '<section class="p-4 text-sm text-gray-300">Balances view (placeholder)</section>';
                    }
                  }}
                  @keydown=${(ev) => this._onKeyActivate(() => ev.target.click(), ev)}
                >
                  Balances
                </a>
              </li>
            </ul>

            <!-- Right: Connect -->
            <div class="flex items-center gap-2">
              ${this.connected
                ? html`
                    <div class="flex items-center gap-2">
                      <span
                        class="px-3 py-1.5 text-sm rounded-full bg-gray-800/60 border border-gray-700 text-gray-200 font-mono"
                        aria-live="polite"
                      >
                        ${this._shortAddress(this.address)}
                      </span>
                      <button
                        class="inline-flex items-center px-3 py-1.5 text-sm rounded-md bg-transparent border border-gray-700 hover:bg-gray-800/50 focus:outline-none focus-visible:ring-2 focus-visible:ring-offset-1 focus-visible:ring-red-500"
                        @click=${() => this._disconnect()}
                        aria-label="Disconnect wallet"
                      >
                        Disconnect
                      </button>
                    </div>
                  `
                : html`
                    <button
                      class="inline-flex items-center gap-2 px-3 py-1.5 text-sm rounded-md bg-indigo-600 text-white font-medium shadow-sm hover:bg-indigo-500 focus:outline-none focus-visible:ring-2 focus-visible:ring-offset-1 focus-visible:ring-indigo-400"
                      @click=${() => this._connect()}
                      aria-pressed="${this.connected ? 'true' : 'false'}"
                      aria-label="Connect wallet"
                    >
                      ${this.busy
                        ? html`<svg class="h-4 w-4 animate-spin" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="10"></circle><path d="M22 12a10 10 0 0 1-10 10"></path></svg>`
                        : html`<svg class="h-4 w-4" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 2v20M2 12h20"/></svg>`
                      }
                      <span>Connect</span>
                    </button>
                  `}
            </div>
          </div>
        </nav>

        <!-- Main content area (kept minimal & compact) -->
        <main class="max-w-4xl mx-auto px-3 py-6" id="app-main" role="main">
          <section class="rounded-xl border border-gray-800/60 p-4 bg-gradient-to-b from-gray-850/40 to-gray-900/30">
            <h1 class="text-lg font-semibold">Welcome to Echo</h1>
            <p class="mt-2 text-sm text-gray-400">
              A compact, public-good interface for community-sustained public data.
              Use the Connect button to simulate a wallet session and view balances.
            </p>
          </section>
        </main>
      </div>
    `;

    render(nav, document.getElementById('root'));
  }
}

export default App;
