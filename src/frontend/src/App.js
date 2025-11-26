import { html, render } from 'lit-html';
import { kv_backend } from 'declarations/kv_backend';
import logo from './logo2.svg';

class App {
  constructor() {
    this.#render();
  }

  #render() {
    let body = html`
      <main>
      </main>
    `;
    render(body, document.getElementById('root'));
  }
}

export default App;
