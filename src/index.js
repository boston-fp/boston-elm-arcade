import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

if (app && app.ports && app.ports.logToConsole) {
  // unused ports get stripped like any other function
  // when elm-make is passed --optimize
  app.ports.logToConsole.subscribe(string => console.log(string))
}


registerServiceWorker();