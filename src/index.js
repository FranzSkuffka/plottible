import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import * as FileDrop from '../../vinylkick-frontend/src/FileDrop'

const flags = () => {
  try {
    return JSON.parse(localStorage.saved)
  } catch (e) {
    console.log(e)
    return []
  }
}

const app = Elm.Main.init({
    node: document.getElementById('root')
  , flags: flags()
});

registerServiceWorker();

FileDrop.preventLoad()
console.log(app.ports.save)


const save = data => {
  localStorage.saved = JSON.stringify(data)
}

app.ports.save.subscribe(save)
