import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import * as FileDrop from '../../vinylkick-frontend/src/FileDrop'

Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

FileDrop.preventLoad()
