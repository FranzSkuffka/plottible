import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import * as FileDrop from '../../vinylkick-frontend/src/FileDrop'

const flags = () => {
  try {
    return JSON.parse(localStorage.saved)
  } catch (e) {
    console.log(e)
    return null
  }
}

const app = Elm.Main.init({
    node: document.getElementById('root')
  , flags: flags()
});

registerServiceWorker();

FileDrop.preventLoad()


const save = data => {
  localStorage.saved = JSON.stringify(data)
}

app.ports.save_.subscribe(save)

const clear = () => {
  localStorage.clear()
}

app.ports.clear_.subscribe(clear)

const download = svgClasses => {
  console.log(svgClasses)
  const svgStrings = svgClasses
    .map(class_ => document.getElementsByClassName(class_)[0])
    .map(el => el.outerHTML)
  console.log(svgStrings)
  app.ports.startDownloads.send(svgStrings)
}

app.ports.download_.subscribe(download)
