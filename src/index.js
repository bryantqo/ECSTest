import "core-js/stable";
import "regenerator-runtime/runtime";

import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


var params = 
  { size: {
      width: window.innerWidth,
      height: window.innerHeight
    }
  }


var elm = Elm.Main.init({
  node: document.getElementById('root'),
  flags: params
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


window.onresize = function()
{
  if(elm.ports.windowResized)
  {
    var newSize = {
      width: window.innerWidth,
      height: window.innerHeight
    };
    elm.ports.windowResized.send(newSize)
  }
}

