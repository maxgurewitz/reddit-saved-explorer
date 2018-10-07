require('./index.scss')

var Elm = require('./Main.elm').Elm;

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    publicPath: __webpack_public_path__
  }
});
