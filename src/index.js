require('./index.scss')

var Elm = require('./Main.elm').Elm;

var cache = JSON.parse(localStorage.getItem('cache') || '{}' );

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    publicPath: __webpack_public_path__,
    queryString: window.location.search,
    redditAuthCode: cache.redditAuthCode || null,
    redditAuthState: cache.redditAuthState || null,
  }
});

app.ports.cache.subscribe(function(data) {
  localStorage.setItem('cache', JSON.stringify(data));
});
