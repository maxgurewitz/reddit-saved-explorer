require('./index.scss')

var Elm = require('./Main.elm').Elm;

var cache = JSON.parse(localStorage.getItem('cache') || '{}' );

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    publicPath: __webpack_public_path__,
    queryString: window.location.search,
    redditAuthState: cache.redditAuthState || null,
    redirectUri: 'http://localhost:3000',
    clientId: "30REJ3DOhCSiBQ"
  }
});

app.ports.cache.subscribe(function(data) {
  localStorage.setItem('cache', JSON.stringify(data));
});

app.ports.initializeReddit.subscribe(function(redditAccess) {
  console.log('loc1', redditAccess);
});
