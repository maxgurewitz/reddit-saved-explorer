require('./index.scss')

var Snoowrap = require('snoowrap');
var Elm = require('./Main.elm').Elm;

var cache = JSON.parse(localStorage.getItem('cache') || '{}' );
var clientId = "30REJ3DOhCSiBQ";

function getStored(key) {
  var stored = localStorage.getItem(key);
  return stored === null ? null : JSON.parse(stored);
}

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    publicPath: __webpack_public_path__,
    queryString: window.location.search,
    redditAuthState: getStored('redditAuthState'),
    redditAccess: getStored('redditAccess'),
    redirectUri: 'http://localhost:3000',
    clientId: clientId
  }
});

var redditClient;

app.ports.cache.subscribe(function(data) {
  localStorage.setItem(data.key, JSON.stringify(data.value));
});

app.ports.initializeReddit.subscribe(function(redditAccess) {
  redditClient = new Snoowrap({
    userAgent: 'web:saved-explorer:' + __webpack_hash__ + ' (by /u/maxgurewitz)',
    clientId: clientId,
    refreshToken: redditAccess.refresh_token,
    accessToken: redditAccess.access_token
  });

  redditClient.getMe().getSavedContent().then(function (content) {
    console.log('loc1', content);
  });
});
