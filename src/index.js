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

var redditClient, me;

app.ports.cache.subscribe(function(data) {
  localStorage.setItem(data.key, JSON.stringify(data.value));
});

function getSavedContent(maybeParams) {
  var params = maybeParams || {};
  params.limit = 100;

  me.getSavedContent(params).then(function (content) {
    var saved = [];

    content.forEach(function (item) {
      saved.push({
        author: item.author.name,
        created_utc: item.created_utc,
        name: item.name,
        permalink: item.permalink,
        subreddit: item.subreddit.display_name,
        thumbnail: item.thumbnail || null,
        title: item.title || item.link_title,
      });
    });

    app.ports.saved.send(saved);
  });
}

app.ports.pageReddit.subscribe(function(data) {
  getSavedContent(data.params);
});

app.ports.initializeReddit.subscribe(function(request) {
  redditClient = new Snoowrap({
    userAgent: 'web:saved-explorer:' + __webpack_hash__ + ' (by /u/maxgurewitz)',
    clientId: clientId,
    refreshToken: request.access.refresh_token,
    accessToken: request.access.access_token
  });

  me = redditClient.getMe();
  getSavedContent();
});
