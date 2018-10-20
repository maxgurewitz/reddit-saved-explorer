import './index.scss';
import Snoowrap from 'snoowrap';
import { Elm } from './Main.elm';

const cache = JSON.parse(localStorage.getItem('cache') || '{}' );

function getStored(key) {
  var stored = localStorage.getItem(key);
  return stored === null ? null : JSON.parse(stored);
}

// homepage: https://maxgurewitz.github.io/reddit-saved-explorer/

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    publicPath: __webpack_public_path__,
    queryString: window.location.search,
    redditAuthState: getStored('redditAuthState'),
    redditAccess: getStored('redditAccess'),
    redirectUri: process.env.redirectUri,
    clientId: process.env.clientId
  }
});

let redditClient, me;

app.ports.cache.subscribe(data => {
  localStorage.setItem(data.key, JSON.stringify(data.value));
});

function getSavedContent(maybeParams) {
  const params = maybeParams || {};

  params.limit = 100;

  me.getSavedContent(params).then(content => {
    const saved = [];

    content.forEach(item => {
      saved.push({
        author: item.author.name,
        createdUtc: item.created_utc,
        name: item.name,
        over18: item.over_18,
        permalink: item.permalink,
        subreddit: item.subreddit.display_name,
        thumbnail: item.thumbnail || null,
        title: item.title || item.link_title,
        type: __proto__.constructor.name,
      });
    });

    app.ports.saved.send(saved);
  });
}

app.ports.pageReddit.subscribe(data => {
  getSavedContent(data.params);
});

app.ports.initializeReddit.subscribe(request => {
  redditClient = new Snoowrap({
    userAgent: 'web:saved-explorer:' + __webpack_hash__ + ' (by /u/maxgurewitz)',
    clientId: process.env.clientId,
    refreshToken: request.access.refresh_token,
    accessToken: request.access.access_token
  });

  me = redditClient.getMe();

  getSavedContent();
});
