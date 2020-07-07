import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import LZString from 'lz-string';
import firebase from 'firebase';

const getGameId = () => location.pathname.split('/')[1];

const c = new Clipboard('#copy_url_btn');

const compress = (str) => LZString.compressToUTF16(str);
const decompress = (str) => LZString.decompressFromUTF16(str);

const firebase_config = require('../secrets/firebase-app-config.json');
firebase.initializeApp(firebase_config);
const gamesRootRef = firebase.database().ref('games/');

let gameId = getGameId();
console.log('gameId', gameId);

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    windowWidth: window.innerWidth,
    windowHeight: window.innerHeight,
    gameId: gameId,
    url: window.location.origin,
    playerName: localStorage.getItem('playerName'),
  },
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

const gameIdPresent = () => gameId !== null && gameId !== '';
const gameRef = () => (gameIdPresent() ? gamesRootRef.child(gameId) : null);

const watchGameState = () => {
  if (gameIdPresent()) {
    gameRef()
      .child('game_state')
      .on('value', (state) => {
        const [name, json] = state.val();
        // console.log("  >> joined state: ", json)

        if (typeof json !== 'undefined' && json !== null) {
          const uncmpd = decompress(json);
          // console.log("  >> uncmpd: ", uncmpd)
          console.log("gameStateNDefChanged", name)

          const parsed = JSON.parse(uncmpd);
          app.ports.gameStateNDefChanged.send(parsed);
        }
      });
  }
};

const watchGameStarted = () => {
  if (gameIdPresent()) {
    gameRef()
      .child('game_started')
      .on('value', (v) => {
        if (v.val()) {
          watchGameState();
          app.ports.gameStarted.send(null);
          // stop watching started status
          gameRef().child('game_started').off('value');
          gameRef().child(`players`).off('value');
        }
      });
  }
};


const createNewGame = () => {
  gamesRootRef
    .push({
      timestamp: Date.now(),
      game_state: `{"dummy": "dummy state"}`,
      game_started: false,
    })
    .then((data) => {
      console.log('  >> data: ', data.key);
      gameId = data.key;
      app.ports.newGameCreated.send(data.key);
    });
};
watchGameStarted();

app.ports.createNewGame.subscribe((str) => createNewGame());
app.ports.focus.subscribe((el) => document.getElementById(el).select());

app.ports.usernameSelected.subscribe((name) => {
  watchGameStarted();
  gameRef()
    .child(`players`)
    .transaction(
      (players) => {
        // console.log("  >>> players: ", players)
        localStorage.setItem('playerName', name);

        let newPlayers = players === null ? [] : players;
        if (newPlayers.includes(name)) {
          return;
        } else {
          newPlayers.push(name);
          return newPlayers;
        }
      },
      (e, commited, snapshot) => {
        console.log('commited', commited, snapshot.val());
        if (commited) {
          app.ports.setPlayers.send(snapshot.val());
          watchPlayers();
        }
      },
      false
    );
});

app.ports.sendGameStateNDef.subscribe(([name, str]) => {
  // console.log("sendGameStateNDef",name);
  let compressed = compress(str);
  gameRef().update({
    game_state: [name, compressed],
    timestamp: Date.now(),
    game_started: true,
  });
});

const watchPlayers = () => {
  if (gameRef() !== null) {
    gameRef()
      .child(`players`)
      .on('value', (players) => {
        console.log('   >>> watchPlayers players', players);
        if (players !== null) app.ports.setPlayers.send(players.val());
      });
  }
};

//
// app.ports.alert.subscribe(str => window.alert(str))
//
// app.ports.copyUrl.subscribe(el => {
//       document.getElementById(el).select()
//       try {
//             succeeded = document.execCommand("copy")
//         } catch (err) {}
//     })
