import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import LZString from "lz-string"
import firebase from "firebase"

const getParameterByName = (name_, url) => {
    if (!url) url = window.location.href
    let name = name_.replace(/[\[\]]/g, "\\$&")
    let regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url)
    if (!results) return null
    if (!results[2]) return ""
    return decodeURIComponent(results[2].replace(/\+/g, " ").trim())
}

const getGameId = () => {
  return location.pathname.split('/')[1]
}

const c = new Clipboard("#copy_url_btn")

const compress = str => LZString.compressToUTF16(str)
const decompress = str => LZString.decompressFromUTF16(str)

const firebase_config = require("../secrets/firebase-app-config.json")
firebase.initializeApp(firebase_config)
const gamesRootRef = firebase.database().ref("games/")

let gameId = getGameId()
console.log("gameId", gameId)

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags:  { windowWidth: window.innerWidth,
            windowHeight: window.innerHeight,
            gameId: gameId,
            url: window.location.origin
          }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


if (gameId !== null) {
  // gamesRootRef.child(gameId).on("value", state => {
  //     const json = state.val()
  //     // console.log("  >> joined state: ", json)
  //     if (json.nPlayers >= 2) {
  //         app.ports.newSharedGameCreated.send(gameId)
  //     }
  //     if (typeof json.game_state !== "undefined" && json.game_state !== null) {
  //         let uncmpd = decompress(json.game_state)
  //         // console.log("  >> uncmpd: ", uncmpd)
  //         app.ports.gameStateChanged.send(JSON.parse(uncmpd))
  //     }
  // })
}

app.ports.focus.subscribe(el => document.getElementById(el).select())
const createNewGame = () => {
    gamesRootRef.push({ timestamp: Date.now() }).then(data => {
        console.log("  >> data: ", data.key)
        gameId = data.key
        app.ports.newGameCreated.send(data.key)
        // window.location.href = `/?game_id=${data.key}`
    })
}

app.ports.createNewGame.subscribe(str => createNewGame())

const gameRef = () => gamesRootRef.child(gameId)

app.ports.usernameSelected.subscribe(name => {
  gameRef().child(`players`).transaction(
        players => {
            console.log("  >>> players: ", players)
            localStorage.setItem("playerName", name)

            let newPlayers = players === null ? [] : players
            newPlayers.push(name)
            return newPlayers
        },
        (e, commited, snapshot) => {
            console.log("commited", commited, snapshot.val())
            if(commited) {
              app.ports.setPlayers.send(snapshot.val())
              watchPlayers()
            }
            // if (commited && snapshot.val() == 2) {
            //     app.ports.setThisPlayer.send("BlackPlayer")
            //     localStorage.setItem(gameId, "BlackPlayer")
            // }

        },
        false
    )


})

const watchPlayers = () => {
  if(gameRef() !== null) {
    gameRef().child(`players`).on("value", players => {
      console.log("   >>> watchPlayers players", players)
      if(players !== null)
        app.ports.setPlayers.send(players.val())
    })
  }
}

//
// app.ports.sendGameState.subscribe(str => {
//     let compressed = compress(str)
//     gamesRootRef.child(gameId).update({ game_state: compressed, timestamp: Date.now() })
// })
//
// app.ports.alert.subscribe(str => window.alert(str))
//
// app.ports.copyUrl.subscribe(el => {
//       document.getElementById(el).select()
//       try {
//             succeeded = document.execCommand("copy")
//         } catch (err) {}
//     })
