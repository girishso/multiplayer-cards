
export function requestFullScreen() {
  try {
    let docElm = document.documentElement;
    if (typeof docElm.requestFullscreen !== 'undefined') {
        docElm.requestFullscreen();
    }
    else if (typeof docElm.mozRequestFullScreen !== 'undefined') {
        docElm.mozRequestFullScreen();
    }
    else if (typeof docElm.webkitRequestFullScreen !== 'undefined') {
        docElm.webkitRequestFullScreen();
    }
    else if (typeof docElm.msRequestFullscreen !== 'undefined') {
        docElm.msRequestFullscreen();
      }
  } catch(e) {
    console.log("e", e)
  }
}
