import { Elm } from './elm/Main.elm'

const app = Elm.Main.init({
  node: document.querySelector("main")
});

app.ports.sendTable.subscribe((data) => {
  // console.log(data)
  const math = document.querySelector("#math");
  const id = document.querySelector("#download")
  math.innerHTML = data;
  id.setAttribute("href", "https://latex.codecogs.com/png.latex?\\dpi{300} \\bg_white " + data.slice(2, -2))
  id.innerHTML = "Tải ảnh"
  try {
    MathJax.typesetClear(["#math"]);
    MathJax.typeset(["#math"]);
  }
  catch (e) { console.log(e) }
})