// @ts-expect-error
import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.querySelector("#app"),
})