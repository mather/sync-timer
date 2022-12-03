import { Elm } from "../src/Main.elm";
import * as serviceWorker from "./serviceWorker";
import "@picocss/pico/css/pico.css";
import "@fortawesome/fontawesome-free/css/fontawesome.css";
import "@fortawesome/fontawesome-free/css/brands.css";
import "@fortawesome/fontawesome-free/css/solid.css";
import "../css/main.scss";

const searchParams = (new URL(document.location)).searchParams;
const parseFg = (s) => {
  if (!s) return null;
  const re = /^#[0-9a-f]{6}$/;
  if (re.test(s)) return s;
  return null;
}
const parseInit = (s) => {
  if (s) {
    const n = parseInt(s, 10);
    if (n > 30 || n < -30) return null;
    return n;
  }
  return null;
}
const flags = {
  fg: parseFg(searchParams.get("fg")),
  bg: searchParams.get("bg"),
  init: parseInit(searchParams.get("init")),
  h: searchParams.get("h")
};

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: flags
});

app.ports.setQueryString.subscribe((newQS) => {
  const currentUrl = new URL(document.location);
  const newUrl = currentUrl.origin + currentUrl.pathname + newQS;
  window.history.replaceState(null, "", newUrl);
})

app.ports.sendAnalyticsEvent.subscribe((event) => {
  const { category, action, label, value } = JSON.parse(event);
  console.debug({ category, action, label, value });
  if (gtag) {
    const data = Object.assign({
      "event_category": category,
      "event_label": label
    }, value ? { value } : {});
    gtag("event", action, data);
  }
});
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
