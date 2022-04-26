import { Elm } from "../src/Main.elm";
import * as serviceWorker from "./serviceWorker";
import "@picocss/pico/css/pico.css";
import "@fortawesome/fontawesome-free/css/fontawesome.css";
import "@fortawesome/fontawesome-free/css/brands.css";
import "@fortawesome/fontawesome-free/css/solid.css";
import "../css/main.css";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

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
