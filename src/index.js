import "./main.css";
import { Main } from "./Main.elm";

import unread from "./unread.json";

const app = Main.embed(document.getElementById("root"), {
  unread
});
