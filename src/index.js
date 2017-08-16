import "./main.css";
import { Main } from "./Main.elm";

import unread from "./unread.json";

const userData = window.localStorage.getItem("userData");

const app = Main.embed(document.getElementById("root"), {
  data: userData
});
