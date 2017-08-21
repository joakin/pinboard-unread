import "./animate.css";
import "./main.css";
import { Main } from "./Main.elm";
import { getData, updateData, removeData } from "./storage";

const app = Main.embed(document.getElementById("root"), {
  data: getData()
});

app.ports.save.subscribe(([token, lastUpdateTime, unread]) => {
  updateData({ token, lastUpdateTime, unread });
});

app.ports.logOut.subscribe(_ => {
  removeData();
});
