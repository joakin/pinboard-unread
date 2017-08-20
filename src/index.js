import "./animate.css";
import "./main.css";
import { Main } from "./Main.elm";
import { getData, updateData, removeData } from "./storage";

import unread from "./unread.json";

const app = Main.embed(document.getElementById("root"), {
  data: getData()
});

app.ports.saveToken.subscribe(token => {
  updateData({ token });
});

app.ports.saveLastUpdateTime.subscribe(lastUpdateTime => {
  updateData({ lastUpdateTime });
});

app.ports.logOut.subscribe(_ => {
  removeData();
});
