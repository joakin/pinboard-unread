import "./animate.css";
import "./main.css";
import { Main } from "./Main.elm";
import { getData, updateData, removeData } from "./storage";

const app = Main.embed(document.getElementById("root"), {
  data: getData()
});

app.ports.ready.subscribe(_ => {
  const loadingOverlay = document.querySelector('.loading-overlay')
  requestAnimationFrame(_ => {
    if (loadingOverlay) { // When hot reloading the div would've already been removed
      loadingOverlay.classList.add('animated', 'fadeOut');
      setTimeout(_ => loadingOverlay.remove(), 1000);
    }
  })
});

app.ports.save.subscribe(([token, lastUpdateTime, unread]) => {
  updateData({ token, lastUpdateTime, unread });
});

app.ports.logOut.subscribe(_ => {
  removeData();
});
