export function defaultData() {
  return {
    unread: null,
    lastUpdateTime: "",
    token: null
  };
}

export function key() {
  return "pinboard-unread-data";
}

export function getData() {
  return JSON.parse(localStorage.getItem(key()));
}

export function setData(data) {
  return localStorage.setItem(key(), JSON.stringify(data));
}

export function updateData(data) {
  setData(Object.assign(getData() || defaultData(), data));
}

export function removeData() {
  localStorage.removeItem(key());
}
