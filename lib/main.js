import "bootstrap";
import "bootstrap/css/bootstrap.css!";
import {get} from "./config.js";

let myapp = Elm.fullscreen(Elm.Main, { accessToken: null });

get("onedriveAccessToken").then(function (accessToken) {
    myapp.ports.accessToken.send(accessToken);
});
