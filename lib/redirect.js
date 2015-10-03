import $ from "jquery";
import {put} from "./config.js";

function parseQuery(str) {
    var result = {};
    var vars = str.split("&");
    for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        result[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);
    }
    return result;
}

$(async function () {
    var query = parseQuery(window.location.hash.substring(1));
    if (query.access_token) {
        await put("onedriveAccessToken", query.access_token);
        window.location = "/main.html";
    }
});
