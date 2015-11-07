import $ from "jquery";
import db from "./db.js";

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
        let config;

        try {
            config = await db.get("config");
        } catch (e) {
            config = {
                _id: "config"
            };
        }
        
        config.onedriveAccessToken = query.access_token;
        await db.put(config);
        window.location = "/main.html";
    }
});
