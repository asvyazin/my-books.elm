import "bootstrap";
import "bootstrap/css/bootstrap.css!";
import "/css/styles.css!";
import db from "./db.js";
import $ from "jquery";


let myapp = Elm.fullscreen(Elm.Main, { accessToken: null });


db.get("config").then(function (config) {
    myapp.ports.accessToken.send(config.onedriveAccessToken);
});


myapp.ports.showOneDriveDirectoryChooserModal.subscribe(id => {
    $("#" + id).modal("show");
});


myapp.ports.hideOneDriveDirectoryChooserModal.subscribe(id => {
    $("#" + id).modal("hide");
});


myapp.ports.saveFolder.subscribe(async folderId => {
    let config = await db.get("config");
    config.booksFolderId = folderId;
    await db.put(config);
    console.log("saved folderId:");
    console.log(folderId);
});
