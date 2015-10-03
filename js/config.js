window.config = {
    get: function (param) {
        return db.instance("config").get("config").then(function (doc) {
            return doc[param];
        });
    },

    put: function (param, val) {
        var d = db.instance("config");
        return d.get("config").then(function (doc) {
            doc[param] = val;
            return d.put(doc);
        }).catch(function (e) {
            if (e.name !== "not_found") {
                throw e;
            }

            var config = {
                _id: "config",
            };
            config[param] = val;
            return d.put(config);
        });
    }
};
