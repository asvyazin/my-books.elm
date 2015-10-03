var dbInstances = {};

window.db = {
    instance: function (name) {
        if (!dbInstances[name]) {
            dbInstances[name] = new PouchDB(name);
        }

        return dbInstances[name];
    }
};
