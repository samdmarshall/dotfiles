"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const SVN = "svn";
class Configuration {
    constructor() {
        this._onDidChange = new vscode_1.EventEmitter();
        this.configuration = vscode_1.workspace.getConfiguration(SVN);
        vscode_1.workspace.onDidChangeConfiguration(this.onConfigurationChanged, this);
    }
    get onDidChange() {
        return this._onDidChange.event;
    }
    onConfigurationChanged(event) {
        if (!event.affectsConfiguration(SVN)) {
            return;
        }
        this.configuration = vscode_1.workspace.getConfiguration(SVN);
        this._onDidChange.fire(event);
    }
    get(section, defaultValue) {
        return this.configuration.get(section, defaultValue);
    }
    update(section, value) {
        return this.configuration.update(section, value);
    }
    inspect(section) {
        return this.configuration.inspect(section);
    }
}
exports.configuration = new Configuration();
//# sourceMappingURL=configuration.js.map