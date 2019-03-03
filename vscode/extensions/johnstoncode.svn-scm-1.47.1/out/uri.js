"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
function fromSvnUri(uri) {
    return JSON.parse(uri.query);
}
exports.fromSvnUri = fromSvnUri;
function toSvnUri(uri, action, extra = {}, replaceFileExtension = false) {
    const params = {
        action,
        fsPath: uri.fsPath,
        extra
    };
    return uri.with({
        scheme: "svn",
        path: replaceFileExtension ? uri.path + ".svn" : uri.path,
        query: JSON.stringify(params)
    });
}
exports.toSvnUri = toSvnUri;
function getIconUri(iconName, theme) {
    const iconsRootPath = path.join(__dirname, "..", "icons");
    return vscode_1.Uri.file(path.join(iconsRootPath, theme, `${iconName}.svg`));
}
exports.getIconUri = getIconUri;
//# sourceMappingURL=uri.js.map