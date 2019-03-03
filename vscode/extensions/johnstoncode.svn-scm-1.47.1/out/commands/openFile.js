"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const vscode_1 = require("vscode");
const resource_1 = require("../resource");
const incomingChangeNode_1 = require("../treeView/nodes/incomingChangeNode");
const uri_1 = require("../uri");
const command_1 = require("./command");
class OpenFile extends command_1.Command {
    constructor() {
        super("svn.openFile");
    }
    execute(arg, ...resourceStates) {
        return __awaiter(this, void 0, void 0, function* () {
            const preserveFocus = arg instanceof resource_1.Resource;
            let uris;
            if (arg instanceof vscode_1.Uri) {
                if (arg.scheme === "svn") {
                    uris = [vscode_1.Uri.file(uri_1.fromSvnUri(arg).fsPath)];
                }
                else if (arg.scheme === "file") {
                    uris = [arg];
                }
            }
            else if (arg instanceof incomingChangeNode_1.default) {
                const resource = new resource_1.Resource(arg.uri, arg.type, undefined, arg.props, true);
                uris = [resource.resourceUri];
            }
            else {
                const resource = arg;
                if (!(resource instanceof resource_1.Resource)) {
                    // can happen when called from a keybinding
                    // TODO(@JohnstonCode) fix this
                    // resource = this.getSCMResource();
                }
                if (resource) {
                    uris = [
                        ...resourceStates.map(r => r.resourceUri),
                        resource.resourceUri
                    ];
                }
            }
            if (!uris) {
                return;
            }
            const preview = uris.length === 1 ? true : false;
            const activeTextEditor = vscode_1.window.activeTextEditor;
            for (const uri of uris) {
                if (fs.existsSync(uri.fsPath) && fs.statSync(uri.fsPath).isDirectory()) {
                    continue;
                }
                const opts = {
                    preserveFocus,
                    preview,
                    viewColumn: vscode_1.ViewColumn.Active
                };
                if (activeTextEditor &&
                    activeTextEditor.document.uri.toString() === uri.toString()) {
                    opts.selection = activeTextEditor.selection;
                }
                const document = yield vscode_1.workspace.openTextDocument(uri);
                yield vscode_1.window.showTextDocument(document, opts);
            }
        });
    }
}
exports.OpenFile = OpenFile;
//# sourceMappingURL=openFile.js.map