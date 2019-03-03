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
const path = require("path");
const vscode_1 = require("vscode");
const types_1 = require("../common/types");
const uri_1 = require("../uri");
const command_1 = require("./command");
class Log extends command_1.Command {
    constructor() {
        super("svn.log", { repository: true });
    }
    execute(repository) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const resource = uri_1.toSvnUri(vscode_1.Uri.file(repository.workspaceRoot), types_1.SvnUriAction.LOG);
                const uri = resource.with({
                    path: path.join(resource.path, "svn.log") // change document title
                });
                yield vscode_1.commands.executeCommand("vscode.open", uri);
            }
            catch (error) {
                console.error(error);
                vscode_1.window.showErrorMessage("Unable to log");
            }
        });
    }
}
exports.Log = Log;
//# sourceMappingURL=log.js.map