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
const util_1 = require("../util");
const command_1 = require("./command");
class DeleteUnversioned extends command_1.Command {
    constructor() {
        super("svn.deleteUnversioned");
    }
    execute(...resourceStates) {
        return __awaiter(this, void 0, void 0, function* () {
            const selection = yield this.getResourceStates(resourceStates);
            if (selection.length === 0) {
                return;
            }
            const uris = selection.map(resource => resource.resourceUri);
            const answer = yield vscode_1.window.showWarningMessage("Would you like to delete selected files?", { modal: true }, "Yes", "No");
            if (answer === "Yes") {
                for (const uri of uris) {
                    const fsPath = uri.fsPath;
                    if (!fs.existsSync(fsPath)) {
                        continue;
                    }
                    const stat = fs.lstatSync(fsPath);
                    if (stat.isDirectory()) {
                        util_1.deleteDirectory(fsPath);
                    }
                    else {
                        fs.unlinkSync(fsPath);
                    }
                }
            }
        });
    }
}
exports.DeleteUnversioned = DeleteUnversioned;
//# sourceMappingURL=deleteUnversioned.js.map