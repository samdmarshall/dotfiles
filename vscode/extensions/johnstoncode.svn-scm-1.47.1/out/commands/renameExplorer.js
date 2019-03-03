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
const util_1 = require("../util");
const command_1 = require("./command");
class RenameExplorer extends command_1.Command {
    constructor() {
        super("svn.renameExplorer", { repository: true });
    }
    execute(repository, mainUri, allUris) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!mainUri) {
                return;
            }
            const oldName = mainUri.fsPath;
            return this.rename(repository, oldName);
        });
    }
    rename(repository, oldFile, newName) {
        return __awaiter(this, void 0, void 0, function* () {
            oldFile = util_1.fixPathSeparator(oldFile);
            if (!newName) {
                const root = util_1.fixPathSeparator(repository.workspaceRoot);
                const oldName = path.relative(root, oldFile);
                newName = yield vscode_1.window.showInputBox({
                    value: path.basename(oldFile),
                    prompt: `New name name for ${oldName}`
                });
            }
            if (!newName) {
                return;
            }
            const basepath = path.dirname(oldFile);
            newName = path.join(basepath, newName);
            yield repository.rename(oldFile, newName);
        });
    }
}
exports.RenameExplorer = RenameExplorer;
//# sourceMappingURL=renameExplorer.js.map