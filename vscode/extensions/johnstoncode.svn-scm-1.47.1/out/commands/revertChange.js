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
const vscode_1 = require("vscode");
const command_1 = require("./command");
class RevertChange extends command_1.Command {
    constructor() {
        super("svn.revertChange");
    }
    execute(uri, changes, index) {
        return __awaiter(this, void 0, void 0, function* () {
            const textEditor = vscode_1.window.visibleTextEditors.filter(e => e.document.uri.toString() === uri.toString())[0];
            if (!textEditor) {
                return;
            }
            yield this._revertChanges(textEditor, [
                ...changes.slice(0, index),
                ...changes.slice(index + 1)
            ]);
        });
    }
}
exports.RevertChange = RevertChange;
//# sourceMappingURL=revertChange.js.map