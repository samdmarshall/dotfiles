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
class RevertSelectedRanges extends command_1.Command {
    constructor() {
        super("svn.revertSelectedRanges", { diff: true });
    }
    execute(changes) {
        return __awaiter(this, void 0, void 0, function* () {
            const textEditor = vscode_1.window.activeTextEditor;
            if (!textEditor) {
                return;
            }
            const modifiedDocument = textEditor.document;
            const selections = textEditor.selections;
            const selectedChanges = changes.filter(change => {
                const modifiedRange = change.modifiedEndLineNumber === 0
                    ? new vscode_1.Range(modifiedDocument.lineAt(change.modifiedStartLineNumber - 1).range.end, modifiedDocument.lineAt(change.modifiedStartLineNumber).range.start)
                    : new vscode_1.Range(modifiedDocument.lineAt(change.modifiedStartLineNumber - 1).range.start, modifiedDocument.lineAt(change.modifiedEndLineNumber - 1).range.end);
                return selections.every(selection => !selection.intersection(modifiedRange));
            });
            if (selectedChanges.length === changes.length) {
                return;
            }
            yield this._revertChanges(textEditor, selectedChanges);
        });
    }
}
exports.RevertSelectedRanges = RevertSelectedRanges;
//# sourceMappingURL=revertSelectedRanges.js.map