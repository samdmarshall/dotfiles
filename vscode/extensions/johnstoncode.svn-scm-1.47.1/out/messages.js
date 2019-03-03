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
function noChangesToCommit() {
    return vscode_1.window.showInformationMessage("There are no changes to commit.");
}
exports.noChangesToCommit = noChangesToCommit;
function inputCommitMessage(message, promptNew = true) {
    return __awaiter(this, void 0, void 0, function* () {
        if (promptNew) {
            message = yield vscode_1.window.showInputBox({
                value: message,
                placeHolder: "Commit message",
                prompt: "Please enter a commit message",
                ignoreFocusOut: true
            });
        }
        if (message === "") {
            const allowEmpty = yield vscode_1.window.showWarningMessage("Do you really want to commit an empty message?", { modal: true }, "Yes");
            if (allowEmpty === "Yes") {
                return "";
            }
            else {
                return undefined;
            }
        }
        return message;
    });
}
exports.inputCommitMessage = inputCommitMessage;
//# sourceMappingURL=messages.js.map