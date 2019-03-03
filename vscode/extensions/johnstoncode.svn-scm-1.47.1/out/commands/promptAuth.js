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
class PromptAuth extends command_1.Command {
    constructor() {
        super("svn.promptAuth");
    }
    execute(prevUsername, prevPassword) {
        return __awaiter(this, void 0, void 0, function* () {
            const username = yield vscode_1.window.showInputBox({
                placeHolder: "Svn repository username",
                prompt: "Please enter your username",
                value: prevUsername
            });
            if (username === undefined) {
                return;
            }
            const password = yield vscode_1.window.showInputBox({
                placeHolder: "Svn repository password",
                prompt: "Please enter your password",
                value: prevPassword,
                password: true
            });
            if (password === undefined) {
                return;
            }
            const auth = {
                username,
                password
            };
            return auth;
        });
    }
}
exports.PromptAuth = PromptAuth;
//# sourceMappingURL=promptAuth.js.map