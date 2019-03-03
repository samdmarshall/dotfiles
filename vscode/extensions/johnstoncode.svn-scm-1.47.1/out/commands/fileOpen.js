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
class FileOpen extends command_1.Command {
    constructor() {
        super("svn.fileOpen");
    }
    execute(resourceUri) {
        return __awaiter(this, void 0, void 0, function* () {
            yield vscode_1.commands.executeCommand("vscode.open", resourceUri);
        });
    }
}
exports.FileOpen = FileOpen;
//# sourceMappingURL=fileOpen.js.map