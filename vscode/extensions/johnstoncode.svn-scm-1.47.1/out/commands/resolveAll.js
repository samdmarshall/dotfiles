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
const conflictItems_1 = require("../conflictItems");
const command_1 = require("./command");
class ResolveAll extends command_1.Command {
    constructor() {
        super("svn.resolveAll", { repository: true });
    }
    execute(repository) {
        return __awaiter(this, void 0, void 0, function* () {
            const conflicts = repository.conflicts.resourceStates;
            if (!conflicts.length) {
                vscode_1.window.showInformationMessage("No Conflicts");
            }
            for (const conflict of conflicts) {
                const placeHolder = `Select conflict option for ${conflict.resourceUri.path}`;
                const picks = conflictItems_1.getConflictPickOptions();
                const choice = yield vscode_1.window.showQuickPick(picks, { placeHolder });
                if (!choice) {
                    return;
                }
                try {
                    const response = yield repository.resolve([conflict.resourceUri.path], choice.label);
                    vscode_1.window.showInformationMessage(response);
                }
                catch (error) {
                    vscode_1.window.showErrorMessage(error.stderr);
                }
            }
        });
    }
}
exports.ResolveAll = ResolveAll;
//# sourceMappingURL=resolveAll.js.map