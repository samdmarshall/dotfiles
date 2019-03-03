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
const branch_1 = require("../helpers/branch");
const command_1 = require("./command");
class SwitchBranch extends command_1.Command {
    constructor() {
        super("svn.switchBranch", { repository: true });
    }
    execute(repository) {
        return __awaiter(this, void 0, void 0, function* () {
            const branch = yield branch_1.selectBranch(repository, true);
            if (!branch) {
                return;
            }
            try {
                if (branch.isNew) {
                    const commitMessage = yield vscode_1.window.showInputBox({
                        value: `Created new branch ${branch.name}`,
                        prompt: `Commit message for create branch ${branch.name}`
                    });
                    // If press ESC on commit message
                    if (commitMessage === undefined) {
                        return;
                    }
                    yield repository.newBranch(branch.path, commitMessage);
                }
                else {
                    try {
                        yield repository.switchBranch(branch.path);
                    }
                    catch (error) {
                        if (typeof error === "object" &&
                            error.hasOwnProperty("stderrFormated") &&
                            error.stderrFormated.includes("ignore-ancestry")) {
                            const answer = yield vscode_1.window.showErrorMessage("Seems like these branches don't have a common ancestor. " +
                                " Do you want to retry with '--ignore-ancestry' option?", "Yes", "No");
                            if (answer === "Yes") {
                                yield repository.switchBranch(branch.path, true);
                            }
                        }
                        else {
                            throw error;
                        }
                    }
                }
            }
            catch (error) {
                console.log(error);
                if (branch.isNew) {
                    vscode_1.window.showErrorMessage("Unable to create new branch");
                }
                else {
                    vscode_1.window.showErrorMessage("Unable to switch branch");
                }
            }
        });
    }
}
exports.SwitchBranch = SwitchBranch;
//# sourceMappingURL=switchBranch.js.map