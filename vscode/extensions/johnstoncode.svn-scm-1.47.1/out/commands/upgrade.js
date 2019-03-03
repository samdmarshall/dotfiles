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
const configuration_1 = require("../helpers/configuration");
const util_1 = require("../util");
const command_1 = require("./command");
class Upgrade extends command_1.Command {
    constructor() {
        super("svn.upgrade");
    }
    execute(folderPath) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!folderPath) {
                return;
            }
            if (configuration_1.configuration.get("ignoreWorkingCopyIsTooOld", false)) {
                return;
            }
            folderPath = util_1.fixPathSeparator(folderPath);
            const yes = "Yes";
            const no = "No";
            const neverShowAgain = "Don't Show Again";
            const choice = yield vscode_1.window.showWarningMessage("You want upgrade the working copy (svn upgrade)?", yes, no, neverShowAgain);
            const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
            if (choice === yes) {
                const upgraded = yield model.upgradeWorkingCopy(folderPath);
                if (upgraded) {
                    vscode_1.window.showInformationMessage(`Working copy "${folderPath}" upgraded`);
                    model.tryOpenRepository(folderPath);
                }
                else {
                    vscode_1.window.showErrorMessage(`Error on upgrading working copy "${folderPath}". See log for more detail`);
                }
            }
            else if (choice === neverShowAgain) {
                return configuration_1.configuration.update("ignoreWorkingCopyIsTooOld", true);
            }
            return;
        });
    }
}
exports.Upgrade = Upgrade;
//# sourceMappingURL=upgrade.js.map