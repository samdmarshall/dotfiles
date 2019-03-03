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
const command_1 = require("./command");
class PromptRemove extends command_1.Command {
    constructor() {
        super("svn.promptRemove", { repository: true });
    }
    execute(repository, ...uris) {
        return __awaiter(this, void 0, void 0, function* () {
            const files = uris.map(uri => uri.fsPath);
            const relativeList = files
                .map(file => repository.repository.removeAbsolutePath(file))
                .sort();
            const ignoreText = "Add to ignored list";
            const resp = yield vscode_1.window.showInformationMessage(`The file(s) "${relativeList.join(", ")}" are removed from disk.\nWould you like remove from SVN?`, { modal: true }, "Yes", ignoreText, "No");
            if (resp === "Yes") {
                yield repository.removeFiles(files, false);
            }
            else if (resp === ignoreText) {
                let ignoreList = configuration_1.configuration.get("delete.ignoredRulesForDeletedFiles", []);
                ignoreList.push(...relativeList);
                ignoreList = [...new Set(ignoreList)]; // Remove duplicates
                configuration_1.configuration.update("delete.ignoredRulesForDeletedFiles", ignoreList);
            }
        });
    }
}
exports.PromptRemove = PromptRemove;
//# sourceMappingURL=promptRemove.js.map