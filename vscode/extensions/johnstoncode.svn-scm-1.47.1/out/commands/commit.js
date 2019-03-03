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
const types_1 = require("../common/types");
const messages_1 = require("../messages");
const resource_1 = require("../resource");
const command_1 = require("./command");
class Commit extends command_1.Command {
    constructor() {
        super("svn.commit");
    }
    execute(...resources) {
        return __awaiter(this, void 0, void 0, function* () {
            if (resources.length === 0 || !(resources[0].resourceUri instanceof vscode_1.Uri)) {
                const resource = yield this.getSCMResource();
                if (!resource) {
                    return;
                }
                resources = [resource];
            }
            const selection = resources.filter(s => s instanceof resource_1.Resource);
            const uris = selection.map(resource => resource.resourceUri);
            selection.forEach(resource => {
                if (resource.type === types_1.Status.ADDED && resource.renameResourceUri) {
                    uris.push(resource.renameResourceUri);
                }
            });
            yield this.runByRepository(uris, (repository, resources) => __awaiter(this, void 0, void 0, function* () {
                if (!repository) {
                    return;
                }
                const paths = resources.map(resource => resource.fsPath);
                for (const resource of resources) {
                    let dir = path.dirname(resource.fsPath);
                    let parent = repository.getResourceFromFile(dir);
                    while (parent) {
                        if (parent.type === types_1.Status.ADDED) {
                            paths.push(dir);
                        }
                        dir = path.dirname(dir);
                        parent = repository.getResourceFromFile(dir);
                    }
                }
                try {
                    const message = yield messages_1.inputCommitMessage(repository.inputBox.value);
                    if (message === undefined) {
                        return;
                    }
                    repository.inputBox.value = message;
                    const result = yield repository.commitFiles(message, paths);
                    vscode_1.window.showInformationMessage(result);
                    repository.inputBox.value = "";
                }
                catch (error) {
                    console.error(error);
                    vscode_1.window.showErrorMessage(error.stderrFormated);
                }
            }));
        });
    }
}
exports.Commit = Commit;
//# sourceMappingURL=commit.js.map