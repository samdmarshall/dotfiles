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
const changelistItems_1 = require("../changelistItems");
const resource_1 = require("../resource");
const util_1 = require("../util");
const command_1 = require("./command");
class ChangeList extends command_1.Command {
    constructor() {
        super("svn.changelist");
    }
    execute(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            let uris;
            if (args[0] instanceof resource_1.Resource) {
                uris = args.map(resource => resource.resourceUri);
            }
            else if (args[0] instanceof vscode_1.Uri) {
                uris = args[1];
            }
            else {
                console.error("Unhandled type for changelist command");
                return;
            }
            const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
            const promiseArray = uris.map((uri) => __awaiter(this, void 0, void 0, function* () { return yield model.getRepositoryFromUri(uri); }));
            let repositories = yield Promise.all(promiseArray);
            repositories = repositories.filter(repository => repository);
            if (repositories.length === 0) {
                vscode_1.window.showErrorMessage("Files are not under version control and cannot be added to a change list");
                return;
            }
            const uniqueRepositories = Array.from(new Set(repositories));
            if (uniqueRepositories.length !== 1) {
                vscode_1.window.showErrorMessage("Unable to add files from different repositories to change list");
                return;
            }
            if (repositories.length !== uris.length) {
                vscode_1.window.showErrorMessage("Some Files are not under version control and cannot be added to a change list");
                return;
            }
            const repository = repositories[0];
            if (!repository) {
                return;
            }
            const paths = uris.map(uri => uri.fsPath);
            let canRemove = false;
            repository.changelists.forEach((group, changelist) => {
                if (group.resourceStates.some(state => {
                    return paths.some(path => {
                        return (util_1.normalizePath(path) === util_1.normalizePath(state.resourceUri.path));
                    });
                })) {
                    canRemove = true;
                    return false;
                }
            });
            const changelistName = yield changelistItems_1.inputSwitchChangelist(repository, canRemove);
            if (!changelistName && changelistName !== false) {
                return;
            }
            if (changelistName === false) {
                try {
                    yield repository.removeChangelist(paths);
                }
                catch (error) {
                    console.log(error);
                    vscode_1.window.showErrorMessage(`Unable to remove file "${paths.join(",")}" from changelist`);
                }
            }
            else {
                try {
                    yield repository.addChangelist(paths, changelistName);
                    vscode_1.window.showInformationMessage(`Added files "${paths.join(",")}" to changelist "${changelistName}"`);
                }
                catch (error) {
                    console.log(error);
                    vscode_1.window.showErrorMessage(`Unable to add file "${paths.join(",")}" to changelist "${changelistName}"`);
                }
            }
        });
    }
}
exports.ChangeList = ChangeList;
//# sourceMappingURL=changeList.js.map