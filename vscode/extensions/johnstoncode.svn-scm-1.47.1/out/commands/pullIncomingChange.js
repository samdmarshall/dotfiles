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
const incomingChangeNode_1 = require("../treeView/nodes/incomingChangeNode");
const command_1 = require("./command");
class PullIncommingChange extends command_1.Command {
    constructor() {
        super("svn.treeview.pullIncomingChange");
    }
    // TODO: clean this up
    execute(...changes) {
        return __awaiter(this, void 0, void 0, function* () {
            const showUpdateMessage = configuration_1.configuration.get("showUpdateMessage", true);
            if (changes[0] instanceof incomingChangeNode_1.default) {
                try {
                    const incomingChange = changes[0];
                    const result = yield incomingChange.repository.pullIncomingChange(incomingChange.uri.fsPath);
                    if (showUpdateMessage) {
                        vscode_1.window.showInformationMessage(result);
                    }
                }
                catch (error) {
                    console.error(error);
                    vscode_1.window.showErrorMessage("Unable to update");
                }
                return;
            }
            const uris = changes.map(change => change.resourceUri);
            yield this.runByRepository(uris, (repository, resources) => __awaiter(this, void 0, void 0, function* () {
                if (!repository) {
                    return;
                }
                const files = resources.map(resource => resource.fsPath);
                files.forEach((path) => __awaiter(this, void 0, void 0, function* () {
                    const result = yield repository.pullIncomingChange(path);
                    if (showUpdateMessage) {
                        vscode_1.window.showInformationMessage(result);
                    }
                }));
            }));
        });
    }
}
exports.PullIncommingChange = PullIncommingChange;
//# sourceMappingURL=pullIncomingChange.js.map