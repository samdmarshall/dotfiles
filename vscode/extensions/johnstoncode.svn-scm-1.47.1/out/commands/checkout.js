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
const os = require("os");
const path = require("path");
const vscode_1 = require("vscode");
const branch_1 = require("../helpers/branch");
const configuration_1 = require("../helpers/configuration");
const svn_1 = require("../svn");
const command_1 = require("./command");
class Checkout extends command_1.Command {
    constructor() {
        super("svn.checkout");
    }
    execute(url) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!url) {
                url = yield vscode_1.window.showInputBox({
                    prompt: "Repository URL",
                    ignoreFocusOut: true
                });
            }
            if (!url) {
                return;
            }
            let defaultCheckoutDirectory = configuration_1.configuration.get("defaultCheckoutDirectory") || os.homedir();
            defaultCheckoutDirectory = defaultCheckoutDirectory.replace(/^~/, os.homedir());
            const uris = yield vscode_1.window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                defaultUri: vscode_1.Uri.file(defaultCheckoutDirectory),
                openLabel: "Select Repository Location"
            });
            if (!uris || uris.length === 0) {
                return;
            }
            const uri = uris[0];
            const parentPath = uri.fsPath;
            let folderName;
            // Get folder name from branch
            const branch = branch_1.getBranchName(url);
            if (branch) {
                const baseUrl = url.replace(/\//g, "/").replace(branch.path, "");
                folderName = path.basename(baseUrl);
            }
            folderName = yield vscode_1.window.showInputBox({
                prompt: "Folder name",
                value: folderName,
                ignoreFocusOut: true
            });
            if (!folderName) {
                return;
            }
            const repositoryPath = path.join(parentPath, folderName);
            // Use Notification location if supported
            let location = vscode_1.ProgressLocation.Window;
            if (vscode_1.ProgressLocation.Notification) {
                location = vscode_1.ProgressLocation.Notification;
            }
            const progressOptions = {
                location,
                title: `Checkout svn repository '${url}'...`,
                cancellable: true
            };
            let attempt = 0;
            const opt = {};
            while (true) {
                attempt++;
                try {
                    yield vscode_1.window.withProgress(progressOptions, () => __awaiter(this, void 0, void 0, function* () {
                        const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
                        const args = ["checkout", url, repositoryPath];
                        yield model.svn.exec(parentPath, args, opt);
                    }));
                    break;
                }
                catch (err) {
                    if (err.svnErrorCode === svn_1.svnErrorCodes.AuthorizationFailed &&
                        attempt <= 3) {
                        const auth = (yield vscode_1.commands.executeCommand("svn.promptAuth", opt.username));
                        if (auth) {
                            opt.username = auth.username;
                            opt.password = auth.password;
                            continue;
                        }
                    }
                    throw err;
                }
            }
            const choices = [];
            let message = "Would you like to open the checked out repository?";
            const open = "Open Repository";
            choices.push(open);
            const addToWorkspace = "Add to Workspace";
            if (vscode_1.workspace.workspaceFolders &&
                vscode_1.workspace.updateWorkspaceFolders // For VSCode >= 1.21
            ) {
                message =
                    "Would you like to open the checked out repository, or add it to the current workspace?";
                choices.push(addToWorkspace);
            }
            const result = yield vscode_1.window.showInformationMessage(message, ...choices);
            const openFolder = result === open;
            if (openFolder) {
                vscode_1.commands.executeCommand("vscode.openFolder", vscode_1.Uri.file(repositoryPath));
            }
            else if (result === addToWorkspace) {
                // For VSCode >= 1.21
                vscode_1.workspace.updateWorkspaceFolders(vscode_1.workspace.workspaceFolders.length, 0, {
                    uri: vscode_1.Uri.file(repositoryPath)
                });
            }
        });
    }
}
exports.Checkout = Checkout;
//# sourceMappingURL=checkout.js.map