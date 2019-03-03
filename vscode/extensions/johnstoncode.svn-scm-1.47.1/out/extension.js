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
const commands_1 = require("./commands");
const types_1 = require("./common/types");
const svnDecorations_1 = require("./decorations/svnDecorations");
const configuration_1 = require("./helpers/configuration");
const itemLogProvider_1 = require("./historyView/itemLogProvider");
const repoLogProvider_1 = require("./historyView/repoLogProvider");
const model_1 = require("./model");
const svn_1 = require("./svn");
const svnContentProvider_1 = require("./svnContentProvider");
const svnFinder_1 = require("./svnFinder");
const svnProvider_1 = require("./treeView/dataProviders/svnProvider");
const util_1 = require("./util");
function init(context, outputChannel, disposables) {
    return __awaiter(this, void 0, void 0, function* () {
        vscode_1.commands.executeCommand("setContext", "svnOpenRepositoryCount", "0");
        const pathHint = configuration_1.configuration.get("path");
        const svnFinder = new svnFinder_1.SvnFinder();
        const info = yield svnFinder.findSvn(pathHint);
        const svn = new svn_1.Svn({ svnPath: info.path, version: info.version });
        const model = yield new model_1.Model(svn, types_1.ConstructorPolicy.Async);
        const contentProvider = new svnContentProvider_1.SvnContentProvider(model);
        commands_1.registerCommands(model, disposables);
        disposables.push(model, contentProvider);
        const svnProvider = new svnProvider_1.default(model);
        vscode_1.window.registerTreeDataProvider("svn", svnProvider);
        const repoLogProvider = new repoLogProvider_1.RepoLogProvider(model);
        disposables.push(repoLogProvider);
        vscode_1.window.registerTreeDataProvider("repolog", repoLogProvider);
        const itemLogProvider = new itemLogProvider_1.ItemLogProvider(model);
        disposables.push(itemLogProvider);
        vscode_1.window.registerTreeDataProvider("itemlog", itemLogProvider);
        // First, check the vscode has support to DecorationProvider
        if (util_1.hasSupportToDecorationProvider()) {
            const decoration = new svnDecorations_1.default(model);
            disposables.push(decoration);
        }
        const onRepository = () => vscode_1.commands.executeCommand("setContext", "svnOpenRepositoryCount", `${model.repositories.length}`);
        model.onDidOpenRepository(onRepository, null, disposables);
        model.onDidCloseRepository(onRepository, null, disposables);
        onRepository();
        vscode_1.commands.executeCommand("setContext", "svnHasSupportToRegisterDiffCommand", util_1.hasSupportToRegisterDiffCommand() ? "1" : "0");
        outputChannel.appendLine(`Using svn "${info.version}" from "${info.path}"`);
        const onOutput = (str) => outputChannel.append(str);
        svn.onOutput.addListener("log", onOutput);
        disposables.push(util_1.toDisposable(() => svn.onOutput.removeListener("log", onOutput)));
    });
}
function _activate(context, disposables) {
    return __awaiter(this, void 0, void 0, function* () {
        const outputChannel = vscode_1.window.createOutputChannel("Svn");
        vscode_1.commands.registerCommand("svn.showOutput", () => outputChannel.show());
        disposables.push(outputChannel);
        const showOutput = configuration_1.configuration.get("showOutput");
        if (showOutput) {
            outputChannel.show();
        }
        const tryInit = () => __awaiter(this, void 0, void 0, function* () {
            try {
                yield init(context, outputChannel, disposables);
            }
            catch (err) {
                if (!/Svn installation not found/.test(err.message || "")) {
                    throw err;
                }
                const shouldIgnore = configuration_1.configuration.get("ignoreMissingSvnWarning") === true;
                if (shouldIgnore) {
                    return;
                }
                console.warn(err.message);
                outputChannel.appendLine(err.message);
                outputChannel.show();
                const findSvnExecutable = "Find SVN executable";
                const download = "Download SVN";
                const neverShowAgain = "Don't Show Again";
                const choice = yield vscode_1.window.showWarningMessage("SVN not found. Install it or configure it using the 'svn.path' setting.", findSvnExecutable, download, neverShowAgain);
                if (choice === findSvnExecutable) {
                    let filters;
                    // For windows, limit to executable files
                    if (path.sep === "\\") {
                        filters = {
                            svn: ["exe", "bat"]
                        };
                    }
                    const executable = yield vscode_1.window.showOpenDialog({
                        canSelectFiles: true,
                        canSelectFolders: false,
                        canSelectMany: false,
                        filters
                    });
                    if (executable && executable[0]) {
                        const file = executable[0].fsPath;
                        outputChannel.appendLine(`Updated "svn.path" with "${file}"`);
                        yield configuration_1.configuration.update("path", file);
                        // Try Re-init after select the executable
                        yield tryInit();
                    }
                }
                else if (choice === download) {
                    vscode_1.commands.executeCommand("vscode.open", vscode_1.Uri.parse("https://subversion.apache.org/packages.html"));
                }
                else if (choice === neverShowAgain) {
                    yield configuration_1.configuration.update("ignoreMissingSvnWarning", true);
                }
            }
        });
        yield tryInit();
    });
}
function activate(context) {
    return __awaiter(this, void 0, void 0, function* () {
        const disposables = [];
        context.subscriptions.push(new vscode_1.Disposable(() => vscode_1.Disposable.from(...disposables).dispose()));
        yield _activate(context, disposables).catch(err => console.error(err));
    });
}
exports.activate = activate;
// this method is called when your extension is deactivated
/* tslint:disable:no-empty */
function deactivate() { }
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map