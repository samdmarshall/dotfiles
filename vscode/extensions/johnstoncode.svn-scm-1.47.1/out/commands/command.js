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
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const types_1 = require("../common/types");
const ignoreitems_1 = require("../ignoreitems");
const lineChanges_1 = require("../lineChanges");
const resource_1 = require("../resource");
const incomingChangeNode_1 = require("../treeView/nodes/incomingChangeNode");
const uri_1 = require("../uri");
const util_1 = require("../util");
class Command {
    constructor(commandName, options = {}) {
        if (options.repository) {
            const command = this.createRepositoryCommand(this.execute);
            this._disposable = vscode_1.commands.registerCommand(commandName, command);
            return;
        }
        if (options.diff && util_1.hasSupportToRegisterDiffCommand()) {
            this._disposable = vscode_1.commands.registerDiffInformationCommand(commandName, (...args) => this.execute(...args));
            return;
        }
        if (!options.repository) {
            this._disposable = vscode_1.commands.registerCommand(commandName, (...args) => this.execute(...args));
            return;
        }
    }
    dispose() {
        this._disposable && this._disposable.dispose(); // tslint:disable-line
    }
    createRepositoryCommand(method) {
        const result = (...args) => __awaiter(this, void 0, void 0, function* () {
            const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
            let result;
            const repository = model.getRepository(args[0]);
            let repositoryPromise;
            if (repository) {
                repositoryPromise = Promise.resolve(repository);
            }
            else if (model.repositories.length === 1) {
                repositoryPromise = Promise.resolve(model.repositories[0]);
            }
            else {
                repositoryPromise = model.pickRepository();
            }
            result = repositoryPromise.then(repository => {
                if (!repository) {
                    return Promise.resolve();
                }
                return Promise.resolve(method.apply(this, [repository, ...args]));
            });
            return result.catch(err => {
                console.error(err);
            });
        });
        return result;
    }
    getResourceStates(resourceStates) {
        return __awaiter(this, void 0, void 0, function* () {
            if (resourceStates.length === 0 ||
                !(resourceStates[0].resourceUri instanceof vscode_1.Uri)) {
                const resource = yield this.getSCMResource();
                if (!resource) {
                    return [];
                }
                resourceStates = [resource];
            }
            return resourceStates.filter(s => s instanceof resource_1.Resource);
        });
    }
    runByRepository(arg, fn) {
        return __awaiter(this, void 0, void 0, function* () {
            const resources = arg instanceof vscode_1.Uri ? [arg] : arg;
            const isSingleResource = arg instanceof vscode_1.Uri;
            const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
            const groups = [];
            for (const resource of resources) {
                const repository = model.getRepository(resource);
                if (!repository) {
                    console.warn("Could not find Svn repository for ", resource);
                    continue;
                }
                const tuple = groups.filter(p => p.repository === repository)[0];
                if (tuple) {
                    tuple.resources.push(resource);
                }
                else {
                    groups.push({ repository, resources: [resource] });
                }
            }
            const promises = groups.map(({ repository, resources }) => fn(repository, isSingleResource ? resources[0] : resources));
            return Promise.all(promises);
        });
    }
    getSCMResource(uri) {
        return __awaiter(this, void 0, void 0, function* () {
            uri = uri
                ? uri
                : vscode_1.window.activeTextEditor && vscode_1.window.activeTextEditor.document.uri;
            if (!uri) {
                return undefined;
            }
            if (uri.scheme === "svn") {
                const { fsPath } = uri_1.fromSvnUri(uri);
                uri = vscode_1.Uri.file(fsPath);
            }
            if (uri.scheme === "file") {
                const model = (yield vscode_1.commands.executeCommand("svn.getModel", ""));
                const repository = model.getRepository(uri);
                if (!repository) {
                    return undefined;
                }
                return repository.getResourceFromFile(uri);
            }
        });
    }
    _openResource(resource, against, preview, preserveFocus, preserveSelection) {
        return __awaiter(this, void 0, void 0, function* () {
            let left = this.getLeftResource(resource, against);
            let right = this.getRightResource(resource, against);
            const title = this.getTitle(resource, against);
            if (resource.remote && left) {
                [left, right] = [right, left];
            }
            if (!right) {
                // TODO
                console.error("oh no");
                return;
            }
            if (fs.existsSync(right.fsPath) &&
                fs.statSync(right.fsPath).isDirectory()) {
                return;
            }
            const opts = {
                preserveFocus,
                preview,
                viewColumn: vscode_1.ViewColumn.Active
            };
            const activeTextEditor = vscode_1.window.activeTextEditor;
            if (preserveSelection &&
                activeTextEditor &&
                activeTextEditor.document.uri.toString() === right.toString()) {
                opts.selection = activeTextEditor.selection;
            }
            if (!left) {
                return vscode_1.commands.executeCommand("vscode.open", right, opts);
            }
            return vscode_1.commands.executeCommand("vscode.diff", left, right, title, opts);
        });
    }
    getLeftResource(resource, against = "") {
        if (resource.remote) {
            if (resource.type !== types_1.Status.DELETED) {
                return uri_1.toSvnUri(resource.resourceUri, types_1.SvnUriAction.SHOW, {
                    ref: against
                });
            }
            return;
        }
        if (resource.type === types_1.Status.ADDED && resource.renameResourceUri) {
            return uri_1.toSvnUri(resource.renameResourceUri, types_1.SvnUriAction.SHOW, {
                ref: against
            });
        }
        // Show file if has conflicts marks
        if (resource.type === types_1.Status.CONFLICTED &&
            fs.existsSync(resource.resourceUri.fsPath)) {
            const text = fs.readFileSync(resource.resourceUri.fsPath, {
                encoding: "utf8"
            });
            // Check for lines begin with "<<<<<<", "=======", ">>>>>>>"
            if (/^<{7}[^]+^={7}[^]+^>{7}/m.test(text)) {
                return undefined;
            }
        }
        switch (resource.type) {
            case types_1.Status.CONFLICTED:
            case types_1.Status.MODIFIED:
            case types_1.Status.REPLACED:
                return uri_1.toSvnUri(resource.resourceUri, types_1.SvnUriAction.SHOW, {
                    ref: against
                });
        }
    }
    getRightResource(resource, against = "") {
        if (resource.remote) {
            if (resource.type !== types_1.Status.ADDED) {
                return resource.resourceUri;
            }
            return;
        }
        switch (resource.type) {
            case types_1.Status.ADDED:
            case types_1.Status.CONFLICTED:
            case types_1.Status.IGNORED:
            case types_1.Status.MODIFIED:
            case types_1.Status.UNVERSIONED:
            case types_1.Status.REPLACED:
                return resource.resourceUri;
            case types_1.Status.DELETED:
            case types_1.Status.MISSING:
                return uri_1.toSvnUri(resource.resourceUri, types_1.SvnUriAction.SHOW, {
                    ref: against
                });
        }
    }
    getTitle(resource, against) {
        if (resource.type === types_1.Status.ADDED && resource.renameResourceUri) {
            const basename = path.basename(resource.renameResourceUri.fsPath);
            const newname = path.relative(path.dirname(resource.renameResourceUri.fsPath), resource.resourceUri.fsPath);
            if (against) {
                return `${basename} -> ${newname} (${against})`;
            }
            return `${basename} -> ${newname}`;
        }
        const basename = path.basename(resource.resourceUri.fsPath);
        if (against) {
            return `${basename} (${against})`;
        }
        return "";
    }
    openChange(arg, against, resourceStates) {
        return __awaiter(this, void 0, void 0, function* () {
            const preserveFocus = arg instanceof resource_1.Resource;
            const preserveSelection = arg instanceof vscode_1.Uri || !arg;
            let resources;
            if (arg instanceof vscode_1.Uri) {
                const resource = yield this.getSCMResource(arg);
                if (resource !== undefined) {
                    resources = [resource];
                }
            }
            else if (arg instanceof incomingChangeNode_1.default) {
                const resource = new resource_1.Resource(arg.uri, arg.type, undefined, arg.props, true);
                resources = [resource];
            }
            else {
                let resource;
                if (arg instanceof resource_1.Resource) {
                    resource = arg;
                }
                else {
                    resource = yield this.getSCMResource();
                }
                if (resource) {
                    resources = [...resourceStates, resource];
                }
            }
            if (!resources) {
                return;
            }
            const preview = resources.length === 1 ? undefined : false;
            for (const resource of resources) {
                yield this._openResource(resource, against, preview, preserveFocus, preserveSelection);
            }
        });
    }
    showDiffPath(repository, content) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const tempFile = path.join(repository.root, ".svn", "tmp", "svn.patch");
                if (fs.existsSync(tempFile)) {
                    fs.unlinkSync(tempFile);
                }
                const uri = vscode_1.Uri.file(tempFile).with({
                    scheme: "untitled"
                });
                const document = yield vscode_1.workspace.openTextDocument(uri);
                const textEditor = yield vscode_1.window.showTextDocument(document);
                yield textEditor.edit(e => {
                    // if is opened, clear content
                    e.delete(new vscode_1.Range(new vscode_1.Position(0, 0), new vscode_1.Position(Number.MAX_SAFE_INTEGER, 0)));
                    e.insert(new vscode_1.Position(0, 0), content);
                });
            }
            catch (error) {
                console.error(error);
                vscode_1.window.showErrorMessage("Unable to patch");
            }
        });
    }
    _revertChanges(textEditor, changes) {
        return __awaiter(this, void 0, void 0, function* () {
            const modifiedDocument = textEditor.document;
            const modifiedUri = modifiedDocument.uri;
            if (modifiedUri.scheme !== "file") {
                return;
            }
            const originalUri = uri_1.toSvnUri(modifiedUri, types_1.SvnUriAction.SHOW, {
                ref: "BASE"
            });
            const originalDocument = yield vscode_1.workspace.openTextDocument(originalUri);
            const result = lineChanges_1.applyLineChanges(originalDocument, modifiedDocument, changes);
            const edit = new vscode_1.WorkspaceEdit();
            edit.replace(modifiedUri, new vscode_1.Range(new vscode_1.Position(0, 0), modifiedDocument.lineAt(modifiedDocument.lineCount - 1).range.end), result);
            vscode_1.workspace.applyEdit(edit);
            yield modifiedDocument.save();
        });
    }
    addToIgnore(uris) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.runByRepository(uris, (repository, resources) => __awaiter(this, void 0, void 0, function* () {
                if (!repository) {
                    return;
                }
                try {
                    const ignored = yield ignoreitems_1.inputIgnoreList(repository, resources);
                    if (ignored) {
                        vscode_1.window.showInformationMessage(`File(s) is now being ignored`);
                    }
                }
                catch (error) {
                    console.log(error);
                    vscode_1.window.showErrorMessage("Unable to set property ignore");
                }
            }));
        });
    }
}
exports.Command = Command;
//# sourceMappingURL=command.js.map