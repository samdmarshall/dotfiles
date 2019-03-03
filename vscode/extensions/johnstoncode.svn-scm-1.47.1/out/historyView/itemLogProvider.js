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
const tempFiles_1 = require("../tempFiles");
const util_1 = require("../util");
const common_1 = require("./common");
class ItemLogProvider {
    constructor(model) {
        this.model = model;
        this._onDidChangeTreeData = new vscode_1.EventEmitter();
        this.onDidChangeTreeData = this
            ._onDidChangeTreeData.event;
        this._dispose = [];
        vscode_1.window.onDidChangeActiveTextEditor(this.editorChanged, this);
        this._dispose.push(vscode_1.commands.registerCommand("svn.itemlog.copymsg", (item) => __awaiter(this, void 0, void 0, function* () { return common_1.copyCommitToClipboard("msg", item); })));
        this._dispose.push(vscode_1.commands.registerCommand("svn.itemlog.openFileRemote", this.openFileRemoteCmd, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.itemlog.openDiff", this.openDiffCmd, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.itemlog.openDiffBase", this.openDiffBaseCmd, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.itemlog.refresh", this.refresh, this));
        this.refresh();
    }
    dispose() {
        util_1.dispose(this._dispose);
    }
    openFileRemoteCmd(element) {
        return __awaiter(this, void 0, void 0, function* () {
            const commit = element.data;
            const item = util_1.unwrap(this.currentItem);
            return common_1.openFileRemote(item.repo, item.svnTarget, commit.revision);
        });
    }
    openDiffBaseCmd(element) {
        return __awaiter(this, void 0, void 0, function* () {
            const commit = element.data;
            const item = util_1.unwrap(this.currentItem);
            return common_1.openDiff(item.repo, item.svnTarget, commit.revision, "BASE");
        });
    }
    openDiffCmd(element) {
        return __awaiter(this, void 0, void 0, function* () {
            const commit = element.data;
            const item = util_1.unwrap(this.currentItem);
            const pos = item.entries.findIndex(e => e === commit);
            if (pos === item.entries.length - 1) {
                vscode_1.window.showWarningMessage("Cannot diff last commit");
                return;
            }
            const prevRev = item.entries[pos + 1].revision;
            return common_1.openDiff(item.repo, item.svnTarget, prevRev, commit.revision);
        });
    }
    editorChanged(te) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.refresh(undefined, te);
        });
    }
    refresh(element, te, loadMore) {
        return __awaiter(this, void 0, void 0, function* () {
            // TODO maybe make autorefresh optionable?
            if (loadMore) {
                yield common_1.fetchMore(util_1.unwrap(this.currentItem));
                this._onDidChangeTreeData.fire(element);
                return;
            }
            if (te === undefined) {
                te = vscode_1.window.activeTextEditor;
            }
            if (te) {
                const uri = te.document.uri;
                if (uri.scheme === "file") {
                    if (uri.path.startsWith(tempFiles_1.tempdir)) {
                        return; // do not refresh if diff was called
                    }
                    const repo = this.model.getRepository(uri);
                    if (repo !== undefined) {
                        try {
                            const info = yield repo.getInfo(uri.path);
                            this.currentItem = {
                                isComplete: false,
                                entries: [],
                                repo,
                                svnTarget: vscode_1.Uri.parse(info.url),
                                persisted: {
                                    commitFrom: "HEAD",
                                    baseRevision: parseInt(info.revision, 10)
                                },
                                order: 0
                            };
                        }
                        catch (e) {
                            // doesn't belong to this repo
                        }
                    }
                }
                this._onDidChangeTreeData.fire(element);
            }
        });
    }
    getTreeItem(element) {
        return __awaiter(this, void 0, void 0, function* () {
            let ti;
            if (element.kind === common_1.LogTreeItemKind.Commit) {
                const cached = util_1.unwrap(this.currentItem);
                const commit = element.data;
                ti = new vscode_1.TreeItem(common_1.getCommitLabel(commit), vscode_1.TreeItemCollapsibleState.None);
                ti.iconPath = common_1.getCommitIcon(commit.author);
                ti.tooltip = common_1.getCommitToolTip(commit);
                ti.contextValue = "diffable";
                ti.command = {
                    command: "svn.itemlog.openDiff",
                    title: "Open diff",
                    arguments: [element]
                };
            }
            else if (element.kind === common_1.LogTreeItemKind.TItem) {
                ti = element.data;
            }
            else {
                throw new Error("Shouldn't happen");
            }
            return ti;
        });
    }
    getChildren(element) {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.currentItem === undefined) {
                return [];
            }
            if (element === undefined) {
                const fname = path.basename(this.currentItem.svnTarget.fsPath);
                const ti = new vscode_1.TreeItem(fname, vscode_1.TreeItemCollapsibleState.Expanded);
                ti.tooltip = path.dirname(this.currentItem.svnTarget.fsPath);
                ti.iconPath = common_1.getIconObject("icon-history");
                const item = {
                    kind: common_1.LogTreeItemKind.TItem,
                    data: ti
                };
                return [item];
            }
            else {
                const entries = this.currentItem.entries;
                if (entries.length === 0) {
                    yield common_1.fetchMore(this.currentItem);
                }
                const result = common_1.transform(entries, common_1.LogTreeItemKind.Commit);
                common_1.insertBaseMarker(this.currentItem, entries, result);
                if (!this.currentItem.isComplete) {
                    const ti = new vscode_1.TreeItem(`Load another ${common_1.getLimit()} revisions`);
                    const ltItem = {
                        kind: common_1.LogTreeItemKind.TItem,
                        data: ti
                    };
                    ti.tooltip = "Paging size may be adjusted using log.length setting";
                    ti.command = {
                        command: "svn.itemlog.refresh",
                        arguments: [element, undefined, true],
                        title: "refresh element"
                    };
                    ti.iconPath = common_1.getIconObject("icon-unfold");
                    result.push(ltItem);
                }
                return result;
            }
        });
    }
}
exports.ItemLogProvider = ItemLogProvider;
//# sourceMappingURL=itemLogProvider.js.map