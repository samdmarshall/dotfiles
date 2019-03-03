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
const repository_1 = require("../repository");
const util_1 = require("../util");
const common_1 = require("./common");
function getActionIcon(action) {
    let name;
    switch (action) {
        case "A":
            name = "status-added";
            break;
        case "D":
            name = "status-deleted";
            break;
        case "M":
            name = "status-modified";
            break;
        case "R":
            name = "status-renamed";
            break;
    }
    if (name === undefined) {
        return undefined;
    }
    return common_1.getIconObject(name);
}
class RepoLogProvider {
    constructor(model) {
        this.model = model;
        this._onDidChangeTreeData = new vscode_1.EventEmitter();
        this.onDidChangeTreeData = this
            ._onDidChangeTreeData.event;
        // TODO on-disk cache?
        this.logCache = new Map();
        this._dispose = [];
        this.refresh();
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.copymsg", (item) => __awaiter(this, void 0, void 0, function* () { return common_1.copyCommitToClipboard("msg", item); })));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.addrepolike", this.addRepolikeGui, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.remove", this.removeRepo, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.openFileRemote", this.openFileRemoteCmd, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.openDiff", this.openDiffCmd, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.openFileLocal", this.openFileLocal, this));
        this._dispose.push(vscode_1.commands.registerCommand("svn.repolog.refresh", this.refresh, this));
        this.model.onDidChangeRepository((e) => __awaiter(this, void 0, void 0, function* () {
            return this.refresh();
            // TODO refresh only required repo, need to pass element === getChildren()
        }));
    }
    getCached(maybeItem) {
        const item = util_1.unwrap(maybeItem);
        if (item.data instanceof common_1.SvnPath) {
            return util_1.unwrap(this.logCache.get(item.data.toString()));
        }
        return this.getCached(item.parent);
    }
    dispose() {
        util_1.dispose(this._dispose);
    }
    removeRepo(element) {
        this.logCache.delete(element.data.toString());
        this.refresh();
    }
    addRepolike(repoLike, rev) {
        return __awaiter(this, void 0, void 0, function* () {
            // TODO save user's custom repositories
            const item = {
                entries: [],
                isComplete: false,
                svnTarget: {},
                repo: {},
                persisted: {
                    commitFrom: rev,
                    userAdded: true
                },
                order: this.logCache.size
            };
            if (this.logCache.has(repoLike)) {
                vscode_1.window.showWarningMessage("This path is already added");
                return;
            }
            const repo = this.model.getRepository(repoLike);
            if (repo === undefined) {
                try {
                    let uri;
                    if (repoLike.startsWith("^")) {
                        const wsrepo = this.model.getRepository(util_1.unwrap(vscode_1.workspace.workspaceFolders)[0].uri);
                        if (!wsrepo) {
                            throw new Error("No repository in workspace root");
                        }
                        const info = yield wsrepo.getInfo(repoLike);
                        uri = vscode_1.Uri.parse(info.url);
                    }
                    else {
                        uri = vscode_1.Uri.parse(repoLike);
                    }
                    if (rev !== "HEAD" && isNaN(parseInt(rev, 10))) {
                        throw new Error("erroneous revision");
                    }
                    const remRepo = yield this.model.getRemoteRepository(uri);
                    item.repo = remRepo;
                    item.svnTarget = uri;
                }
                catch (e) {
                    vscode_1.window.showWarningMessage("Failed to add repo: " + (e instanceof Error ? e.message : ""));
                    return;
                }
            }
            else {
                try {
                    const svninfo = yield repo.getInfo(repoLike, rev);
                    item.repo = repo;
                    item.svnTarget = vscode_1.Uri.parse(svninfo.url);
                    item.persisted.baseRevision = parseInt(svninfo.revision, 10);
                }
                catch (e) {
                    vscode_1.window.showErrorMessage("Failed to resolve svn path");
                    return;
                }
            }
            const repoName = item.svnTarget.toString(true);
            if (this.logCache.has(repoName)) {
                vscode_1.window.showWarningMessage("Repository with this name already exists");
                return;
            }
            this.logCache.set(repoName, item);
            this._onDidChangeTreeData.fire();
        });
    }
    addRepolikeGui() {
        const box = vscode_1.window.createInputBox();
        box.prompt = "Enter SVN URL or local path";
        box.onDidAccept(() => {
            let repoLike = box.value;
            if (!path.isAbsolute(repoLike) &&
                vscode_1.workspace.workspaceFolders &&
                !repoLike.startsWith("^") &&
                !/^[a-z]+?:\/\//.test(repoLike)) {
                for (const wsf of vscode_1.workspace.workspaceFolders) {
                    const joined = path.join(wsf.uri.fsPath, repoLike);
                    if (fs.existsSync(joined)) {
                        repoLike = joined;
                        break;
                    }
                }
            }
            box.dispose();
            const box2 = vscode_1.window.createInputBox();
            box2.prompt = "Enter starting revision (optional)";
            box2.onDidAccept(() => __awaiter(this, void 0, void 0, function* () {
                const rev = box2.value;
                box2.dispose();
                return this.addRepolike(repoLike, rev || "HEAD");
            }), undefined);
            box2.show();
        });
        box.show();
    }
    openFileRemoteCmd(element) {
        const commit = element.data;
        const item = this.getCached(element);
        const ri = item.repo.getPathNormalizer().parse(commit._);
        if (common_1.checkIfFile(ri, false) === false) {
            return;
        }
        const parent = element.parent.data;
        return common_1.openFileRemote(item.repo, ri.remoteFullPath, parent.revision);
    }
    openFileLocal(element) {
        const commit = element.data;
        const item = this.getCached(element);
        const ri = item.repo.getPathNormalizer().parse(commit._);
        if (!common_1.checkIfFile(ri, true)) {
            return;
        }
        vscode_1.commands.executeCommand("vscode.open", util_1.unwrap(ri.localFullPath));
    }
    openDiffCmd(element) {
        return __awaiter(this, void 0, void 0, function* () {
            const commit = element.data;
            const item = this.getCached(element);
            const ri = item.repo.getPathNormalizer().parse(commit._);
            if (common_1.checkIfFile(ri, false) === false) {
                return;
            }
            const parent = element.parent.data;
            let prevRev;
            {
                // find prevRev scope
                const pos = item.entries.findIndex(e => e === parent);
                let posPrev;
                for (let i = pos + 1; posPrev === undefined && i < item.entries.length; i++) {
                    for (const p of item.entries[i].paths) {
                        if (p._ === commit._) {
                            posPrev = i;
                            break;
                        }
                    }
                }
                if (posPrev !== undefined) {
                    prevRev = item.entries[posPrev];
                }
                else {
                    // if not found in cache
                    const nm = item.repo.getPathNormalizer();
                    const revs = yield item.repo.log(parent.revision, "1", 2, nm.parse(commit._).remoteFullPath);
                    if (revs.length === 2) {
                        prevRev = revs[1];
                    }
                    else {
                        vscode_1.window.showWarningMessage("Cannot find previous commit");
                        return;
                    }
                }
            }
            return common_1.openDiff(item.repo, ri.remoteFullPath, prevRev.revision, parent.revision);
        });
    }
    refresh(element, fetchMoreClick) {
        return __awaiter(this, void 0, void 0, function* () {
            if (element === undefined) {
                for (const [k, v] of this.logCache) {
                    // Remove auto-added repositories
                    if (!v.persisted.userAdded) {
                        this.logCache.delete(k);
                    }
                }
                for (const repo of this.model.repositories) {
                    const remoteRoot = repo.branchRoot;
                    const repoUrl = remoteRoot.toString(true);
                    let persisted = {
                        commitFrom: "HEAD",
                        baseRevision: parseInt(repo.repository.info.revision, 10)
                    };
                    const prev = this.logCache.get(repoUrl);
                    if (prev) {
                        persisted = prev.persisted;
                    }
                    this.logCache.set(repoUrl, {
                        entries: [],
                        isComplete: false,
                        repo,
                        svnTarget: remoteRoot,
                        persisted,
                        order: this.logCache.size
                    });
                }
            }
            else if (element.kind === common_1.LogTreeItemKind.Repo) {
                const cached = this.getCached(element);
                if (fetchMoreClick) {
                    yield common_1.fetchMore(cached);
                }
                else {
                    cached.entries = [];
                    cached.isComplete = false;
                }
            }
            this._onDidChangeTreeData.fire(element);
        });
    }
    getTreeItem(element) {
        return __awaiter(this, void 0, void 0, function* () {
            let ti;
            if (element.kind === common_1.LogTreeItemKind.Repo) {
                const svnTarget = element.data;
                const cached = this.getCached(element);
                ti = new vscode_1.TreeItem(svnTarget.toString(), vscode_1.TreeItemCollapsibleState.Collapsed);
                if (cached.persisted.userAdded) {
                    ti.label = "∘ " + ti.label;
                    ti.contextValue = "userrepo";
                }
                else {
                    ti.contextValue = "repo";
                }
                if (cached.repo instanceof repository_1.Repository) {
                    ti.iconPath = common_1.getIconObject("folder");
                }
                else {
                    ti.iconPath = common_1.getIconObject("icon-repo");
                }
                const from = cached.persisted.commitFrom || "HEAD";
                ti.tooltip = `${svnTarget} since ${from}`;
            }
            else if (element.kind === common_1.LogTreeItemKind.Commit) {
                const commit = element.data;
                ti = new vscode_1.TreeItem(common_1.getCommitLabel(commit), vscode_1.TreeItemCollapsibleState.Collapsed);
                ti.tooltip = common_1.getCommitToolTip(commit);
                ti.iconPath = common_1.getCommitIcon(commit.author);
                ti.contextValue = "commit";
            }
            else if (element.kind === common_1.LogTreeItemKind.CommitDetail) {
                // TODO optional tree-view instead of flat
                const pathElem = element.data;
                const basename = path.basename(pathElem._);
                ti = new vscode_1.TreeItem(basename, vscode_1.TreeItemCollapsibleState.None);
                const cached = this.getCached(element);
                const nm = cached.repo.getPathNormalizer();
                ti.tooltip = nm.parse(pathElem._).relativeFromBranch;
                ti.iconPath = getActionIcon(pathElem.action);
                ti.contextValue = "diffable";
                ti.command = {
                    command: "svn.repolog.openDiff",
                    title: "Open diff",
                    arguments: [element]
                };
            }
            else if (element.kind === common_1.LogTreeItemKind.TItem) {
                ti = element.data;
            }
            else {
                throw new Error("Unknown tree elem");
            }
            return ti;
        });
    }
    getChildren(element) {
        return __awaiter(this, void 0, void 0, function* () {
            if (element === undefined) {
                return common_1.transform(Array.from(this.logCache.entries())
                    .sort(([lk, lv], [rk, rv]) => {
                    if (lv.persisted.userAdded !== rv.persisted.userAdded) {
                        return lv.persisted.userAdded ? 1 : -1;
                    }
                    return lv.order - rv.order;
                })
                    .map(([k, v]) => new common_1.SvnPath(k)), common_1.LogTreeItemKind.Repo);
            }
            else if (element.kind === common_1.LogTreeItemKind.Repo) {
                const limit = common_1.getLimit();
                const cached = this.getCached(element);
                const logentries = cached.entries;
                if (logentries.length === 0) {
                    yield common_1.fetchMore(cached);
                }
                const result = common_1.transform(logentries, common_1.LogTreeItemKind.Commit, element);
                common_1.insertBaseMarker(cached, logentries, result);
                if (!cached.isComplete) {
                    const ti = new vscode_1.TreeItem(`Load another ${limit} revisions`);
                    ti.tooltip = "Paging size may be adjusted using log.length setting";
                    ti.command = {
                        command: "svn.repolog.refresh",
                        arguments: [element, true],
                        title: "refresh element"
                    };
                    ti.iconPath = common_1.getIconObject("icon-unfold");
                    result.push({ kind: common_1.LogTreeItemKind.TItem, data: ti });
                }
                return result;
            }
            else if (element.kind === common_1.LogTreeItemKind.Commit) {
                const commit = element.data;
                return common_1.transform(commit.paths, common_1.LogTreeItemKind.CommitDetail, element);
            }
            return [];
        });
    }
}
exports.RepoLogProvider = RepoLogProvider;
//# sourceMappingURL=repoLogProvider.js.map