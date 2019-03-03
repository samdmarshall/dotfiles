"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
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
const timers_1 = require("timers");
const vscode_1 = require("vscode");
const types_1 = require("./common/types");
const decorators_1 = require("./decorators");
const configuration_1 = require("./helpers/configuration");
const operationsImpl_1 = require("./operationsImpl");
const pathNormalizer_1 = require("./pathNormalizer");
const resource_1 = require("./resource");
const statusBar_1 = require("./statusBar");
const svn_1 = require("./svn");
const uri_1 = require("./uri");
const util_1 = require("./util");
const globMatch_1 = require("./util/globMatch");
function shouldShowProgress(operation) {
    switch (operation) {
        case types_1.Operation.CurrentBranch:
        case types_1.Operation.Show:
        case types_1.Operation.Info:
            return false;
        default:
            return true;
    }
}
class Repository {
    constructor(repository) {
        this.repository = repository;
        this.changelists = new Map();
        this.statusIgnored = [];
        this.statusExternal = [];
        this.disposables = [];
        this.currentBranch = "";
        this.remoteChangedFiles = 0;
        this.isIncomplete = false;
        this.needCleanUp = false;
        this.deletedUris = [];
        this._onDidChangeRepository = new vscode_1.EventEmitter();
        this.onDidChangeRepository = this
            ._onDidChangeRepository.event;
        this._onDidChangeState = new vscode_1.EventEmitter();
        this.onDidChangeState = this
            ._onDidChangeState.event;
        this._onDidChangeStatus = new vscode_1.EventEmitter();
        this.onDidChangeStatus = this._onDidChangeStatus
            .event;
        this._onDidChangeRemoteChangedFiles = new vscode_1.EventEmitter();
        this.onDidChangeRemoteChangedFile = this
            ._onDidChangeRemoteChangedFiles.event;
        this._onRunOperation = new vscode_1.EventEmitter();
        this.onRunOperation = this._onRunOperation.event;
        this._onDidRunOperation = new vscode_1.EventEmitter();
        this.onDidRunOperation = this._onDidRunOperation
            .event;
        this._operations = new operationsImpl_1.default();
        this._state = types_1.RepositoryState.Idle;
        const fsWatcher = vscode_1.workspace.createFileSystemWatcher("**");
        this.disposables.push(fsWatcher);
        const onWorkspaceChange = util_1.anyEvent(fsWatcher.onDidChange, fsWatcher.onDidCreate, fsWatcher.onDidDelete);
        const onRepositoryChange = util_1.filterEvent(onWorkspaceChange, uri => !/^\.\./.test(path.relative(repository.root, uri.fsPath)));
        const onRelevantRepositoryChange = util_1.filterEvent(onRepositoryChange, uri => !/[\\\/]\.svn[\\\/]tmp/.test(uri.path));
        onRelevantRepositoryChange(this.onFSChange, this, this.disposables);
        const onRelevantSvnChange = util_1.filterEvent(onRelevantRepositoryChange, uri => /[\\\/]\.svn[\\\/]/.test(uri.path));
        // TODO on svn switch event fired two times since two files were changed
        onRelevantSvnChange((e) => __awaiter(this, void 0, void 0, function* () {
            yield this.repository.updateInfo();
            this._onDidChangeRepository.fire(e);
        }), this, this.disposables);
        this.sourceControl = vscode_1.scm.createSourceControl("svn", "SVN", vscode_1.Uri.file(repository.workspaceRoot));
        this.sourceControl.count = 0;
        this.sourceControl.inputBox.placeholder =
            "Message (press Ctrl+Enter to commit)";
        this.sourceControl.acceptInputCommand = {
            command: "svn.commitWithMessage",
            title: "commit",
            arguments: [this.sourceControl]
        };
        this.sourceControl.quickDiffProvider = this;
        this.disposables.push(this.sourceControl);
        this.statusBar = new statusBar_1.SvnStatusBar(this);
        this.disposables.push(this.statusBar);
        this.statusBar.onDidChange(() => (this.sourceControl.statusBarCommands = this.statusBar.commands), null, this.disposables);
        this.changes = this.sourceControl.createResourceGroup("changes", "Changes");
        this.conflicts = this.sourceControl.createResourceGroup("conflicts", "conflicts");
        this.unversioned = this.sourceControl.createResourceGroup("unversioned", "Unversioned");
        this.changes.hideWhenEmpty = true;
        this.unversioned.hideWhenEmpty = true;
        this.conflicts.hideWhenEmpty = true;
        this.disposables.push(this.changes);
        this.disposables.push(this.conflicts);
        // The this.unversioned can recreated by update state model
        this.disposables.push(util_1.toDisposable(() => this.unversioned.dispose()));
        // Dispose the setInterval of Remote Changes
        this.disposables.push(util_1.toDisposable(() => {
            if (this.remoteChangedUpdateInterval) {
                timers_1.clearInterval(this.remoteChangedUpdateInterval);
            }
        }));
        // For each deleted file, append to list
        const onFsDelete = util_1.filterEvent(fsWatcher.onDidDelete, uri => !/[\\\/]\.svn[\\\/]/.test(uri.path));
        onFsDelete(uri => this.deletedUris.push(uri), this, this.disposables);
        // Only check deleted files after the status list is fully updated
        this.onDidChangeStatus(this.actionForDeletedFiles, this, this.disposables);
        this.createRemoteChangedInterval();
        this.updateRemoteChangedFiles();
        // On change config, dispose current interval and create a new.
        configuration_1.configuration.onDidChange(e => {
            if (e.affectsConfiguration("svn.remoteChanges.checkFrequency")) {
                if (this.remoteChangedUpdateInterval) {
                    timers_1.clearInterval(this.remoteChangedUpdateInterval);
                }
                this.createRemoteChangedInterval();
                this.updateRemoteChangedFiles();
            }
        });
        this.status();
        this.disposables.push(vscode_1.workspace.onDidSaveTextDocument(document => {
            this.onDidSaveTextDocument(document);
        }));
    }
    get onDidChangeOperations() {
        return util_1.anyEvent(this.onRunOperation, this.onDidRunOperation);
    }
    get operations() {
        return this._operations;
    }
    get state() {
        return this._state;
    }
    set state(state) {
        this._state = state;
        this._onDidChangeState.fire(state);
        this.changes.resourceStates = [];
        this.unversioned.resourceStates = [];
        this.conflicts.resourceStates = [];
        this.changelists.forEach((group, changelist) => {
            group.resourceStates = [];
        });
        if (this.remoteChanges) {
            this.remoteChanges.dispose();
        }
        this.isIncomplete = false;
        this.needCleanUp = false;
    }
    get root() {
        return this.repository.root;
    }
    get workspaceRoot() {
        return this.repository.workspaceRoot;
    }
    /** 'svn://repo.x/branches/b1' e.g. */
    get branchRoot() {
        return vscode_1.Uri.parse(this.repository.info.url);
    }
    get inputBox() {
        return this.sourceControl.inputBox;
    }
    get username() {
        return this.repository.username;
    }
    set username(username) {
        this.repository.username = username;
    }
    get password() {
        return this.repository.password;
    }
    set password(password) {
        this.repository.password = password;
    }
    createRemoteChangedInterval() {
        const updateFreq = configuration_1.configuration.get("remoteChanges.checkFrequency", 300);
        if (!updateFreq) {
            return;
        }
        this.remoteChangedUpdateInterval = timers_1.setInterval(() => {
            this.updateRemoteChangedFiles();
        }, 1000 * updateFreq);
    }
    /**
     * Check all recently deleted files and compare with svn status "missing"
     */
    actionForDeletedFiles() {
        return __awaiter(this, void 0, void 0, function* () {
            if (!this.deletedUris.length) {
                return;
            }
            const allUris = this.deletedUris;
            this.deletedUris = [];
            const actionForDeletedFiles = configuration_1.configuration.get("delete.actionForDeletedFiles", "prompt");
            if (actionForDeletedFiles === "none") {
                return;
            }
            const resources = allUris
                .map(uri => this.getResourceFromFile(uri))
                .filter(resource => resource && resource.type === types_1.Status.MISSING);
            let uris = resources.map(resource => resource.resourceUri);
            if (!uris.length) {
                return;
            }
            const ignoredRulesForDeletedFiles = configuration_1.configuration.get("delete.ignoredRulesForDeletedFiles", []);
            const rules = ignoredRulesForDeletedFiles.map(ignored => globMatch_1.match(ignored));
            if (rules.length) {
                uris = uris.filter(uri => {
                    // Check first for relative URL (Better for workspace configuration)
                    const relativePath = this.repository.removeAbsolutePath(uri.fsPath);
                    // If some match, remove from list
                    return !rules.some(rule => rule.match(relativePath) || rule.match(uri.fsPath));
                });
            }
            if (!uris.length) {
                return;
            }
            if (actionForDeletedFiles === "remove") {
                return yield this.removeFiles(uris.map(uri => uri.fsPath), false);
            }
            else if (actionForDeletedFiles === "prompt") {
                return yield vscode_1.commands.executeCommand("svn.promptRemove", ...uris);
            }
        });
    }
    updateRemoteChangedFiles() {
        return __awaiter(this, void 0, void 0, function* () {
            const updateFreq = configuration_1.configuration.get("remoteChanges.checkFrequency", 300);
            if (updateFreq) {
                this.run(types_1.Operation.StatusRemote);
            }
            else {
                // Remove list of remote changes
                if (this.remoteChanges) {
                    this.remoteChanges.dispose();
                    this.remoteChanges = undefined;
                }
            }
        });
    }
    onFSChange(uri) {
        const autorefresh = configuration_1.configuration.get("autorefresh");
        if (!autorefresh) {
            return;
        }
        if (!this.operations.isIdle()) {
            return;
        }
        this.eventuallyUpdateWhenIdleAndWait();
    }
    eventuallyUpdateWhenIdleAndWait() {
        this.updateWhenIdleAndWait();
    }
    updateWhenIdleAndWait() {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.whenIdleAndFocused();
            yield this.status();
            yield util_1.timeout(5000);
        });
    }
    whenIdleAndFocused() {
        return __awaiter(this, void 0, void 0, function* () {
            while (true) {
                if (!this.operations.isIdle()) {
                    yield util_1.eventToPromise(this.onDidRunOperation);
                    continue;
                }
                if (!vscode_1.window.state.focused) {
                    const onDidFocusWindow = util_1.filterEvent(vscode_1.window.onDidChangeWindowState, e => e.focused);
                    yield util_1.eventToPromise(onDidFocusWindow);
                    continue;
                }
                return;
            }
        });
    }
    updateModelState(checkRemoteChanges = false) {
        return __awaiter(this, void 0, void 0, function* () {
            const changes = [];
            const unversioned = [];
            const external = [];
            const conflicts = [];
            const changelists = new Map();
            const remoteChanges = [];
            this.statusExternal = [];
            this.statusIgnored = [];
            this.isIncomplete = false;
            this.needCleanUp = false;
            const combineExternal = configuration_1.configuration.get("sourceControl.combineExternalIfSameServer", false);
            const statuses = (yield this.retryRun(() => __awaiter(this, void 0, void 0, function* () {
                return yield this.repository.getStatus({
                    includeIgnored: true,
                    includeExternals: combineExternal,
                    checkRemoteChanges
                });
            }))) || [];
            const fileConfig = vscode_1.workspace.getConfiguration("files", vscode_1.Uri.file(this.root));
            const filesToExclude = fileConfig.get("exclude");
            const excludeList = [];
            for (const pattern in filesToExclude) {
                if (filesToExclude.hasOwnProperty(pattern)) {
                    const negate = !filesToExclude[pattern];
                    excludeList.push((negate ? "!" : "") + pattern);
                }
            }
            this.statusExternal = statuses.filter(status => status.status === types_1.Status.EXTERNAL);
            if (combineExternal && this.statusExternal.length) {
                const repositoryUuid = yield this.repository.getRepositoryUuid();
                this.statusExternal = this.statusExternal.filter(status => repositoryUuid !== status.repositoryUuid);
            }
            const statusesRepository = statuses.filter(status => {
                if (status.status === types_1.Status.EXTERNAL) {
                    return false;
                }
                return !this.statusExternal.some(external => util_1.isDescendant(external.path, status.path));
            });
            const hideUnversioned = configuration_1.configuration.get("sourceControl.hideUnversioned");
            for (const status of statusesRepository) {
                if (status.path === ".") {
                    this.isIncomplete = status.status === types_1.Status.INCOMPLETE;
                    this.needCleanUp = status.wcStatus.locked;
                }
                // If exists a switched item, the repository is incomplete
                // To simulate, run "svn switch" and kill "svn" proccess
                // After, run "svn update"
                if (status.wcStatus.switched) {
                    this.isIncomplete = true;
                }
                if (status.wcStatus.locked ||
                    status.wcStatus.switched ||
                    status.status === types_1.Status.INCOMPLETE) {
                    // On commit, `svn status` return all locked files with status="normal" and props="none"
                    continue;
                }
                if (globMatch_1.matchAll(status.path, excludeList, { dot: true })) {
                    continue;
                }
                const uri = vscode_1.Uri.file(path.join(this.workspaceRoot, status.path));
                const renameUri = status.rename
                    ? vscode_1.Uri.file(path.join(this.workspaceRoot, status.rename))
                    : undefined;
                if (status.reposStatus) {
                    remoteChanges.push(new resource_1.Resource(uri, status.reposStatus.item, undefined, status.reposStatus.props, true));
                }
                const resource = new resource_1.Resource(uri, status.status, renameUri, status.props);
                if ((status.status === types_1.Status.NORMAL || status.status === types_1.Status.NONE) &&
                    (status.props === types_1.Status.NORMAL || status.props === types_1.Status.NONE) &&
                    !status.changelist) {
                    // Ignore non changed itens
                    continue;
                }
                else if (status.status === types_1.Status.IGNORED) {
                    this.statusIgnored.push(status);
                }
                else if (status.status === types_1.Status.CONFLICTED) {
                    conflicts.push(resource);
                }
                else if (status.status === types_1.Status.UNVERSIONED) {
                    if (hideUnversioned) {
                        continue;
                    }
                    const matches = status.path.match(/(.+?)\.(mine|working|merge-\w+\.r\d+|r\d+)$/);
                    // If file end with (mine, working, merge, etc..) and has file without extension
                    if (matches &&
                        matches[1] &&
                        statuses.some(s => s.path === matches[1])) {
                        continue;
                    }
                    else {
                        unversioned.push(resource);
                    }
                }
                else if (status.changelist) {
                    let changelist = changelists.get(status.changelist);
                    if (!changelist) {
                        changelist = [];
                    }
                    changelist.push(resource);
                    changelists.set(status.changelist, changelist);
                }
                else {
                    changes.push(resource);
                }
            }
            this.changes.resourceStates = changes;
            this.conflicts.resourceStates = conflicts;
            const prevChangelistsSize = this.changelists.size;
            this.changelists.forEach((group, changelist) => {
                group.resourceStates = [];
            });
            const counts = [this.changes, this.conflicts];
            const ignoreOnCommitList = configuration_1.configuration.get("sourceControl.ignoreOnCommit");
            const ignoreOnStatusCountList = configuration_1.configuration.get("sourceControl.ignoreOnStatusCount");
            changelists.forEach((resources, changelist) => {
                let group = this.changelists.get(changelist);
                if (!group) {
                    // Prefix 'changelist-' to prevent double id with 'change' or 'external'
                    group = this.sourceControl.createResourceGroup(`changelist-${changelist}`, `Changelist "${changelist}"`);
                    group.hideWhenEmpty = true;
                    this.disposables.push(group);
                    this.changelists.set(changelist, group);
                }
                group.resourceStates = resources;
                if (!ignoreOnStatusCountList.includes(changelist)) {
                    counts.push(group);
                }
            });
            // Recreate unversioned group to move after changelists
            if (prevChangelistsSize !== this.changelists.size) {
                this.unversioned.dispose();
                this.unversioned = this.sourceControl.createResourceGroup("unversioned", "Unversioned");
                this.unversioned.hideWhenEmpty = true;
            }
            this.unversioned.resourceStates = unversioned;
            if (configuration_1.configuration.get("sourceControl.countUnversioned", false)) {
                counts.push(this.unversioned);
            }
            this.sourceControl.count = counts.reduce((a, b) => a + b.resourceStates.length, 0);
            // Recreate remoteChanges group to move after unversioned
            if (!this.remoteChanges || prevChangelistsSize !== this.changelists.size) {
                /**
                 * Destroy and create for keep at last position
                 */
                let tempResourceStates = [];
                if (this.remoteChanges) {
                    tempResourceStates = this.remoteChanges.resourceStates;
                    this.remoteChanges.dispose();
                }
                this.remoteChanges = this.sourceControl.createResourceGroup("remotechanges", "Remote Changes");
                this.remoteChanges.repository = this;
                this.remoteChanges.hideWhenEmpty = true;
                this.remoteChanges.resourceStates = tempResourceStates;
            }
            // Update remote changes group
            if (checkRemoteChanges) {
                this.remoteChanges.resourceStates = remoteChanges;
                if (remoteChanges.length !== this.remoteChangedFiles) {
                    this.remoteChangedFiles = remoteChanges.length;
                    this._onDidChangeRemoteChangedFiles.fire();
                }
            }
            this._onDidChangeStatus.fire();
            this.currentBranch = yield this.getCurrentBranch();
            return Promise.resolve();
        });
    }
    getResourceFromFile(uri) {
        if (typeof uri === "string") {
            uri = vscode_1.Uri.file(uri);
        }
        const groups = [
            this.changes,
            this.conflicts,
            this.unversioned,
            ...this.changelists.values()
        ];
        const uriString = uri.toString();
        for (const group of groups) {
            for (const resource of group.resourceStates) {
                if (uriString === resource.resourceUri.toString() &&
                    resource instanceof resource_1.Resource) {
                    return resource;
                }
            }
        }
        return undefined;
    }
    provideOriginalResource(uri) {
        if (uri.scheme !== "file") {
            return;
        }
        // Not has original resource for content of ".svn" folder
        if (util_1.isDescendant(path.join(this.root, ".svn"), uri.fsPath)) {
            return;
        }
        return uri_1.toSvnUri(uri, types_1.SvnUriAction.SHOW, {}, true);
    }
    getBranches() {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                return yield this.repository.getBranches();
            }
            catch (error) {
                return [];
            }
        });
    }
    status() {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Status);
        });
    }
    show(filePath, revision) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Show, () => {
                return this.repository.show(filePath, revision);
            });
        });
    }
    addFiles(files) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Add, () => this.repository.addFiles(files));
        });
    }
    addChangelist(files, changelist) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.AddChangelist, () => this.repository.addChangelist(files, changelist));
        });
    }
    removeChangelist(files) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.RemoveChangelist, () => this.repository.removeChangelist(files));
        });
    }
    getCurrentBranch() {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.CurrentBranch, () => __awaiter(this, void 0, void 0, function* () {
                return this.repository.getCurrentBranch();
            }));
        });
    }
    newBranch(name, commitMessage = "Created new branch") {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.NewBranch, () => __awaiter(this, void 0, void 0, function* () {
                yield this.repository.newBranch(name, commitMessage);
                this.updateRemoteChangedFiles();
            }));
        });
    }
    switchBranch(name, force = false) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.run(types_1.Operation.SwitchBranch, () => __awaiter(this, void 0, void 0, function* () {
                yield this.repository.switchBranch(name, force);
                this.updateRemoteChangedFiles();
            }));
        });
    }
    updateRevision(ignoreExternals = false) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Update, () => __awaiter(this, void 0, void 0, function* () {
                const response = yield this.repository.update(ignoreExternals);
                this.updateRemoteChangedFiles();
                return response;
            }));
        });
    }
    pullIncomingChange(path) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Update, () => __awaiter(this, void 0, void 0, function* () {
                const response = yield this.repository.pullIncomingChange(path);
                this.updateRemoteChangedFiles();
                return response;
            }));
        });
    }
    resolve(files, action) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Resolve, () => this.repository.resolve(files, action));
        });
    }
    commitFiles(message, files) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Commit, () => this.repository.commitFiles(message, files));
        });
    }
    revert(files) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Revert, () => this.repository.revert(files));
        });
    }
    info(path) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Info, () => this.repository.getInfo(path));
        });
    }
    patch(files) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Patch, () => this.repository.patch(files));
        });
    }
    patchChangelist(changelistName) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Patch, () => this.repository.patchChangelist(changelistName));
        });
    }
    removeFiles(files, keepLocal) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Remove, () => this.repository.removeFiles(files, keepLocal));
        });
    }
    plainLog() {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Log, () => this.repository.plainLog());
        });
    }
    log(rfrom, rto, limit, target) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Log, () => this.repository.log(rfrom, rto, limit, target));
        });
    }
    cleanup() {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.CleanUp, () => this.repository.cleanup());
        });
    }
    getInfo(path, revision) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Info, () => this.repository.getInfo(path, revision, true));
        });
    }
    finishCheckout() {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.SwitchBranch, () => this.repository.finishCheckout());
        });
    }
    addToIgnore(expressions, directory, recursive = false) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Ignore, () => this.repository.addToIgnore(expressions, directory, recursive));
        });
    }
    rename(oldFile, newFile) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.run(types_1.Operation.Rename, () => this.repository.rename(oldFile, newFile));
        });
    }
    getPathNormalizer() {
        return new pathNormalizer_1.PathNormalizer(this.repository.info);
    }
    promptAuth() {
        return __awaiter(this, void 0, void 0, function* () {
            // Prevent multiple prompts for auth
            if (this.lastPromptAuth) {
                return this.lastPromptAuth;
            }
            this.lastPromptAuth = vscode_1.commands.executeCommand("svn.promptAuth");
            const result = yield this.lastPromptAuth;
            if (result) {
                this.username = result.username;
                this.password = result.password;
            }
            this.lastPromptAuth = undefined;
            return result;
        });
    }
    onDidSaveTextDocument(document) {
        const uriString = document.uri.toString();
        const conflict = this.conflicts.resourceStates.find(resource => resource.resourceUri.toString() === uriString);
        if (!conflict) {
            return;
        }
        const text = document.getText();
        // Check for lines begin with "<<<<<<", "=======", ">>>>>>>"
        if (!/^<{7}[^]+^={7}[^]+^>{7}/m.test(text)) {
            vscode_1.commands.executeCommand("svn.resolved", conflict.resourceUri);
        }
    }
    run(operation, runOperation = () => Promise.resolve(null)) {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.state !== types_1.RepositoryState.Idle) {
                throw new Error("Repository not initialized");
            }
            const run = () => __awaiter(this, void 0, void 0, function* () {
                this._operations.start(operation);
                this._onRunOperation.fire(operation);
                try {
                    const result = yield this.retryRun(runOperation);
                    const checkRemote = operation === types_1.Operation.StatusRemote;
                    if (!util_1.isReadOnly(operation)) {
                        yield this.updateModelState(checkRemote);
                    }
                    return result;
                }
                catch (err) {
                    if (err.svnErrorCode === svn_1.svnErrorCodes.NotASvnRepository) {
                        this.state = types_1.RepositoryState.Disposed;
                    }
                    throw err;
                }
                finally {
                    this._operations.end(operation);
                    this._onDidRunOperation.fire(operation);
                }
            });
            return shouldShowProgress(operation)
                ? vscode_1.window.withProgress({ location: vscode_1.ProgressLocation.SourceControl }, run)
                : run();
        });
    }
    retryRun(runOperation = () => Promise.resolve(null)) {
        return __awaiter(this, void 0, void 0, function* () {
            let attempt = 0;
            while (true) {
                try {
                    attempt++;
                    return yield runOperation();
                }
                catch (err) {
                    if (err.svnErrorCode === svn_1.svnErrorCodes.RepositoryIsLocked &&
                        attempt <= 10) {
                        // quatratic backoff
                        yield util_1.timeout(Math.pow(attempt, 2) * 50);
                    }
                    else if (err.svnErrorCode === svn_1.svnErrorCodes.AuthorizationFailed &&
                        attempt <= 3) {
                        const result = yield this.promptAuth();
                        if (!result) {
                            throw err;
                        }
                    }
                    else {
                        throw err;
                    }
                }
            }
        });
    }
    dispose() {
        this.disposables = util_1.dispose(this.disposables);
    }
}
__decorate([
    decorators_1.memoize
], Repository.prototype, "onDidChangeOperations", null);
__decorate([
    decorators_1.debounce(1000)
], Repository.prototype, "actionForDeletedFiles", null);
__decorate([
    decorators_1.debounce(1000)
], Repository.prototype, "updateRemoteChangedFiles", null);
__decorate([
    decorators_1.debounce(1000)
], Repository.prototype, "eventuallyUpdateWhenIdleAndWait", null);
__decorate([
    decorators_1.throttle
], Repository.prototype, "updateWhenIdleAndWait", null);
__decorate([
    decorators_1.throttle
], Repository.prototype, "updateModelState", null);
__decorate([
    decorators_1.throttle
], Repository.prototype, "status", null);
exports.Repository = Repository;
//# sourceMappingURL=repository.js.map