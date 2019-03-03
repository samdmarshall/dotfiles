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
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const types_1 = require("./common/types");
const decorators_1 = require("./decorators");
const configuration_1 = require("./helpers/configuration");
const remoteRepository_1 = require("./remoteRepository");
const repository_1 = require("./repository");
const svn_1 = require("./svn");
const svnError_1 = require("./svnError");
const util_1 = require("./util");
const globMatch_1 = require("./util/globMatch");
class Model {
    constructor(_svn, policy) {
        this._svn = _svn;
        this._onDidOpenRepository = new vscode_1.EventEmitter();
        this.onDidOpenRepository = this
            ._onDidOpenRepository.event;
        this._onDidCloseRepository = new vscode_1.EventEmitter();
        this.onDidCloseRepository = this
            ._onDidCloseRepository.event;
        this._onDidChangeRepository = new vscode_1.EventEmitter();
        this.onDidChangeRepository = this
            ._onDidChangeRepository.event;
        this.openRepositories = [];
        this.disposables = [];
        this.enabled = false;
        this.possibleSvnRepositoryPaths = new Set();
        this.ignoreList = [];
        this.maxDepth = 0;
        if (policy !== types_1.ConstructorPolicy.Async) {
            throw new Error("Unsopported policy");
        }
        this.enabled = configuration_1.configuration.get("enabled") === true;
        this.configurationChangeDisposable = vscode_1.workspace.onDidChangeConfiguration(this.onDidChangeConfiguration, this);
        return (() => __awaiter(this, void 0, void 0, function* () {
            if (this.enabled) {
                yield this.enable();
            }
            return this;
        }))();
    }
    get repositories() {
        return this.openRepositories.map(r => r.repository);
    }
    get svn() {
        return this._svn;
    }
    onDidChangeConfiguration() {
        const enabled = configuration_1.configuration.get("enabled") === true;
        this.maxDepth = configuration_1.configuration.get("multipleFolders.depth", 0);
        if (enabled === this.enabled) {
            return;
        }
        this.enabled = enabled;
        if (enabled) {
            this.enable();
        }
        else {
            this.disable();
        }
    }
    enable() {
        return __awaiter(this, void 0, void 0, function* () {
            const multipleFolders = configuration_1.configuration.get("multipleFolders.enabled", false);
            if (multipleFolders) {
                this.maxDepth = configuration_1.configuration.get("multipleFolders.depth", 0);
                this.ignoreList = configuration_1.configuration.get("multipleFolders.ignore", []);
            }
            vscode_1.workspace.onDidChangeWorkspaceFolders(this.onDidChangeWorkspaceFolders, this, this.disposables);
            const fsWatcher = vscode_1.workspace.createFileSystemWatcher("**");
            this.disposables.push(fsWatcher);
            const onWorkspaceChange = util_1.anyEvent(fsWatcher.onDidChange, fsWatcher.onDidCreate, fsWatcher.onDidDelete);
            const onPossibleSvnRepositoryChange = util_1.filterEvent(onWorkspaceChange, uri => !this.getRepository(uri));
            onPossibleSvnRepositoryChange(this.onPossibleSvnRepositoryChange, this, this.disposables);
            vscode_1.window.onDidChangeActiveTextEditor(() => this.checkHasChangesOnActiveEditor(), this, this.disposables);
            yield this.scanWorkspaceFolders();
        });
    }
    onPossibleSvnRepositoryChange(uri) {
        const possibleSvnRepositoryPath = uri.fsPath.replace(/\.svn.*$/, "");
        this.eventuallyScanPossibleSvnRepository(possibleSvnRepositoryPath);
    }
    eventuallyScanPossibleSvnRepository(path) {
        this.possibleSvnRepositoryPaths.add(path);
        this.eventuallyScanPossibleSvnRepositories();
    }
    eventuallyScanPossibleSvnRepositories() {
        for (const path of this.possibleSvnRepositoryPaths) {
            this.tryOpenRepository(path);
        }
        this.possibleSvnRepositoryPaths.clear();
    }
    scanExternals(repository) {
        const shouldScanExternals = configuration_1.configuration.get("detectExternals") === true;
        if (!shouldScanExternals) {
            return;
        }
        repository.statusExternal
            .map(r => path.join(repository.workspaceRoot, r.path))
            .forEach(p => this.eventuallyScanPossibleSvnRepository(p));
    }
    hasChangesOnActiveEditor() {
        if (!vscode_1.window.activeTextEditor) {
            return false;
        }
        const uri = vscode_1.window.activeTextEditor.document.uri;
        const repository = this.getRepository(uri);
        if (!repository) {
            return false;
        }
        const resource = repository.getResourceFromFile(uri);
        if (!resource) {
            return false;
        }
        switch (resource.type) {
            case types_1.Status.ADDED:
            case types_1.Status.DELETED:
            case types_1.Status.EXTERNAL:
            case types_1.Status.IGNORED:
            case types_1.Status.NONE:
            case types_1.Status.NORMAL:
            case types_1.Status.UNVERSIONED:
                return false;
            case types_1.Status.CONFLICTED:
            case types_1.Status.INCOMPLETE:
            case types_1.Status.MERGED:
            case types_1.Status.MISSING:
            case types_1.Status.MODIFIED:
            case types_1.Status.OBSTRUCTED:
            case types_1.Status.REPLACED:
                return true;
        }
        // Show if not match
        return true;
    }
    checkHasChangesOnActiveEditor() {
        vscode_1.commands.executeCommand("setContext", "svnActiveEditorHasChanges", this.hasChangesOnActiveEditor());
    }
    disable() {
        this.repositories.forEach(repository => repository.dispose());
        this.openRepositories = [];
        this.possibleSvnRepositoryPaths.clear();
        this.disposables = util_1.dispose(this.disposables);
    }
    onDidChangeWorkspaceFolders({ added, removed }) {
        return __awaiter(this, void 0, void 0, function* () {
            const possibleRepositoryFolders = added.filter(folder => !this.getOpenRepository(folder.uri));
            const openRepositoriesToDispose = removed
                .map(folder => this.getOpenRepository(folder.uri.fsPath))
                .filter(repository => !!repository)
                .filter(repository => !(vscode_1.workspace.workspaceFolders || []).some(f => repository.repository.workspaceRoot.startsWith(f.uri.fsPath)));
            possibleRepositoryFolders.forEach(p => this.tryOpenRepository(p.uri.fsPath));
            openRepositoriesToDispose.forEach(r => r.repository.dispose());
        });
    }
    scanWorkspaceFolders() {
        return __awaiter(this, void 0, void 0, function* () {
            for (const folder of vscode_1.workspace.workspaceFolders || []) {
                const root = folder.uri.fsPath;
                yield this.tryOpenRepository(root);
            }
        });
    }
    tryOpenRepository(path, level = 0) {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.getRepository(path)) {
                return;
            }
            let isSvnFolder = fs.existsSync(path + "/.svn");
            // If open only a subpath.
            if (!isSvnFolder && level === 0) {
                const pathParts = path.split(/[\\/]/);
                while (pathParts.length > 0) {
                    pathParts.pop();
                    const topPath = pathParts.join("/") + "/.svn";
                    isSvnFolder = fs.existsSync(topPath);
                    if (isSvnFolder) {
                        break;
                    }
                }
            }
            if (isSvnFolder) {
                // Config based on folder path
                const resourceConfig = vscode_1.workspace.getConfiguration("svn", vscode_1.Uri.file(path));
                const ignoredRepos = new Set((resourceConfig.get("ignoreRepositories") || []).map(p => util_1.normalizePath(p)));
                if (ignoredRepos.has(util_1.normalizePath(path))) {
                    return;
                }
                try {
                    const repositoryRoot = yield this.svn.getRepositoryRoot(path);
                    const repository = new repository_1.Repository(yield this.svn.open(repositoryRoot, path));
                    this.open(repository);
                }
                catch (err) {
                    if (err instanceof svnError_1.default) {
                        if (err.svnErrorCode === svn_1.svnErrorCodes.WorkingCopyIsTooOld) {
                            yield vscode_1.commands.executeCommand("svn.upgrade", path);
                            return;
                        }
                    }
                    console.error(err);
                }
                return;
            }
            const newLevel = level + 1;
            if (newLevel <= this.maxDepth) {
                for (const file of fs.readdirSync(path)) {
                    const dir = path + "/" + file;
                    if (fs.statSync(dir).isDirectory() &&
                        !globMatch_1.matchAll(dir, this.ignoreList, { dot: true })) {
                        yield this.tryOpenRepository(dir, newLevel);
                    }
                }
            }
        });
    }
    getRemoteRepository(uri) {
        return __awaiter(this, void 0, void 0, function* () {
            return remoteRepository_1.RemoteRepository.open(this.svn, uri);
        });
    }
    getRepository(hint) {
        const liveRepository = this.getOpenRepository(hint);
        if (liveRepository && liveRepository.repository) {
            return liveRepository.repository;
        }
    }
    getOpenRepository(hint) {
        if (!hint) {
            return undefined;
        }
        if (hint instanceof repository_1.Repository) {
            return this.openRepositories.find(r => r.repository === hint);
        }
        if (hint.repository instanceof repository_1.Repository) {
            return this.openRepositories.find(r => r.repository === hint.repository);
        }
        if (typeof hint === "string") {
            hint = vscode_1.Uri.file(hint);
        }
        if (hint instanceof vscode_1.Uri) {
            return this.openRepositories.find(liveRepository => {
                if (!util_1.isDescendant(liveRepository.repository.workspaceRoot, hint.fsPath)) {
                    return false;
                }
                for (const external of liveRepository.repository.statusExternal) {
                    const externalPath = path.join(liveRepository.repository.workspaceRoot, external.path);
                    if (util_1.isDescendant(externalPath, hint.fsPath)) {
                        return false;
                    }
                }
                return true;
            });
        }
        for (const liveRepository of this.openRepositories) {
            const repository = liveRepository.repository;
            if (hint === repository.sourceControl) {
                return liveRepository;
            }
            if (hint === repository.changes) {
                return liveRepository;
            }
        }
        return undefined;
    }
    getRepositoryFromUri(uri) {
        return __awaiter(this, void 0, void 0, function* () {
            for (const liveRepository of this.openRepositories) {
                const repository = liveRepository.repository;
                try {
                    const path = util_1.normalizePath(uri.fsPath);
                    const info = yield repository.info(path);
                    return repository;
                }
                catch (error) {
                    console.error();
                }
            }
        });
    }
    open(repository) {
        const onDidDisappearRepository = util_1.filterEvent(repository.onDidChangeState, state => state === types_1.RepositoryState.Disposed);
        const disappearListener = onDidDisappearRepository(() => dispose());
        const changeListener = repository.onDidChangeRepository(uri => this._onDidChangeRepository.fire({ repository, uri }));
        const statusListener = repository.onDidChangeStatus(() => {
            this.scanExternals(repository);
            this.checkHasChangesOnActiveEditor();
        });
        this.scanExternals(repository);
        const dispose = () => {
            disappearListener.dispose();
            changeListener.dispose();
            statusListener.dispose();
            repository.dispose();
            this.openRepositories = this.openRepositories.filter(e => e !== openRepository);
            this._onDidCloseRepository.fire(repository);
        };
        const openRepository = { repository, dispose };
        this.openRepositories.push(openRepository);
        this._onDidOpenRepository.fire(repository);
    }
    close(repository) {
        const openRepository = this.getOpenRepository(repository);
        if (!openRepository) {
            return;
        }
        openRepository.dispose();
    }
    pickRepository() {
        return __awaiter(this, void 0, void 0, function* () {
            if (this.openRepositories.length === 0) {
                throw new Error("There are no available repositories");
            }
            const picks = this.repositories.map(repository => {
                return {
                    label: path.basename(repository.root),
                    repository
                };
            });
            const placeHolder = "Choose a repository";
            const pick = yield vscode_1.window.showQuickPick(picks, { placeHolder });
            return pick && pick.repository;
        });
    }
    upgradeWorkingCopy(folderPath) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const result = yield this.svn.exec(folderPath, ["upgrade"]);
                return result.exitCode === 0;
            }
            catch (e) {
                console.log(e);
            }
            return false;
        });
    }
    dispose() {
        this.disable();
        this.configurationChangeDisposable.dispose();
    }
}
__decorate([
    decorators_1.debounce(500)
], Model.prototype, "eventuallyScanPossibleSvnRepositories", null);
__decorate([
    decorators_1.debounce(100)
], Model.prototype, "checkHasChangesOnActiveEditor", null);
exports.Model = Model;
//# sourceMappingURL=model.js.map