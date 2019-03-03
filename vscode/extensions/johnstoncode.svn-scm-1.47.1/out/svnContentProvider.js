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
const vscode_1 = require("vscode");
const types_1 = require("./common/types");
const decorators_1 = require("./decorators");
const uri_1 = require("./uri");
const util_1 = require("./util");
const THREE_MINUTES = 1000 * 60 * 3;
const FIVE_MINUTES = 1000 * 60 * 5;
class SvnContentProvider {
    constructor(model) {
        this.model = model;
        this._onDidChange = new vscode_1.EventEmitter();
        this.changedRepositoryRoots = new Set();
        this.cache = Object.create(null);
        this.disposables = [];
        this.disposables.push(model.onDidChangeRepository(this.onDidChangeRepository, this), vscode_1.workspace.registerTextDocumentContentProvider("svn", this));
        const interval = setInterval(() => this.cleanup(), FIVE_MINUTES);
        this.disposables.push(util_1.toDisposable(() => clearInterval(interval)));
    }
    get onDidChange() {
        return this._onDidChange.event;
    }
    onDidChangeRepository({ repository }) {
        this.changedRepositoryRoots.add(repository.root);
        this.eventuallyFireChangeEvents();
    }
    eventuallyFireChangeEvents() {
        this.fireChangeEvents();
    }
    fireChangeEvents() {
        return __awaiter(this, void 0, void 0, function* () {
            if (!vscode_1.window.state.focused) {
                const onDidFocusWindow = util_1.filterEvent(vscode_1.window.onDidChangeWindowState, e => e.focused);
                yield util_1.eventToPromise(onDidFocusWindow);
            }
            Object.keys(this.cache).forEach(key => {
                const uri = this.cache[key].uri;
                const fsPath = uri.fsPath;
                for (const root of this.changedRepositoryRoots) {
                    if (util_1.isDescendant(root, fsPath)) {
                        this._onDidChange.fire(uri);
                        return;
                    }
                }
            });
            this.changedRepositoryRoots.clear();
        });
    }
    provideTextDocumentContent(uri) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const { fsPath, action, extra } = uri_1.fromSvnUri(uri);
                const repository = this.model.getRepository(fsPath);
                if (!repository) {
                    return "";
                }
                const cacheKey = uri.toString();
                const timestamp = new Date().getTime();
                const cacheValue = { uri, timestamp };
                this.cache[cacheKey] = cacheValue;
                if (action === types_1.SvnUriAction.SHOW) {
                    const ref = extra.ref;
                    return yield repository.show(fsPath, ref);
                }
                if (action === types_1.SvnUriAction.LOG) {
                    return yield repository.plainLog();
                }
                if (action === types_1.SvnUriAction.PATCH) {
                    return yield repository.patch([fsPath]);
                }
            }
            catch (error) {
                console.error(error);
            }
            return "";
        });
    }
    cleanup() {
        const now = new Date().getTime();
        const cache = Object.create(null);
        Object.keys(this.cache).forEach(key => {
            const row = this.cache[key];
            const { fsPath } = uri_1.fromSvnUri(row.uri);
            const isOpen = vscode_1.workspace.textDocuments
                .filter(d => d.uri.scheme === "file")
                .some(d => d.uri.fsPath === fsPath);
            if (isOpen || now - row.timestamp < THREE_MINUTES) {
                cache[row.uri.toString()] = row;
            }
        });
        this.cache = cache;
    }
    dispose() {
        this.disposables.forEach(d => d.dispose());
    }
}
__decorate([
    decorators_1.debounce(1100)
], SvnContentProvider.prototype, "eventuallyFireChangeEvents", null);
__decorate([
    decorators_1.throttle
], SvnContentProvider.prototype, "fireChangeEvents", null);
exports.SvnContentProvider = SvnContentProvider;
//# sourceMappingURL=svnContentProvider.js.map