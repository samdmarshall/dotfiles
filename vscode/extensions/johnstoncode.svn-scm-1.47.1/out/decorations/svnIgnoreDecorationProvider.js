"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
const decorators_1 = require("../decorators");
const util_1 = require("../util");
class SvnIgnoreDecorationProvider {
    constructor(repository) {
        this.repository = repository;
        this._onDidChangeDecorations = new vscode_1.EventEmitter();
        this.onDidChangeDecorations = this
            ._onDidChangeDecorations.event;
        this.checkIgnoreQueue = new Map();
        this.disposables = [];
        this.disposables.push(vscode_1.window.registerDecorationProvider(this), repository.onDidChangeStatus(_ => this._onDidChangeDecorations.fire()));
    }
    dispose() {
        this.disposables.forEach(d => d.dispose());
        this.checkIgnoreQueue.clear();
    }
    provideDecoration(uri) {
        return new Promise((resolve, reject) => {
            this.checkIgnoreQueue.set(uri.fsPath, { resolve, reject });
            this.checkIgnoreSoon();
        }).then(ignored => {
            if (ignored) {
                return {
                    priority: 3,
                    color: new vscode_1.ThemeColor("gitDecoration.ignoredResourceForeground")
                };
            }
        });
    }
    checkIgnoreSoon() {
        const queue = new Map(this.checkIgnoreQueue.entries());
        this.checkIgnoreQueue.clear();
        const ignored = this.repository.statusIgnored;
        const external = this.repository.statusExternal;
        const files = ignored.map(stat => path.join(this.repository.workspaceRoot, stat.path));
        files.push(...external.map(stat => path.join(this.repository.workspaceRoot, stat.path)));
        for (const [key, value] of queue.entries()) {
            value.resolve(files.some(file => util_1.isDescendant(file, key)));
        }
    }
}
__decorate([
    decorators_1.debounce(500)
], SvnIgnoreDecorationProvider.prototype, "checkIgnoreSoon", null);
exports.default = SvnIgnoreDecorationProvider;
//# sourceMappingURL=svnIgnoreDecorationProvider.js.map