"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
class SvnDecorationProvider {
    constructor(repository) {
        this.repository = repository;
        this._onDidChangeDecorations = new vscode_1.EventEmitter();
        this.onDidChangeDecorations = this
            ._onDidChangeDecorations.event;
        this.disposables = [];
        this.decorations = new Map();
        this.disposables.push(vscode_1.window.registerDecorationProvider(this), repository.onDidRunOperation(this.onDidRunOperation, this));
    }
    onDidRunOperation() {
        const newDecorations = new Map();
        this.collectDecorationData(this.repository.changes, newDecorations);
        this.collectDecorationData(this.repository.unversioned, newDecorations);
        this.collectDecorationData(this.repository.conflicts, newDecorations);
        this.repository.changelists.forEach((group, changelist) => {
            this.collectDecorationData(group, newDecorations);
        });
        const uris = [];
        newDecorations.forEach((value, uriString) => {
            if (this.decorations.has(uriString)) {
                this.decorations.delete(uriString);
            }
            else {
                uris.push(vscode_1.Uri.parse(uriString));
            }
        });
        this.decorations.forEach((value, uriString) => {
            uris.push(vscode_1.Uri.parse(uriString));
        });
        this.decorations = newDecorations;
        this._onDidChangeDecorations.fire(uris);
    }
    collectDecorationData(group, bucket) {
        group.resourceStates.forEach(r => {
            if (r.resourceDecoration) {
                bucket.set(r.resourceUri.toString(), r.resourceDecoration);
            }
        });
    }
    provideDecoration(uri) {
        return this.decorations.get(uri.toString());
    }
    dispose() {
        this.disposables.forEach(d => d.dispose());
    }
}
exports.default = SvnDecorationProvider;
//# sourceMappingURL=svnDecorationProvider.js.map