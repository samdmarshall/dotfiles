"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const dispose_1 = require("../util/dispose");
const file_1 = require("../util/file");
const lazy_1 = require("../util/lazy");
class VSCodeWorkspaceReStructuredTextDocumentProvider {
    constructor() {
        this._onDidChangeReStructuredTextDocumentEmitter = new vscode.EventEmitter();
        this._onDidCreateReStructuredTextDocumentEmitter = new vscode.EventEmitter();
        this._onDidDeleteReStructuredTextDocumentEmitter = new vscode.EventEmitter();
        this._disposables = [];
    }
    dispose() {
        this._onDidChangeReStructuredTextDocumentEmitter.dispose();
        this._onDidDeleteReStructuredTextDocumentEmitter.dispose();
        if (this._watcher) {
            this._watcher.dispose();
        }
        dispose_1.disposeAll(this._disposables);
    }
    async getAllReStructuredTextDocuments() {
        const resources = await vscode.workspace.findFiles('**/*.md', '**/node_modules/**');
        const docs = await Promise.all(resources.map(doc => this.getReStructuredTextDocument(doc)));
        return docs.filter(doc => !!doc);
    }
    get onDidChangeReStructuredTextDocument() {
        this.ensureWatcher();
        return this._onDidChangeReStructuredTextDocumentEmitter.event;
    }
    get onDidCreateReStructuredTextDocument() {
        this.ensureWatcher();
        return this._onDidCreateReStructuredTextDocumentEmitter.event;
    }
    get onDidDeleteReStructuredTextDocument() {
        this.ensureWatcher();
        return this._onDidDeleteReStructuredTextDocumentEmitter.event;
    }
    ensureWatcher() {
        if (this._watcher) {
            return;
        }
        this._watcher = vscode.workspace.createFileSystemWatcher('**/*.md');
        this._watcher.onDidChange(async (resource) => {
            const document = await this.getReStructuredTextDocument(resource);
            if (document) {
                this._onDidChangeReStructuredTextDocumentEmitter.fire(document);
            }
        }, null, this._disposables);
        this._watcher.onDidCreate(async (resource) => {
            const document = await this.getReStructuredTextDocument(resource);
            if (document) {
                this._onDidCreateReStructuredTextDocumentEmitter.fire(document);
            }
        }, null, this._disposables);
        this._watcher.onDidDelete(async (resource) => {
            this._onDidDeleteReStructuredTextDocumentEmitter.fire(resource);
        }, null, this._disposables);
        vscode.workspace.onDidChangeTextDocument(e => {
            if (file_1.isRSTFile(e.document)) {
                this._onDidChangeReStructuredTextDocumentEmitter.fire(e.document);
            }
        }, null, this._disposables);
    }
    async getReStructuredTextDocument(resource) {
        const doc = await vscode.workspace.openTextDocument(resource);
        return doc && file_1.isRSTFile(doc) ? doc : undefined;
    }
}
class ReStructuredTextWorkspaceSymbolProvider {
    constructor(_symbolProvider, _workspaceReStructuredTextDocumentProvider = new VSCodeWorkspaceReStructuredTextDocumentProvider()) {
        this._symbolProvider = _symbolProvider;
        this._workspaceReStructuredTextDocumentProvider = _workspaceReStructuredTextDocumentProvider;
        this._symbolCache = new Map();
        this._symbolCachePopulated = false;
        this._disposables = [];
    }
    async provideWorkspaceSymbols(query) {
        if (!this._symbolCachePopulated) {
            await this.populateSymbolCache();
            this._symbolCachePopulated = true;
            this._workspaceReStructuredTextDocumentProvider.onDidChangeReStructuredTextDocument(this.onDidChangeDocument, this, this._disposables);
            this._workspaceReStructuredTextDocumentProvider.onDidCreateReStructuredTextDocument(this.onDidChangeDocument, this, this._disposables);
            this._workspaceReStructuredTextDocumentProvider.onDidDeleteReStructuredTextDocument(this.onDidDeleteDocument, this, this._disposables);
        }
        const allSymbolsSets = await Promise.all(Array.from(this._symbolCache.values()).map(x => x.value));
        const allSymbols = Array.prototype.concat.apply([], allSymbolsSets);
        return allSymbols.filter(symbolInformation => symbolInformation.name.toLowerCase().indexOf(query.toLowerCase()) !== -1);
    }
    async populateSymbolCache() {
        const reStructuredTextDocumentUris = await this._workspaceReStructuredTextDocumentProvider.getAllReStructuredTextDocuments();
        for (const document of reStructuredTextDocumentUris) {
            this._symbolCache.set(document.uri.fsPath, this.getSymbols(document));
        }
    }
    dispose() {
        dispose_1.disposeAll(this._disposables);
    }
    getSymbols(document) {
        return lazy_1.lazy(async () => {
            return this._symbolProvider.provideDocumentSymbolInformation(document);
        });
    }
    onDidChangeDocument(document) {
        this._symbolCache.set(document.uri.fsPath, this.getSymbols(document));
    }
    onDidDeleteDocument(resource) {
        this._symbolCache.delete(resource.fsPath);
    }
}
exports.default = ReStructuredTextWorkspaceSymbolProvider;
//# sourceMappingURL=workspaceSymbolProvider.js.map