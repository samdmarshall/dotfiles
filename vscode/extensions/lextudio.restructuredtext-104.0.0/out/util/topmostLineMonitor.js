"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const dispose_1 = require("../util/dispose");
const file_1 = require("./file");
class RSTFileTopmostLineMonitor {
    constructor() {
        this.disposables = [];
        this.pendingUpdates = new Map();
        this.throttle = 50;
        this._onDidChangeTopmostLineEmitter = new vscode.EventEmitter();
        this.onDidChangeTopmostLine = this._onDidChangeTopmostLineEmitter.event;
        vscode.window.onDidChangeTextEditorVisibleRanges(event => {
            if (file_1.isRSTFile(event.textEditor.document)) {
                const line = getVisibleLine(event.textEditor);
                if (typeof line === 'number') {
                    this.updateLine(event.textEditor.document.uri, line);
                }
            }
        }, null, this.disposables);
    }
    dispose() {
        dispose_1.disposeAll(this.disposables);
    }
    updateLine(resource, line) {
        const key = resource.toString();
        if (!this.pendingUpdates.has(key)) {
            // schedule update
            setTimeout(() => {
                if (this.pendingUpdates.has(key)) {
                    this._onDidChangeTopmostLineEmitter.fire({
                        resource,
                        line: this.pendingUpdates.get(key)
                    });
                    this.pendingUpdates.delete(key);
                }
            }, this.throttle);
        }
        this.pendingUpdates.set(key, line);
    }
}
exports.RSTFileTopmostLineMonitor = RSTFileTopmostLineMonitor;
/**
 * Get the top-most visible range of `editor`.
 *
 * Returns a fractional line number based the visible character within the line.
 * Floor to get real line number
 */
function getVisibleLine(editor) {
    if (!editor.visibleRanges.length) {
        return undefined;
    }
    const firstVisiblePosition = editor.visibleRanges[0].start;
    const lineNumber = firstVisiblePosition.line;
    const line = editor.document.lineAt(lineNumber);
    const progress = firstVisiblePosition.character / (line.text.length + 2);
    return lineNumber + progress;
}
exports.getVisibleLine = getVisibleLine;
//# sourceMappingURL=topmostLineMonitor.js.map