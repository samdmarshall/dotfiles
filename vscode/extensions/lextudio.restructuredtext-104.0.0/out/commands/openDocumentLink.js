"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
const file_1 = require("../util/file");
class OpenDocumentLinkCommand {
    constructor() {
        this.id = OpenDocumentLinkCommand.id;
    }
    static createCommandUri(path, fragment) {
        return vscode.Uri.parse(`command:${OpenDocumentLinkCommand.id}?${encodeURIComponent(JSON.stringify({ path, fragment }))}`);
    }
    execute(args) {
        const p = decodeURIComponent(args.path);
        return this.tryOpen(p, args).catch(() => {
            if (path.extname(p) === '') {
                return this.tryOpen(p + '.md', args);
            }
            const resource = vscode.Uri.file(p);
            return Promise.resolve(void 0)
                .then(() => vscode.commands.executeCommand('vscode.open', resource))
                .then(() => void 0);
        });
    }
    async tryOpen(path, args) {
        const resource = vscode.Uri.file(path);
        if (vscode.window.activeTextEditor && file_1.isRSTFile(vscode.window.activeTextEditor.document) && vscode.window.activeTextEditor.document.uri.fsPath === resource.fsPath) {
            return this.tryRevealLine(vscode.window.activeTextEditor, args.fragment);
        }
        else {
            return vscode.workspace.openTextDocument(resource)
                .then(vscode.window.showTextDocument)
                .then(editor => this.tryRevealLine(editor, args.fragment));
        }
    }
    async tryRevealLine(editor, fragment) {
        if (editor && fragment) {
            const lineNumberFragment = fragment.match(/^L(\d+)$/i);
            if (lineNumberFragment) {
                const line = +lineNumberFragment[1] - 1;
                if (!isNaN(line)) {
                    return editor.revealRange(new vscode.Range(line, 0, line, 0), vscode.TextEditorRevealType.AtTop);
                }
            }
        }
    }
}
OpenDocumentLinkCommand.id = '_rst.openDocumentLink';
exports.OpenDocumentLinkCommand = OpenDocumentLinkCommand;
//# sourceMappingURL=openDocumentLink.js.map