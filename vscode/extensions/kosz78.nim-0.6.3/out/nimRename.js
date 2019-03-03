/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
class NimRenameProvider {
    provideRenameEdits(document, position, newName, token) {
        return new Promise((resolve, reject) => {
            vscode.workspace.saveAll(false).then(() => {
                nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.use, document.fileName, position.line + 1, position.character, nimUtils_1.getDirtyFile(document))
                    .then(result => {
                    let edit = new vscode.WorkspaceEdit();
                    if (result) {
                        result.forEach(item => {
                            if (!path.isAbsolute(vscode.workspace.asRelativePath(item.location.uri))) {
                                let range = new vscode.Range(item.position, item.position.translate(0, item.symbolName.length));
                                edit.replace(item.location.uri, range, newName);
                            }
                        });
                        resolve(edit);
                    }
                    else {
                        resolve();
                    }
                })
                    .catch(reason => reject(reason));
            });
        });
    }
}
exports.NimRenameProvider = NimRenameProvider;
//# sourceMappingURL=nimRename.js.map