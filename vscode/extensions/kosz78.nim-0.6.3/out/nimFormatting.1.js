/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const cp = require("child_process");
const fs = require("fs");
const nimUtils_1 = require("./nimUtils");
class NimFormattingProvider {
    provideDocumentFormattingEdits(document, options, token) {
        return new Promise((resolve, reject) => {
            if (nimUtils_1.getNimPrettyExecPath() === '') {
                vscode.window.showInformationMessage('No \'nimpretty\' binary could be found in PATH environment variable');
                resolve([]);
            }
            else {
                let file = nimUtils_1.getDirtyFile(document);
                let res = cp.spawnSync(nimUtils_1.getNimPrettyExecPath(), ['--backup:OFF', file], { cwd: vscode.workspace.rootPath });
                if (res.status !== 0) {
                    reject(res.error);
                }
                else {
                    if (!fs.existsSync(file)) {
                        reject(file + ' file not found');
                    }
                    else {
                        let content = fs.readFileSync(file, 'utf-8');
                        let range = document.validateRange(new vscode.Range(new vscode.Position(0, 0), new vscode.Position(1000000, 1000000)));
                        resolve([vscode.TextEdit.replace(range, content)]);
                    }
                }
            }
        });
    }
}
exports.NimFormattingProvider = NimFormattingProvider;
//# sourceMappingURL=nimFormatting.1.js.map