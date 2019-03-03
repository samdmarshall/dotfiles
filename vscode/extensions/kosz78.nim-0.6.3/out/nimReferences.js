/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
class NimReferenceProvider {
    provideReferences(document, position, options, token) {
        return new Promise((resolve, reject) => {
            vscode.workspace.saveAll(false).then(() => {
                nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.use, document.fileName, position.line + 1, position.character, nimUtils_1.getDirtyFile(document))
                    .then(result => {
                    var references = [];
                    if (result) {
                        result.forEach(item => {
                            references.push(item.location);
                        });
                        resolve(references);
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
exports.NimReferenceProvider = NimReferenceProvider;
//# sourceMappingURL=nimReferences.js.map