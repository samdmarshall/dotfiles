/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const nimMode_1 = require("./nimMode");
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
class NimHoverProvider {
    provideHover(document, position, token) {
        return new Promise((resolve, reject) => {
            nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.def, document.fileName, position.line + 1, position.character, nimUtils_1.getDirtyFile(document)).then(result => {
                if (result && result.length > 0) {
                    let def = result.pop();
                    let label = def.fullName;
                    if (def.type !== '')
                        label += ': ' + def.type;
                    let hoverLabel = { language: nimMode_1.NIM_MODE.language, value: label };
                    if (def.documentation !== '') {
                        resolve(new vscode.Hover([hoverLabel, def.documentation], def.range));
                    }
                    else {
                        resolve(new vscode.Hover(hoverLabel, def.range));
                    }
                }
                else {
                    resolve();
                }
            }).catch(reason => reject(reason));
        });
    }
}
exports.NimHoverProvider = NimHoverProvider;
//# sourceMappingURL=nimHover.js.map