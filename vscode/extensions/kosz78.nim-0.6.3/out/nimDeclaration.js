/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
class NimDefinitionProvider {
    provideDefinition(document, position, token) {
        return new Promise((resolve, reject) => {
            nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.def, document.fileName, position.line + 1, position.character, nimUtils_1.getDirtyFile(document)).then(result => {
                if (result && result.length > 0) {
                    let def = result.pop();
                    if (def) {
                        resolve(def.location);
                    }
                    else {
                        resolve(null);
                    }
                }
                else {
                    resolve(null);
                }
            }).catch(reason => reject(reason));
        });
    }
}
exports.NimDefinitionProvider = NimDefinitionProvider;
//# sourceMappingURL=nimDeclaration.js.map