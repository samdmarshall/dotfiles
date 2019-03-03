/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const nimUtils_1 = require("./nimUtils");
const nimIndexer_1 = require("./nimIndexer");
class NimWorkspaceSymbolProvider {
    provideWorkspaceSymbols(query, token) {
        return nimIndexer_1.findWorkspaceSymbols(query);
    }
}
exports.NimWorkspaceSymbolProvider = NimWorkspaceSymbolProvider;
class NimDocumentSymbolProvider {
    provideDocumentSymbols(document, token) {
        return nimIndexer_1.getFileSymbols(document.fileName, nimUtils_1.getDirtyFile(document));
    }
}
exports.NimDocumentSymbolProvider = NimDocumentSymbolProvider;
//# sourceMappingURL=nimOutline.js.map