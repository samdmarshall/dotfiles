/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
class NimSignatureHelpProvider {
    provideSignatureHelp(document, position, token) {
        return new Promise((resolve, reject) => {
            var filename = document.fileName;
            var currentArgument = 0;
            var identBeforeDot = '';
            {
                var lines = document.getText().split('\n');
                var cursorX = position.character - 1, cursorY = position.line;
                var line = lines[cursorY];
                var bracketsWithin = 0;
                while (line[cursorX] !== '(' || bracketsWithin !== 0) {
                    if ((line[cursorX] === ',' || line[cursorX] === ';') && bracketsWithin === 0)
                        currentArgument++;
                    else if (line[cursorX] === ')')
                        bracketsWithin++;
                    else if (line[cursorX] === '(')
                        bracketsWithin--;
                    cursorX--;
                    if (cursorX < 0) {
                        if (cursorY - 1 < 0) {
                            resolve();
                            return;
                        }
                        line = lines[--cursorY];
                    }
                }
                var dotPosition = -1, start = -1;
                while (cursorX >= 0) {
                    if (line[cursorX] === '.') {
                        dotPosition = cursorX;
                        break;
                    }
                    cursorX--;
                }
                while (cursorX >= 0 && dotPosition !== -1) {
                    if (line[cursorX].search('[ \t\({=]') !== -1) {
                        start = cursorX + 1;
                        break;
                    }
                    cursorX--;
                }
                if (start === -1)
                    start = 0;
                if (start !== -1) {
                    identBeforeDot = line.substring(start, dotPosition);
                }
            }
            nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.con, filename, position.line + 1, position.character, nimUtils_1.getDirtyFile(document))
                .then(items => {
                var signatures = new vscode.SignatureHelp();
                var isModule = 0;
                if (items && items.length > 0)
                    signatures.activeSignature = 0;
                if (items) {
                    items.forEach(item => {
                        var signature = new vscode.SignatureInformation(item.type, item.documentation);
                        var genericsCleanType = '';
                        {
                            var insideGeneric = 0;
                            for (var i = 0; i < item.type.length; i++) {
                                if (item.type[i] === '[')
                                    insideGeneric++;
                                if (!insideGeneric)
                                    genericsCleanType += item.type[i];
                                if (item.type[i] === ']')
                                    insideGeneric--;
                            }
                        }
                        var signatureCutDown = /(proc|macro|template|iterator|func) \((.+: .+)*\)/.exec(genericsCleanType);
                        if (signatureCutDown) {
                            var parameters = signatureCutDown[2].split(', ');
                            parameters.forEach(parameter => {
                                signature.parameters.push(new vscode.ParameterInformation(parameter));
                            });
                        }
                        if (item.names[0] === identBeforeDot || item.path.search('/' + identBeforeDot + '/') !== -1 || item.path.search('\\\\' + identBeforeDot + '\\\\') !== -1)
                            isModule++;
                        signatures.signatures.push(signature);
                    });
                }
                signatures.activeParameter = isModule > 0 || identBeforeDot === '' ? currentArgument : currentArgument + 1;
                resolve(signatures);
            }).catch(reason => reject(reason));
        });
    }
}
exports.NimSignatureHelpProvider = NimSignatureHelpProvider;
//# sourceMappingURL=nimSignature.js.map