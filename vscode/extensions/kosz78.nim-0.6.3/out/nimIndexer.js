/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const Datastore = require("nedb");
const path = require("path");
const fs = require("fs");
const nimSuggestExec_1 = require("./nimSuggestExec");
const nimStatus_1 = require("./nimStatus");
let dbVersion = 4;
var dbFiles;
var dbTypes;
/**
 * Returns workspace path from lowercase version of worspace path.
 * It is required for pathes in different cases,
 * because nim compiler on windows system returns all pathes converted in lowercase.
 * @param file lowercase workspace path
 */
function addWorkspaceFile(file) {
    indexFile(file);
}
exports.addWorkspaceFile = addWorkspaceFile;
function removeWorkspaceFile(file) {
    removeFromIndex(file);
}
exports.removeWorkspaceFile = removeWorkspaceFile;
function changeWorkspaceFile(file) {
    indexFile(file);
}
exports.changeWorkspaceFile = changeWorkspaceFile;
function initWorkspace(extensionPath) {
    return __awaiter(this, void 0, void 0, function* () {
        // remove old version of indexes
        cleanOldDb(extensionPath, 'files');
        cleanOldDb(extensionPath, 'types');
        dbTypes = new Datastore({ filename: path.join(extensionPath, getDbName('types', dbVersion)), autoload: true });
        dbTypes.persistence.setAutocompactionInterval(600000); // compact each 10 munites
        dbTypes.ensureIndex({ fieldName: 'workspace' });
        dbTypes.ensureIndex({ fieldName: 'file' });
        dbTypes.ensureIndex({ fieldName: 'timestamp' });
        dbTypes.ensureIndex({ fieldName: 'type' });
        dbFiles = new Datastore({ filename: path.join(extensionPath, getDbName('files', dbVersion)), autoload: true });
        dbFiles.persistence.setAutocompactionInterval(600000); // compact each 10 munites
        dbFiles.ensureIndex({ fieldName: 'file' });
        dbFiles.ensureIndex({ fieldName: 'timestamp' });
        if (!nimSuggestExec_1.getNimSuggestPath()) {
            return;
        }
        let urls = yield vscode.workspace.findFiles('**/*.nim', '');
        nimStatus_1.showNimProgress(`Indexing: ${urls.length}`);
        for (var i = 0; i < urls.length; i++) {
            let url = urls[i];
            let file = url.fsPath;
            let cnt = urls.length - i;
            if (cnt % 10 === 0) {
                nimStatus_1.updateNimProgress(`Indexing: ${cnt} of ${urls.length}`);
            }
            yield indexFile(file);
        }
        nimStatus_1.hideNimProgress();
    });
}
exports.initWorkspace = initWorkspace;
function findWorkspaceSymbols(query) {
    return new Promise((resolve, reject) => {
        try {
            let reg = new RegExp(query, 'i');
            dbTypes.find({ ws: vscode.workspace.rootPath, type: reg }).limit(100).exec((err, docs) => {
                let symbols = [];
                docs.forEach(doc => {
                    symbols.push(new vscode.SymbolInformation(doc.type, doc.kind, new vscode.Range(new vscode.Position(doc.range_start._line, doc.range_start._character), new vscode.Position(doc.range_end._line, doc.range_end._character)), vscode.Uri.file(doc.file), doc.container));
                });
                resolve(symbols);
            });
        }
        catch (e) {
            resolve([]);
        }
    });
}
exports.findWorkspaceSymbols = findWorkspaceSymbols;
function getFileSymbols(file, dirtyFile) {
    return new Promise((resolve, reject) => {
        nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.outline, file, 0, 0, dirtyFile)
            .then(result => {
            var symbols = [];
            var exists = [];
            if (result) {
                result.forEach(item => {
                    // skip let and var in proc and methods
                    if ((item.suggest === 'skLet' || item.suggest === 'skVar') && item.containerName.indexOf('.') > 0) {
                        return;
                    }
                    if (exists.indexOf(item.column + ':' + item.line) === -1) {
                        exists.push(item.column + ':' + item.line);
                        let symbolInfo = new vscode.SymbolInformation(item.symbolName, vscodeKindFromNimSym(item.suggest), item.range, item.uri, item.containerName);
                        symbols.push(symbolInfo);
                    }
                });
            }
            resolve(symbols);
        })
            .catch(reason => reject(reason));
    });
}
exports.getFileSymbols = getFileSymbols;
function findFile(file, timestamp) {
    return __awaiter(this, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            dbFiles.findOne({ file: file, timestamp: timestamp }, function (err, doc) { resolve(doc); });
        });
    });
}
function indexFile(file) {
    return __awaiter(this, void 0, void 0, function* () {
        let timestamp = fs.statSync(file).mtime.getTime();
        let doc = yield findFile(file, timestamp);
        if (!doc) {
            // console.log("index: " + file);
            let infos = yield getFileSymbols(file, '');
            if (infos && infos.length > 0) {
                dbFiles.remove({ file: file }, { multi: true }, (err, n) => {
                    dbFiles.insert({ file: file, timestamp: timestamp });
                });
                dbTypes.remove({ file: file }, { multi: true }, (err, n) => {
                    infos.forEach((value) => {
                        dbTypes.insert({
                            ws: vscode.workspace.rootPath,
                            file: value.location.uri.fsPath,
                            range_start: value.location.range.start,
                            range_end: value.location.range.end,
                            type: value.name,
                            container: value.containerName,
                            kind: value.kind
                        });
                    });
                });
            }
        }
        return;
    });
}
function vscodeKindFromNimSym(kind) {
    switch (kind) {
        case 'skConst':
            return vscode.SymbolKind.Constant;
        case 'skEnumField':
            return vscode.SymbolKind.Enum;
        case 'skForVar':
            return vscode.SymbolKind.Variable;
        case 'skIterator':
            return vscode.SymbolKind.Array;
        case 'skLabel':
            return vscode.SymbolKind.String;
        case 'skLet':
            return vscode.SymbolKind.Variable;
        case 'skMacro':
            return vscode.SymbolKind.Function;
        case 'skMethod':
            return vscode.SymbolKind.Method;
        case 'skParam':
            return vscode.SymbolKind.Variable;
        case 'skProc':
            return vscode.SymbolKind.Function;
        case 'skResult':
            return vscode.SymbolKind.Function;
        case 'skTemplate':
            return vscode.SymbolKind.Interface;
        case 'skType':
            return vscode.SymbolKind.Class;
        case 'skVar':
            return vscode.SymbolKind.Variable;
        case 'skFunc':
            return vscode.SymbolKind.Function;
    }
    return vscode.SymbolKind.Property;
}
function removeFromIndex(file) {
    dbFiles.remove({ file: file }, { multi: true }, function (err, doc) {
        dbTypes.remove({ file: file }, { multi: true });
    });
}
function cleanOldDb(basePath, name) {
    let dbPath = path.join(basePath, `${name}.db`);
    if (fs.existsSync(dbPath)) {
        fs.unlinkSync(dbPath);
    }
    for (var i = 0; i < dbVersion; ++i) {
        let dbPath = path.join(basePath, getDbName(name, i));
        if (fs.existsSync(dbPath)) {
            fs.unlinkSync(dbPath);
        }
    }
}
function getDbName(name, version) {
    return `${name}_${version}.db`;
}
//# sourceMappingURL=nimIndexer.js.map