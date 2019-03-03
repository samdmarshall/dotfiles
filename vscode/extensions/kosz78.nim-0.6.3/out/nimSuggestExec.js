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
const cp = require("child_process");
const path = require("path");
const fs = require("fs");
const elrpc = require("./elrpc/elrpc");
const nimUtils_1 = require("./nimUtils");
class NimSuggestProcessDescription {
}
let nimSuggestProcessCache = {};
var _nimSuggestPath = '';
var _nimSuggestVersion = '';
var NimSuggestType;
(function (NimSuggestType) {
    /** Suggest from position */
    NimSuggestType[NimSuggestType["sug"] = 0] = "sug";
    /** Get context from position */
    NimSuggestType[NimSuggestType["con"] = 1] = "con";
    /** Get symbol definition from position */
    NimSuggestType[NimSuggestType["def"] = 2] = "def";
    /** Get references of symbol from position */
    NimSuggestType[NimSuggestType["use"] = 3] = "use";
    /** Get usage of symbol from position in project */
    NimSuggestType[NimSuggestType["dus"] = 4] = "dus";
    /** Ivoke nim check on file */
    NimSuggestType[NimSuggestType["chk"] = 5] = "chk";
    /** Returns all tokens in file (symbolType, line, pos, lenght) */
    NimSuggestType[NimSuggestType["highlight"] = 6] = "highlight";
    /** Get outline symbols for file */
    NimSuggestType[NimSuggestType["outline"] = 7] = "outline";
    /** Returns 'true' if given file is related to the project, otherwise 'false'  */
    NimSuggestType[NimSuggestType["known"] = 8] = "known";
})(NimSuggestType = exports.NimSuggestType || (exports.NimSuggestType = {}));
/**
 * Parsed string line from nimsuggest utility.
 */
class NimSuggestResult {
    constructor() {
        /** Three characters indicating the type of returned answer
         * (e.g. def for definition, sug for suggestion, etc). */
        this.answerType = '';
        /** Type of the symbol. This can be skProc, skLet, and just
         *  about any of the enums defined in the module compiler/ast.nim. */
        this.suggest = '';
        /** Full qualitifed path of the symbol.If you are querying a symbol
         * defined in the proj.nim file, this would have the form [proj, symbolName]. */
        this.names = [];
        /** Type / signature.For variables and enums this will contain the type
         * of the symbol, for procs, methods and templates this will contain the
         * full unique signature (e.g.proc(File)). */
        this.type = '';
        /** Full path to the file containing the symbol. */
        this.path = '';
        /** Line where the symbol is located in the file.Lines start to count at 1. */
        this.line = 1;
        /** Column where the symbol is located in the file.Columns start to count at 0. */
        this.column = 0;
        /** Docstring for the symbol if available or the empty string.
         * To differentiate the docstring from end of answer in server mode,
         * the docstring is always provided enclosed in double quotes, and if
         * the docstring spans multiple lines, all following lines of the docstring
         * will start with a blank space to align visually with the starting quote.
         * //
         * Also, you won't find raw \n characters breaking the one answer per line format.
         * Instead you will need to parse sequences in the form \xHH, where HH
         * is a hexadecimal value (e.g. newlines generate the sequence \x0A). */
        this.documentation = '';
    }
    get range() {
        return new vscode.Range(this.line - 1, this.column, this.line - 1, this.column);
    }
    get position() {
        return new vscode.Position(this.line - 1, this.column);
    }
    get uri() {
        return vscode.Uri.file(this.path);
    }
    get location() {
        return new vscode.Location(this.uri, this.position);
    }
    get fullName() {
        return this.names ? this.names.join('.') : '';
    }
    get symbolName() {
        return this.names ? this.names[this.names.length - 1] : '';
    }
    get moduleName() {
        return this.names ? this.names[0] : '';
    }
    get containerName() {
        return this.names ? this.names.slice(0, this.names.length - 1).join('.') : '';
    }
}
exports.NimSuggestResult = NimSuggestResult;
function getNimSuggestPath() {
    return _nimSuggestPath;
}
exports.getNimSuggestPath = getNimSuggestPath;
function getNimSuggestVersion() {
    return _nimSuggestVersion;
}
exports.getNimSuggestVersion = getNimSuggestVersion;
/**
 * Returns true if nimsuggest version is great or equals to given.
 * @param version version to match
 */
function isNimSuggestVersion(version) {
    if (!_nimSuggestVersion) {
        return false;
    }
    let nimVersionParts = _nimSuggestVersion.split('.');
    let versionParts = version.split('.');
    for (var i = 0; i < Math.min(nimVersionParts.length, versionParts.length); i++) {
        let diff = parseInt(nimVersionParts[i]) - parseInt(versionParts[i]);
        if (diff === 0) {
            continue;
        }
        return diff > 0;
    }
    return true;
}
exports.isNimSuggestVersion = isNimSuggestVersion;
function initNimSuggest(ctx) {
    nimUtils_1.prepareConfig();
    // let check nimsuggest related nim executable
    let nimSuggestNewPath = path.resolve(path.dirname(nimUtils_1.getNimExecPath()), nimUtils_1.correctBinname('nimsuggest'));
    if (fs.existsSync(nimSuggestNewPath)) {
        _nimSuggestPath = nimSuggestNewPath;
        let versionOutput = cp.spawnSync(getNimSuggestPath(), ['--version'], { cwd: vscode.workspace.rootPath }).output.toString();
        let versionArgs = /.+Version\s([\d|\.]+)\s\(.+/g.exec(versionOutput);
        if (versionArgs && versionArgs.length === 2) {
            _nimSuggestVersion = versionArgs[1];
        }
        console.log(versionOutput);
        console.log('Nimsuggest version: ' + _nimSuggestVersion);
    }
    vscode.workspace.onDidChangeConfiguration(nimUtils_1.prepareConfig);
}
exports.initNimSuggest = initNimSuggest;
function trace(pid, projectFile, msg) {
    if (!!vscode.workspace.getConfiguration('nim').get('logNimsuggest')) {
        if (typeof projectFile === 'string') {
            console.log('[' + pid + ':' + projectFile + ']');
        }
        else {
            console.log('[' + pid + ':' + projectFile.wsFolder.name + ':' + projectFile.wsFolder.uri.fsPath + ':' + projectFile.filePath + ']');
        }
        console.log(msg);
    }
}
function execNimSuggest(suggestType, filename, line, column, dirtyFile) {
    return __awaiter(this, void 0, void 0, function* () {
        var nimSuggestExec = getNimSuggestPath();
        // if nimsuggest not found just ignore
        if (!nimSuggestExec) {
            return [];
        }
        // Dont run nimsuggest for nims files
        // See https://github.com/pragmagic/vscode-nim/issues/84
        if (path.extname(filename).toLowerCase() === '.nims') {
            return [];
        }
        let projectFile = nimUtils_1.getProjectFileInfo(filename);
        try {
            let normalizedFilename = filename.replace(/\\+/g, '/');
            let desc = yield getNimSuggestProcess(projectFile);
            let suggestCmd = NimSuggestType[suggestType];
            if (desc && desc.process) {
                trace(desc.process.pid, projectFile, suggestCmd + ' ' + normalizedFilename + ':' + line + ':' + column);
            }
            var result = [];
            if (desc && desc.rpc) {
                let ret = yield desc.rpc.callMethod(suggestCmd, { kind: 'string', str: normalizedFilename }, { kind: 'number', n: line }, { kind: 'number', n: column }, { kind: 'string', str: dirtyFile });
                if (desc.process) {
                    trace(desc.process.pid, nimUtils_1.toLocalFile(projectFile) + '=' + suggestCmd + ' ' + normalizedFilename, ret);
                }
                if (ret != null) {
                    if (ret instanceof Array) {
                        for (var i = 0; i < ret.length; i++) {
                            var parts = ret[i];
                            if (parts.length >= 8) {
                                var item = new NimSuggestResult();
                                item.answerType = parts[0];
                                item.suggest = parts[1];
                                item.names = parts[2];
                                item.path = parts[3].replace(/\\,\\/g, '\\');
                                item.type = parts[4];
                                item.line = parts[5];
                                item.column = parts[6];
                                var doc = parts[7];
                                if (doc !== '') {
                                    doc = doc.replace(/\`\`/g, '`');
                                    doc = doc.replace(/\.\. code-block:: (\w+)\r?\n(( .*\r?\n?)+)/g, '```$1\n$2\n```\n');
                                    doc = doc.replace(/\`([^\<\`]+)\<([^\>]+)\>\`\_/g, '\[$1\]\($2\)');
                                }
                                item.documentation = doc;
                                result.push(item);
                            }
                        }
                    }
                    else if (ret === 'EPC Connection closed') {
                        console.error(ret);
                        yield closeNimSuggestProcess(projectFile);
                    }
                    else {
                        var res = new NimSuggestResult();
                        res.suggest = '' + ret;
                        result.push(res);
                    }
                }
            }
            if (!nimUtils_1.isProjectMode() &&
                vscode.window.visibleTextEditors.every((value, index, array) => { return value.document.uri.fsPath !== filename; })) {
                yield closeNimSuggestProcess(projectFile);
            }
            return result;
        }
        catch (e) {
            console.error(e);
            yield closeNimSuggestProcess(projectFile);
        }
    });
}
exports.execNimSuggest = execNimSuggest;
function closeAllNimSuggestProcesses() {
    return __awaiter(this, void 0, void 0, function* () {
        console.log('Close all nimsuggest processes');
        for (var project in nimSuggestProcessCache) {
            let desc = yield nimSuggestProcessCache[project];
            if (desc) {
                if (desc.rpc) {
                    desc.rpc.stop();
                }
                if (desc.process) {
                    desc.process.kill();
                }
            }
        }
        nimSuggestProcessCache = {};
    });
}
exports.closeAllNimSuggestProcesses = closeAllNimSuggestProcesses;
function closeNimSuggestProcess(project) {
    return __awaiter(this, void 0, void 0, function* () {
        var file = nimUtils_1.toLocalFile(project);
        if (nimSuggestProcessCache[file]) {
            let desc = yield nimSuggestProcessCache[file];
            if (desc) {
                if (desc.rpc) {
                    desc.rpc.stop();
                }
                if (desc.process) {
                    desc.process.kill();
                }
            }
            nimSuggestProcessCache[file] = undefined;
        }
    });
}
exports.closeNimSuggestProcess = closeNimSuggestProcess;
function getNimSuggestProcess(nimProject) {
    return __awaiter(this, void 0, void 0, function* () {
        let projectPath = nimUtils_1.toLocalFile(nimProject);
        if (!nimSuggestProcessCache[projectPath]) {
            nimSuggestProcessCache[projectPath] = new Promise((resolve, reject) => {
                let nimConfig = vscode.workspace.getConfiguration('nim');
                var args = ['--epc', '--v2'];
                if (!!nimConfig['logNimsuggest']) {
                    args.push('--log');
                }
                if (!!nimConfig['useNimsuggestCheck']) {
                    args.push('--refresh:on');
                }
                args.push(nimProject.filePath);
                let process = cp.spawn(getNimSuggestPath(), args, { cwd: nimProject.wsFolder.uri.fsPath });
                process.stdout.once('data', (data) => {
                    let dataStr = data.toString();
                    let portNumber = parseInt(dataStr);
                    if (isNaN(portNumber)) {
                        reject('Nimsuggest returns unknown port number: ' + dataStr);
                    }
                    else {
                        elrpc.startClient(portNumber).then((peer) => {
                            resolve({ process: process, rpc: peer });
                        });
                    }
                });
                process.stdout.once('data', (data) => {
                    console.log(data.toString());
                });
                process.stderr.once('data', (data) => {
                    console.log(data.toString());
                });
                process.on('close', (code, signal) => {
                    if (code !== 0) {
                        console.error('nimsuggest closed with code: ' + code + ', signal: ' + signal);
                    }
                    if (nimSuggestProcessCache[projectPath]) {
                        nimSuggestProcessCache[projectPath].then((desc) => {
                            if (desc && desc.rpc) {
                                desc.rpc.stop();
                            }
                        });
                    }
                    reject();
                });
            });
        }
        return nimSuggestProcessCache[projectPath];
    });
}
//# sourceMappingURL=nimSuggestExec.js.map