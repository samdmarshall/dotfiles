/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const cp = require("child_process");
const os = require("os");
const nimUtils_1 = require("./nimUtils");
const nimSuggestExec_1 = require("./nimSuggestExec");
let executors = {};
function nimExec(project, command, args, useStdErr, callback) {
    return new Promise((resolve, reject) => {
        if (!nimUtils_1.getNimExecPath()) {
            return resolve([]);
        }
        let projectPath = nimUtils_1.toLocalFile(project);
        if (executors[projectPath] && executors[projectPath].initialized) {
            let ps = executors[projectPath].process;
            executors[projectPath] = { initialized: false, process: undefined };
            if (ps) {
                ps.kill('SIGKILL');
            }
        }
        else {
            executors[projectPath] = { initialized: false, process: undefined };
        }
        let executor = cp.spawn(nimUtils_1.getNimExecPath(), [command, ...args], { cwd: project.wsFolder.uri.fsPath });
        executors[projectPath].process = executor;
        executors[projectPath].initialized = true;
        executor.on('error', (err) => {
            if (err && err.code === 'ENOENT') {
                vscode.window.showInformationMessage('No \'nim\' binary could be found in PATH: \'' + process.env['PATH'] + '\'');
                return resolve([]);
            }
        });
        executor.stdout.on('data', data => {
            nimUtils_1.outputLine('[info] nim check output:\n' + data.toString());
        });
        var out = '';
        executor.on('exit', (code, signal) => {
            if (signal === 'SIGKILL') {
                reject([]);
            }
            else {
                executors[projectPath] = { initialized: false, process: undefined };
                try {
                    let split = out.split(os.EOL);
                    if (split.length === 1) {
                        var lfSplit = split[0].split('\n');
                        if (lfSplit.length > split.length)
                            split = lfSplit;
                    }
                    var ret = callback(split);
                    resolve(ret);
                }
                catch (e) {
                    reject(e);
                }
            }
        });
        if (useStdErr) {
            executor.stderr.on('data', (data) => {
                out += data.toString();
            });
        }
        else {
            executor.stdout.on('data', (data) => {
                out += data.toString();
            });
        }
    });
}
function parseErrors(lines) {
    var ret = [];
    var messageText = '';
    var lastFile = { file: '', column: '', line: '' };
    for (var i = 0; i < lines.length; i++) {
        let line = lines[i].trim();
        if (line.startsWith('Hint:')) {
            continue;
        }
        let match = /^([^(]*)?\((\d+)(,\s(\d+))?\)( (\w+):)? (.*)/.exec(line);
        if (!match) {
            if (messageText.length < 1024) {
                messageText += os.EOL + line;
            }
        }
        else {
            let [, file, lineStr, , charStr, , severity, msg] = match;
            if (msg === 'template/generic instantiation from here') {
                if (nimUtils_1.isWorkspaceFile(file)) {
                    lastFile = { file: file, column: charStr, line: lineStr };
                }
            }
            else {
                if (messageText !== '' && ret.length > 0) {
                    ret[ret.length - 1].msg += os.EOL + messageText;
                }
                messageText = '';
                if (nimUtils_1.isWorkspaceFile(file)) {
                    ret.push({ file: file, line: parseInt(lineStr), column: parseInt(charStr), msg: msg, severity });
                }
                else if (lastFile.file !== '') {
                    ret.push({ file: lastFile.file, line: parseInt(lastFile.line), column: parseInt(lastFile.column), msg: msg, severity });
                }
                lastFile = { file: '', column: '', line: '' };
            }
        }
    }
    if (messageText !== '' && ret.length > 0) {
        ret[ret.length - 1].msg += os.EOL + messageText;
    }
    return ret;
}
function parseNimsuggestErrors(items) {
    var ret = [];
    for (var i = 0; i < items.length; i++) {
        let item = items[i];
        if (item.path === '???' && item.type === 'Hint') {
            continue;
        }
        ret.push({ file: item.path, line: item.line, column: item.column, msg: item.documentation, severity: item.type });
    }
    return ret;
}
function check(filename, nimConfig) {
    var runningToolsPromises = [];
    if (!!nimConfig['useNimsuggestCheck']) {
        runningToolsPromises.push(new Promise((resolve, reject) => {
            nimSuggestExec_1.execNimSuggest(nimSuggestExec_1.NimSuggestType.chk, filename, 0, 0, '').then(items => {
                if (items && items.length > 0) {
                    resolve(parseNimsuggestErrors(items));
                }
                else {
                    resolve([]);
                }
            }).catch(reason => reject(reason));
        }));
    }
    else {
        if (!nimUtils_1.isProjectMode()) {
            let project = nimUtils_1.getProjectFileInfo(filename);
            runningToolsPromises.push(nimExec(project, 'check', ['--listFullPaths', project.filePath], true, parseErrors));
        }
        else {
            nimUtils_1.getProjects().forEach(project => {
                runningToolsPromises.push(nimExec(project, 'check', ['--listFullPaths', project.filePath], true, parseErrors));
            });
        }
    }
    return Promise.all(runningToolsPromises).then(resultSets => [].concat.apply([], resultSets));
}
exports.check = check;
var evalTerminal;
function activateEvalConsole() {
    vscode.window.onDidCloseTerminal((e) => {
        if (evalTerminal && e.processId === evalTerminal.processId) {
            evalTerminal = undefined;
        }
    });
}
exports.activateEvalConsole = activateEvalConsole;
function execSelectionInTerminal(document) {
    if (vscode.window.activeTextEditor) {
        if (!nimUtils_1.getNimExecPath()) {
            return;
        }
        if (!evalTerminal) {
            evalTerminal = vscode.window.createTerminal('Nim Console');
            evalTerminal.show(true);
            evalTerminal.sendText(nimUtils_1.getNimExecPath() + ' ' + 'secret\n');
        }
        evalTerminal.sendText(vscode.window.activeTextEditor.document.getText(vscode.window.activeTextEditor.selection));
        // evalTerminal.sendText('\n');
    }
}
exports.execSelectionInTerminal = execSelectionInTerminal;
//# sourceMappingURL=nimBuild.js.map