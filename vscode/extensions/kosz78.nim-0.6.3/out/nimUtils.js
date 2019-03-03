/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const os = require("os");
const cp = require("child_process");
const vscode = require("vscode");
let _pathesCache = {};
var _projects = [];
function getNimExecPath() {
    let path = getBinPath('nim');
    if (!path) {
        vscode.window.showInformationMessage('No \'nim\' binary could be found in PATH environment variable');
    }
    return path;
}
exports.getNimExecPath = getNimExecPath;
/**
 * Returns true if path related to any workspace folders,
 *
 * @param filePath absolute file path
 */
function isWorkspaceFile(filePath) {
    if (vscode.workspace.workspaceFolders) {
        for (const wsFolder of vscode.workspace.workspaceFolders) {
            if (wsFolder.uri.scheme === 'file' &&
                filePath.toLowerCase().startsWith(wsFolder.uri.fsPath.toLowerCase())) {
                return true;
            }
        }
    }
    return false;
}
exports.isWorkspaceFile = isWorkspaceFile;
/**
 * Return project info from file path.
 *
 * @param filePath relative or absolite file path
 */
function toProjectInfo(filePath) {
    if (path.isAbsolute(filePath)) {
        let workspaceFolder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(filePath));
        if (workspaceFolder) {
            return { wsFolder: workspaceFolder, filePath: vscode.workspace.asRelativePath(filePath, false) };
        }
    }
    else {
        if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
            if (vscode.workspace.workspaceFolders.length === 1) {
                return { wsFolder: vscode.workspace.workspaceFolders[0], filePath: filePath };
            }
            else {
                let parsedPath = filePath.split('/');
                if (parsedPath.length > 1) {
                    for (const folder of vscode.workspace.workspaceFolders) {
                        if (parsedPath[0] === folder.name) {
                            return { wsFolder: folder, filePath: filePath.substr(parsedPath[0].length + 1) };
                        }
                    }
                }
            }
        }
    }
    let parsedPath = path.parse(filePath);
    return {
        wsFolder: {
            uri: vscode.Uri.file(parsedPath.dir),
            name: 'root',
            index: 0
        },
        filePath: parsedPath.base
    };
}
exports.toProjectInfo = toProjectInfo;
/**
 * Return project file in filesystem.
 *
 * @param project project file info
 */
function toLocalFile(project) {
    return project.wsFolder.uri.with({ path: project.wsFolder.uri.path + '/' + project.filePath }).fsPath;
}
exports.toLocalFile = toLocalFile;
/**
 * Returns full path to nimpretty executables or '' if file not found.
 */
function getNimPrettyExecPath() {
    let toolname = 'nimpretty';
    if (!_pathesCache[toolname]) {
        let nimPrettyPath = path.resolve(path.dirname(getNimExecPath()), correctBinname(toolname));
        if (fs.existsSync(nimPrettyPath)) {
            _pathesCache[toolname] = nimPrettyPath;
        }
        else {
            _pathesCache[toolname] = '';
        }
    }
    return _pathesCache[toolname];
}
exports.getNimPrettyExecPath = getNimPrettyExecPath;
/**
 * Returns full path to nimble executables or '' if file not found.
 */
function getNimbleExecPath() {
    let toolname = 'nimble';
    if (!_pathesCache[toolname]) {
        let nimblePath = path.resolve(path.dirname(getNimExecPath()), correctBinname(toolname));
        if (fs.existsSync(nimblePath)) {
            _pathesCache[toolname] = nimblePath;
        }
        else {
            _pathesCache[toolname] = '';
        }
    }
    return _pathesCache[toolname];
}
exports.getNimbleExecPath = getNimbleExecPath;
function getProjectFileInfo(filename) {
    if (!isProjectMode()) {
        return toProjectInfo(filename);
    }
    for (const project of _projects) {
        if (filename.startsWith(path.dirname(toLocalFile(project)))) {
            return project;
        }
    }
    return _projects[0];
}
exports.getProjectFileInfo = getProjectFileInfo;
/**
 * Returns temporary file path of edited document.
 */
function getDirtyFile(document) {
    var dirtyFilePath = path.normalize(path.join(os.tmpdir(), 'vscodenimdirty.nim'));
    fs.writeFileSync(dirtyFilePath, document.getText());
    return dirtyFilePath;
}
exports.getDirtyFile = getDirtyFile;
function isProjectMode() {
    return _projects.length > 0;
}
exports.isProjectMode = isProjectMode;
function getProjects() {
    return _projects;
}
exports.getProjects = getProjects;
function prepareConfig() {
    let config = vscode.workspace.getConfiguration('nim');
    let projects = config['project'];
    _projects = [];
    if (projects) {
        if (projects instanceof Array) {
            projects.forEach((project) => {
                _projects.push(toProjectInfo(project));
            });
        }
        else {
            vscode.workspace.findFiles(projects).then(result => {
                if (result && result.length > 0) {
                    _projects.push(toProjectInfo(result[0].fsPath));
                }
            });
        }
    }
}
exports.prepareConfig = prepareConfig;
function getBinPath(tool) {
    if (_pathesCache[tool])
        return _pathesCache[tool];
    if (process.env['PATH']) {
        var pathparts = process.env.PATH.split(path.delimiter);
        _pathesCache[tool] = pathparts.map(dir => path.join(dir, correctBinname(tool))).filter(candidate => fs.existsSync(candidate))[0];
        if (process.platform !== 'win32') {
            try {
                let nimPath;
                if (process.platform === 'darwin') {
                    nimPath = cp.execFileSync('readlink', [_pathesCache[tool]]).toString().trim();
                    if (nimPath.length > 0 && !path.isAbsolute(nimPath)) {
                        nimPath = path.normalize(path.join(path.dirname(_pathesCache[tool]), nimPath));
                    }
                }
                else if (process.platform === 'linux') {
                    nimPath = cp.execFileSync('readlink', ['-f', _pathesCache[tool]]).toString().trim();
                }
                else {
                    nimPath = cp.execFileSync('readlink', [_pathesCache[tool]]).toString().trim();
                }
                if (nimPath.length > 0) {
                    _pathesCache[tool] = nimPath;
                }
            }
            catch (e) {
                // ignore exception
            }
        }
    }
    return _pathesCache[tool];
}
exports.getBinPath = getBinPath;
function correctBinname(binname) {
    if (process.platform === 'win32') {
        return binname + '.exe';
    }
    else {
        return binname;
    }
}
exports.correctBinname = correctBinname;
function removeDirSync(p) {
    if (fs.existsSync(p)) {
        fs.readdirSync(p).forEach((file, index) => {
            var curPath = path.resolve(p, file);
            if (fs.lstatSync(curPath).isDirectory()) {
                removeDirSync(curPath);
            }
            else {
                fs.unlinkSync(curPath);
            }
        });
        fs.rmdirSync(p);
    }
}
exports.removeDirSync = removeDirSync;
let _channel;
function getOutputChannel() {
    if (!_channel) {
        _channel = vscode.window.createOutputChannel('Nim');
    }
    return _channel;
}
exports.getOutputChannel = getOutputChannel;
function padStart(len, input) {
    let out = '';
    for (let i = input.length; i < len; i++) {
        out += '0';
    }
    return out + input;
}
function cleanDateString(date) {
    let year = date.getFullYear();
    let month = padStart(2, date.getMonth().toString());
    let dd = padStart(2, date.getDate().toString());
    let hour = padStart(2, date.getHours().toString());
    let minute = padStart(2, date.getMinutes().toString());
    let second = padStart(2, date.getSeconds().toString());
    let millisecond = padStart(3, date.getMilliseconds().toString());
    return `${year}-${month}-${dd} ${hour}:${minute}:${second}.${millisecond}`;
}
/**
 * Prints message in Nim's output channel
 */
function outputLine(message) {
    let channel = getOutputChannel();
    let timeNow = new Date();
    channel.appendLine(`${cleanDateString(timeNow)} - ${message}`);
}
exports.outputLine = outputLine;
//# sourceMappingURL=nimUtils.js.map