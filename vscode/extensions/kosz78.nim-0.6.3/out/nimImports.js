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
const fs = require("fs");
const path = require("path");
const nimUtils_1 = require("./nimUtils");
class NimbleModuleInfo {
}
class NimModuleInfo {
}
var nimbleModules = [];
var nimModules = {};
function getNimDirectories(projectDir, projectFile) {
    return new Promise((resolve, reject) => {
        cp.exec(nimUtils_1.getNimExecPath() + ' dump ' + projectFile, { cwd: projectDir }, (error, stdout, stderr) => {
            var res = [];
            let parts = stderr.split('\n');
            for (const part of parts) {
                let p = part.trim();
                if (p.indexOf('Hint: ') !== 0 && p.length > 0) {
                    res.push(p);
                }
            }
            resolve(res);
        });
    });
}
function createNimModule(projectDir, rootDir, dir, file) {
    let fullPath = path.join(dir, file);
    var nimModule = new NimModuleInfo();
    nimModule.name = file.substr(0, file.length - 4);
    if (dir.length > rootDir.length) {
        let moduleDir = dir.substr(rootDir.length + 1).replace(path.sep, '.');
        nimModule.fullName = moduleDir + '.' + nimModule.name;
    }
    else {
        nimModule.fullName = nimModule.name;
    }
    nimModule.path = fullPath;
    return nimModule;
}
function walkDir(projectDir, rootDir, dir, singlePass) {
    fs.readdir(dir, (err, files) => {
        if (files) {
            for (const file of files) {
                let fullPath = path.join(dir, file);
                if (fs.statSync(fullPath).isDirectory()) {
                    if (!singlePass) {
                        walkDir(projectDir, rootDir, fullPath, false);
                    }
                }
                else if (file.toLowerCase().endsWith('.nim')) {
                    nimModules[projectDir].push(createNimModule(projectDir, rootDir, dir, file));
                }
            }
        }
    });
}
function initNimDirectories(projectDir, projectFile) {
    return __awaiter(this, void 0, void 0, function* () {
        if (!nimModules[projectDir]) {
            nimModules[projectDir] = [];
            let nimDirectories = yield getNimDirectories(projectDir, projectFile);
            let nimRoot = path.dirname(path.dirname(nimUtils_1.getNimExecPath()));
            for (const dirPath of nimDirectories) {
                walkDir(projectDir, dirPath, dirPath, dirPath.startsWith(nimRoot));
            }
        }
    });
}
function getNimbleModules(rootDir) {
    return new Promise((resolve, reject) => {
        cp.exec(nimUtils_1.getNimbleExecPath() + ' list -i', { cwd: rootDir }, (error, stdout, stderr) => {
            var res = [];
            let parts = stdout.split('\n');
            for (const part of parts) {
                let p = part.split('[')[0].trim();
                if (p.length > 0 && p !== 'compiler') {
                    res.push(p);
                }
            }
            resolve(res);
        });
    });
}
function initNimbleModules(rootDir) {
    return __awaiter(this, void 0, void 0, function* () {
        let nimbleModuleNames = yield getNimbleModules(rootDir);
        for (const moduleName of nimbleModuleNames) {
            try {
                let out = cp.execSync(nimUtils_1.getNimbleExecPath() + ' --y dump ' + moduleName, { cwd: rootDir }).toString();
                var nimbleModule = new NimbleModuleInfo();
                nimbleModule.name = moduleName;
                for (const line of out.split(/\n/)) {
                    let pairs = line.trim().split(': "');
                    if (pairs.length === 2) {
                        let value = pairs[1].substring(0, pairs[1].length - 1);
                        if (pairs[0] === 'author') {
                            nimbleModule.author = value;
                        }
                        else if (pairs[0] === 'version') {
                            nimbleModule.version = value;
                        }
                        else if (pairs[0] === 'desc') {
                            nimbleModule.description = value;
                        }
                    }
                }
                nimbleModules.push(nimbleModule);
            }
            catch (_a) {
                console.log('Module incorrect ' + moduleName);
            }
        }
    });
}
function getImports(prefix, projectDir) {
    var suggestions = [];
    for (const nimbleModule of nimbleModules) {
        if (!prefix || nimbleModule.name.startsWith(prefix)) {
            var suggestion = new vscode.CompletionItem(nimbleModule.name);
            suggestion.kind = vscode.CompletionItemKind.Module;
            if (nimbleModule.version) {
                suggestion.detail = nimbleModule.name + ' [' + nimbleModule.version + ']';
            }
            else {
                suggestion.detail = nimbleModule.name;
            }
            suggestion.detail += ' (Nimble)';
            var doc = '**Name**: ' + nimbleModule.name;
            if (nimbleModule.version) {
                doc += '\n\n**Version**: ' + nimbleModule.version;
            }
            if (nimbleModule.author) {
                doc += '\n\n**Author**: ' + nimbleModule.author;
            }
            if (nimbleModule.description) {
                doc += '\n\n**Description**: ' + nimbleModule.description;
            }
            suggestion.documentation = new vscode.MarkdownString(doc);
            suggestions.push(suggestion);
        }
        if (suggestions.length >= 20) {
            return suggestions;
        }
    }
    if (nimModules[projectDir]) {
        for (const nimModule of nimModules[projectDir]) {
            if (!prefix || nimModule.name.startsWith(prefix)) {
                var suggest = new vscode.CompletionItem(nimModule.name);
                suggest.kind = vscode.CompletionItemKind.Module;
                suggest.insertText = nimModule.fullName;
                suggest.detail = nimModule.fullName;
                suggest.documentation = nimModule.path;
                suggestions.push(suggest);
            }
            if (suggestions.length >= 100) {
                return suggestions;
            }
        }
    }
    return suggestions;
}
exports.getImports = getImports;
function initImports() {
    return __awaiter(this, void 0, void 0, function* () {
        if (vscode.workspace.workspaceFolders) {
            yield yield initNimbleModules(vscode.workspace.workspaceFolders[0].uri.fsPath);
        }
        if (nimUtils_1.isProjectMode()) {
            for (const project of nimUtils_1.getProjects()) {
                yield initNimDirectories(project.wsFolder.uri.fsPath, project.filePath);
            }
        }
        else {
            if (vscode.workspace.workspaceFolders) {
                yield initNimDirectories(vscode.workspace.workspaceFolders[0].uri.fsPath, '');
            }
        }
    });
}
exports.initImports = initImports;
function addFileToImports(file) {
    return __awaiter(this, void 0, void 0, function* () {
        if (nimUtils_1.isProjectMode()) {
            for (const project of nimUtils_1.getProjects()) {
                let projectDir = project.wsFolder.uri.fsPath;
                if (file.startsWith(projectDir)) {
                    if (!nimModules[projectDir]) {
                        nimModules[projectDir] = [];
                    }
                    nimModules[projectDir].push(createNimModule(projectDir, projectDir, path.dirname(file), path.basename(file)));
                }
            }
        }
        else {
            if (vscode.workspace.workspaceFolders) {
                let projectDir = vscode.workspace.workspaceFolders[0].uri.fsPath;
                if (!nimModules[projectDir]) {
                    nimModules[projectDir] = [];
                }
                nimModules[projectDir].push(createNimModule(projectDir, projectDir, path.dirname(file), path.basename(file)));
            }
        }
    });
}
exports.addFileToImports = addFileToImports;
function removeFileFromImports(file) {
    return __awaiter(this, void 0, void 0, function* () {
        for (const key in nimModules) {
            const items = nimModules[key];
            var i = 0;
            while (i < items.length) {
                if (items[i].path === file) {
                    items.splice(i);
                }
                else {
                    i++;
                }
            }
        }
    });
}
exports.removeFileFromImports = removeFileFromImports;
//# sourceMappingURL=nimImports.js.map