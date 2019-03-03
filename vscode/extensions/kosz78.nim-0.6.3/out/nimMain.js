/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const nimSuggestExec_1 = require("./nimSuggestExec");
const nimSuggest_1 = require("./nimSuggest");
const nimDeclaration_1 = require("./nimDeclaration");
const nimReferences_1 = require("./nimReferences");
const nimHover_1 = require("./nimHover");
const nimOutline_1 = require("./nimOutline");
const indexer = require("./nimIndexer");
const nimSignature_1 = require("./nimSignature");
const nimFormatting_1 = require("./nimFormatting");
const nimBuild_1 = require("./nimBuild");
const nimMode_1 = require("./nimMode");
const nimStatus_1 = require("./nimStatus");
const nimUtils_1 = require("./nimUtils");
const vscode_1 = require("vscode");
const nimImports_1 = require("./nimImports");
let diagnosticCollection;
var fileWatcher;
var terminal;
function activate(ctx) {
    let config = vscode.workspace.getConfiguration('nim');
    vscode.commands.registerCommand('nim.run.file', runFile);
    vscode.commands.registerCommand('nim.check', runCheck);
    vscode.commands.registerCommand('nim.execSelectionInTerminal', nimBuild_1.execSelectionInTerminal);
    nimSuggestExec_1.initNimSuggest(ctx);
    ctx.subscriptions.push(vscode.languages.registerCompletionItemProvider(nimMode_1.NIM_MODE, new nimSuggest_1.NimCompletionItemProvider(), '.', ' '));
    ctx.subscriptions.push(vscode.languages.registerDefinitionProvider(nimMode_1.NIM_MODE, new nimDeclaration_1.NimDefinitionProvider()));
    ctx.subscriptions.push(vscode.languages.registerReferenceProvider(nimMode_1.NIM_MODE, new nimReferences_1.NimReferenceProvider()));
    ctx.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(nimMode_1.NIM_MODE, new nimOutline_1.NimDocumentSymbolProvider()));
    ctx.subscriptions.push(vscode.languages.registerSignatureHelpProvider(nimMode_1.NIM_MODE, new nimSignature_1.NimSignatureHelpProvider(), '(', ','));
    ctx.subscriptions.push(vscode.languages.registerHoverProvider(nimMode_1.NIM_MODE, new nimHover_1.NimHoverProvider()));
    ctx.subscriptions.push(vscode.languages.registerDocumentFormattingEditProvider(nimMode_1.NIM_MODE, new nimFormatting_1.NimFormattingProvider()));
    diagnosticCollection = vscode.languages.createDiagnosticCollection('nim');
    ctx.subscriptions.push(diagnosticCollection);
    vscode.languages.setLanguageConfiguration(nimMode_1.NIM_MODE.language, {
        indentationRules: {
            increaseIndentPattern: /^\s*((((proc|macro|iterator|template|converter|func)\b.*\=)|(import|var|const|type)\s)|(import|let|var|const|type)|([^:]+:))$/,
            decreaseIndentPattern: /^\s*(((return|break|continue|raise)\n)|((elif|else|except|finally)\b.*:))\s*$/
        },
        wordPattern: /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g,
        onEnterRules: [{
                beforeText: /^\s*$/,
                action: { indentAction: vscode.IndentAction.None }
            }]
    });
    vscode.window.onDidChangeActiveTextEditor(nimStatus_1.showHideStatus, null, ctx.subscriptions);
    vscode.window.onDidCloseTerminal((e) => {
        if (terminal && e.processId === terminal.processId) {
            terminal = undefined;
        }
    });
    console.log(ctx.extensionPath);
    nimBuild_1.activateEvalConsole();
    indexer.initWorkspace(ctx.extensionPath);
    fileWatcher = vscode.workspace.createFileSystemWatcher('**/*.nim');
    fileWatcher.onDidCreate((uri) => {
        if (config.has('licenseString')) {
            let path = uri.fsPath.toLowerCase();
            if (path.endsWith('.nim') || path.endsWith('.nims')) {
                fs.stat(uri.fsPath, (err, stats) => {
                    if (stats && stats.size === 0) {
                        let edit = new vscode.WorkspaceEdit();
                        edit.insert(uri, new vscode.Position(0, 0), config['licenseString']);
                        vscode.workspace.applyEdit(edit);
                    }
                });
            }
        }
        nimImports_1.addFileToImports(uri.fsPath);
    });
    fileWatcher.onDidDelete(uri => {
        nimImports_1.removeFileFromImports(uri.fsPath);
    });
    ctx.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider(new nimOutline_1.NimWorkspaceSymbolProvider()));
    startBuildOnSaveWatcher(ctx.subscriptions);
    if (vscode.window.activeTextEditor && !!vscode.workspace.getConfiguration('nim')['lintOnSave']) {
        runCheck(vscode.window.activeTextEditor.document);
    }
    if (config.has('nimsuggestRestartTimeout')) {
        let timeout = config['nimsuggestRestartTimeout'];
        if (timeout > 0) {
            console.log('Reset nimsuggest process each ' + timeout + ' minutes');
            global.setInterval(() => nimSuggestExec_1.closeAllNimSuggestProcesses(), timeout * 60000);
        }
    }
    nimImports_1.initImports();
    nimUtils_1.outputLine('[info] Extension Activated');
}
exports.activate = activate;
function deactivate() {
    nimSuggestExec_1.closeAllNimSuggestProcesses();
    fileWatcher.dispose();
}
exports.deactivate = deactivate;
function runCheck(document) {
    let config = vscode.workspace.getConfiguration('nim');
    if (!document && vscode.window.activeTextEditor) {
        document = vscode.window.activeTextEditor.document;
    }
    function mapSeverityToVSCodeSeverity(sev) {
        switch (sev) {
            case 'Hint': return vscode.DiagnosticSeverity.Warning;
            case 'Error': return vscode.DiagnosticSeverity.Error;
            case 'Warning': return vscode.DiagnosticSeverity.Warning;
            default: return vscode.DiagnosticSeverity.Error;
        }
    }
    if (!document || document.languageId !== 'nim') {
        return;
    }
    var uri = document.uri;
    vscode.window.withProgress({ location: vscode_1.ProgressLocation.Window, cancellable: false, title: 'Nim: check project...' }, (progress) => nimBuild_1.check(uri.fsPath, config)).then(errors => {
        diagnosticCollection.clear();
        let diagnosticMap = new Map();
        var err = {};
        errors.forEach(error => {
            if (!err[error.file + error.line + error.column + error.msg]) {
                let targetUri = error.file;
                let endColumn = error.column;
                if (error.msg.indexOf('\'') >= 0) {
                    endColumn += error.msg.lastIndexOf('\'') - error.msg.indexOf('\'') - 2;
                }
                let line = Math.max(0, error.line - 1);
                let range = new vscode.Range(line, Math.max(0, error.column - 1), line, Math.max(0, endColumn));
                let diagnostic = new vscode.Diagnostic(range, error.msg, mapSeverityToVSCodeSeverity(error.severity));
                let diagnostics = diagnosticMap.get(targetUri);
                if (!diagnostics) {
                    diagnostics = [];
                }
                diagnosticMap.set(targetUri, diagnostics);
                diagnostics.push(diagnostic);
                err[error.file + error.line + error.column + error.msg] = true;
            }
        });
        let entries = [];
        diagnosticMap.forEach((diags, uri) => {
            entries.push([vscode.Uri.file(uri), diags]);
        });
        diagnosticCollection.set(entries);
    });
}
function startBuildOnSaveWatcher(subscriptions) {
    vscode.workspace.onDidSaveTextDocument(document => {
        if (document.languageId !== 'nim') {
            return;
        }
        if (!!vscode.workspace.getConfiguration('nim')['lintOnSave']) {
            runCheck(document);
        }
        if (!!vscode.workspace.getConfiguration('nim')['buildOnSave']) {
            vscode.commands.executeCommand('workbench.action.tasks.build');
        }
    }, null, subscriptions);
}
function runFile() {
    let editor = vscode.window.activeTextEditor;
    if (editor) {
        if (!terminal) {
            terminal = vscode.window.createTerminal('Nim');
        }
        terminal.show(true);
        if (editor.document.isUntitled) {
            terminal.sendText('nim ' + vscode.workspace.getConfiguration('nim')['buildCommand'] +
                ' -r "' + nimUtils_1.getDirtyFile(editor.document) + '"', true);
        }
        else {
            let outputDirConfig = vscode.workspace.getConfiguration('nim')['runOutputDirectory'];
            var outputParams = '';
            if (!!outputDirConfig) {
                if (vscode.workspace.workspaceFolders) {
                    var rootPath = '';
                    for (const folder of vscode.workspace.workspaceFolders) {
                        if (folder.uri.scheme === 'file') {
                            rootPath = folder.uri.fsPath;
                            break;
                        }
                    }
                    if (rootPath !== '') {
                        if (!fs.existsSync(path.join(rootPath, outputDirConfig))) {
                            fs.mkdirSync(path.join(rootPath, outputDirConfig));
                        }
                        outputParams = ' --out:"' + path.join(outputDirConfig, path.basename(editor.document.fileName, '.nim')) + '"';
                    }
                }
            }
            if (editor && editor.document.isDirty) {
                editor.document.save().then((success) => {
                    if (terminal && editor && success) {
                        terminal.sendText('nim ' + vscode.workspace.getConfiguration('nim')['buildCommand'] +
                            outputParams + ' -r "' + editor.document.fileName + '"', true);
                    }
                });
            }
            else {
                terminal.sendText('nim ' + vscode.workspace.getConfiguration('nim')['buildCommand'] +
                    outputParams + ' -r "' + editor.document.fileName + '"', true);
            }
        }
    }
}
//# sourceMappingURL=nimMain.js.map