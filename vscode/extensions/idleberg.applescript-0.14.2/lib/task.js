'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const fs_1 = require("fs");
const util_1 = require("./util");
const path_1 = require("path");
const createBuildTask = (isJXA = false) => {
    if (typeof vscode_1.workspace.rootPath === 'undefined' || vscode_1.workspace.rootPath === null) {
        return vscode_1.window.showErrorMessage('Task support is only available when working on a workspace folder. It is not available when editing single files.');
    }
    let config = util_1.getConfig();
    let command = 'osacompile';
    let doc = vscode_1.window.activeTextEditor.document;
    const args = [];
    let runArgs = [path_1.basename(doc.fileName)];
    let scriptArgs = ['-o', path_1.basename(util_1.getOutName(doc.fileName)), path_1.basename(doc.fileName)];
    let bundleArgs = ['-o', path_1.basename(util_1.getOutName(doc.fileName, 'scptd')), path_1.basename(doc.fileName)];
    let appArgs = ['-o', path_1.basename(util_1.getOutName(doc.fileName, 'app')), path_1.basename(doc.fileName)];
    if (config.osacompile.executeOnly === true) {
        args.unshift('-x');
    }
    if (config.osacompile.stayOpen === true) {
        appArgs.unshift('-s');
    }
    if (config.osacompile.startupScreen === true) {
        appArgs.unshift('-u');
    }
    if (isJXA === true) {
        args.push('-l', 'JavaScript');
    }
    if (config.osascript.outputStyle.trim().length > 0 && config.osascript.outputStyle.trim().length <= 2) {
        runArgs.unshift('-s', config.osascript.outputStyle.trim());
    }
    runArgs = args.concat(runArgs);
    scriptArgs = args.concat(scriptArgs);
    bundleArgs = args.concat(bundleArgs);
    appArgs = args.concat(appArgs);
    const { version } = require('../package.json');
    let taskFile = {
        'version': `0.1.0`,
        'isShellCommand': false,
        'showOutput': 'always',
        'suppressTaskName': true,
        'echoCommand': false,
        'tasks': [
            {
                'command': 'osascript',
                'taskName': 'Run Script',
                'args': runArgs,
            },
            {
                'command': 'osacompile',
                'taskName': 'Compile Script',
                'args': scriptArgs,
                'isBuildCommand': (config.defaultBuildTask === 'script') ? true : false
            },
            {
                'command': 'osacompile',
                'taskName': 'Compile Script Bundle',
                'args': bundleArgs,
                'isBuildCommand': (config.defaultBuildTask === 'bundle') ? true : false
            },
            {
                'command': 'osacompile',
                'taskName': 'Compile Application',
                'args': appArgs,
                'isBuildCommand': (config.defaultBuildTask === 'app') ? true : false
            }
        ]
    };
    let jsonString = JSON.stringify(taskFile, null, 2);
    let dotFolder = path_1.join(vscode_1.workspace.rootPath, '/.vscode');
    let buildFile = path_1.join(dotFolder, 'tasks.json');
    fs_1.mkdir(dotFolder, (error) => {
        // ignore errors for now
        fs_1.writeFile(buildFile, jsonString, (error) => {
            if (error) {
                vscode_1.window.showErrorMessage(error.toString());
            }
            if (config.alwaysOpenBuildTask === false)
                return;
            // Open tasks.json
            vscode_1.workspace.openTextDocument(buildFile).then((doc) => {
                vscode_1.window.showTextDocument(doc);
            });
        });
    });
};
exports.createBuildTask = createBuildTask;
//# sourceMappingURL=task.js.map