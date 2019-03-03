'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
// Dependencies
const os_1 = require("os");
const vscode_1 = require("vscode");
// Modules
const util_1 = require("./util");
const outputChannel = vscode_1.window.createOutputChannel('AppleScript');
const osacompile = (compileTarget, options = { isJXA: false }) => {
    const config = util_1.getConfig();
    // might become useful in a future release
    options = Object.assign({}, options, config.osacompile);
    if (os_1.platform() !== 'darwin' && config.ignoreOS !== true) {
        return vscode_1.window.showWarningMessage('This command is only available on macOS');
    }
    let doc = vscode_1.window.activeTextEditor.document;
    doc.save().then(() => {
        const outName = util_1.getOutName(doc.fileName, compileTarget);
        const args = ['-o', outName, doc.fileName];
        if (options.executeOnly === true) {
            args.unshift('-x');
        }
        if (compileTarget === 'app' && options.stayOpen === true) {
            args.unshift('-s');
        }
        if (compileTarget === 'app' && options.startupScreen === true) {
            args.unshift('-u');
        }
        if (options.isJXA === true) {
            args.unshift('-l', 'JavaScript');
        }
        util_1.spawnPromise('osacompile', args, outputChannel)
            .then(() => {
            if (config.showNotifications)
                vscode_1.window.showInformationMessage(`Successfully compiled '${doc.fileName}'`);
        })
            .catch(() => {
            outputChannel.show(true);
            if (config.showNotifications)
                vscode_1.window.showErrorMessage('Failed to run compile (see output for details)');
        });
    });
};
exports.osacompile = osacompile;
const osascript = (options = { isJXA: false }) => {
    const config = util_1.getConfig();
    if (os_1.platform() !== 'darwin' && config.ignoreOS !== true) {
        return vscode_1.window.showWarningMessage('This command is only available on macOS');
    }
    let doc = vscode_1.window.activeTextEditor.document;
    const args = [];
    if (doc.isDirty) {
        const lines = doc.getText().split('\n');
        lines.forEach(function (line) {
            args.push('-e', line);
        });
    }
    else {
        args.push(doc.fileName);
    }
    if (config.osascript.outputStyle.trim().length > 0 && config.osascript.outputStyle.trim().length <= 2) {
        args.unshift('-s', config.osascript.outputStyle.trim());
    }
    if (options.isJXA === true) {
        args.unshift('-l', 'JavaScript');
    }
    util_1.spawnPromise('osascript', args, outputChannel)
        .catch(() => {
        outputChannel.show(true);
        if (config.showNotifications)
            vscode_1.window.showErrorMessage('Failed to run script (see output for details)');
    });
};
exports.osascript = osascript;
//# sourceMappingURL=osa.js.map