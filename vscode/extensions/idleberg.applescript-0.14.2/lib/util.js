'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
// Dependencies
const child_process_1 = require("child_process");
const path_1 = require("path");
const vscode_1 = require("vscode");
const getConfig = () => {
    return vscode_1.workspace.getConfiguration('applescript');
};
exports.getConfig = getConfig;
const getOutName = (fileName, extension = 'scpt') => {
    let dirName = path_1.dirname(fileName);
    let baseName = path_1.basename(fileName, path_1.extname(fileName));
    let outName = path_1.join(dirName, `${baseName}.${extension}`);
    return outName;
};
exports.getOutName = getOutName;
const spawnPromise = (cmd, args, outputChannel) => {
    return new Promise((resolve, reject) => {
        outputChannel.clear();
        if (getConfig().alwaysShowOutput === true) {
            outputChannel.show();
        }
        const process = child_process_1.spawn(cmd, args);
        let stdErr = '';
        process.stdout.on('data', (data) => {
            outputChannel.appendLine(data.toString());
        });
        process.stderr.on('data', (data) => {
            stdErr += '\n' + data;
            outputChannel.appendLine(data.toString());
        });
        process.on('close', (code) => {
            if (code !== 0) {
                console.error(stdErr);
                return reject();
            }
            return resolve();
        });
    });
};
exports.spawnPromise = spawnPromise;
//# sourceMappingURL=util.js.map