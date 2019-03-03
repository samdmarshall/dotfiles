"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode = require("vscode");
const assert = require("assert");
const fs = require("fs");
exports.samplePath = path.join(__dirname, "..", "..", "test-resources");
async function checkValidFile(file) {
    return new Promise((resolve, reject) => {
        fs.stat(file, (err, stats) => {
            if (err) {
                return reject(err);
            }
            assert(stats.isFile(), "Not a valid file");
            resolve(true);
        });
    });
}
async function initialize() {
    let dummyFile = path.join(exports.samplePath, "docutils", "example1.rst");
    await checkValidFile(dummyFile);
    return vscode.workspace.openTextDocument(dummyFile);
}
exports.initialize = initialize;
async function openFile(file) {
    await checkValidFile(file);
    const document = await vscode.workspace.openTextDocument(file);
    return vscode.window.showTextDocument(document);
}
exports.openFile = openFile;
async function closeActiveWindows() {
    // https://github.com/Microsoft/vscode/blob/master/extensions/vscode-api-tests/src/utils.ts
    await new Promise(async (c, e) => {
        if (vscode.window.visibleTextEditors.length === 0) {
            return c();
        }
        // TODO: the visibleTextEditors variable doesn't seem to be
        // up to date after a onDidChangeActiveTextEditor event, not
        // even using a setTimeout 0... so we MUST poll :(
        let interval = setInterval(() => {
            if (vscode.window.visibleTextEditors.length > 0) {
                return;
            }
            clearInterval(interval);
            c();
        }, 10);
        try {
            await vscode.commands.executeCommand("workbench.action.closeAllEditors");
        }
        catch (e) {
            clearInterval(interval);
            e(e);
        }
    });
    assert.equal(vscode.window.visibleTextEditors.length, 0);
    assert(!vscode.window.activeTextEditor);
}
exports.closeActiveWindows = closeActiveWindows;
function wait(time) {
    return new Promise(res => setTimeout(res, time));
}
exports.wait = wait;
//# sourceMappingURL=initialize.js.map