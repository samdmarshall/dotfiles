/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const nimMode_1 = require("./nimMode");
const vscode = require("vscode");
let statusBarEntry;
let progressBarEntry;
function showHideStatus() {
    if (!statusBarEntry) {
        return;
    }
    if (!vscode.window.activeTextEditor) {
        statusBarEntry.hide();
        return;
    }
    if (vscode.languages.match(nimMode_1.NIM_MODE, vscode.window.activeTextEditor.document)) {
        statusBarEntry.show();
        return;
    }
    statusBarEntry.hide();
}
exports.showHideStatus = showHideStatus;
function hideNimStatus() {
    statusBarEntry.dispose();
}
exports.hideNimStatus = hideNimStatus;
function hideNimProgress() {
    progressBarEntry.dispose();
}
exports.hideNimProgress = hideNimProgress;
function showNimStatus(message, command, tooltip) {
    statusBarEntry = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, Number.MIN_VALUE);
    statusBarEntry.text = message;
    statusBarEntry.command = command;
    statusBarEntry.color = 'yellow';
    statusBarEntry.tooltip = tooltip;
    statusBarEntry.show();
}
exports.showNimStatus = showNimStatus;
function showNimProgress(message) {
    progressBarEntry = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, Number.MIN_VALUE);
    console.log(message);
    progressBarEntry.text = message;
    progressBarEntry.tooltip = message;
    progressBarEntry.show();
}
exports.showNimProgress = showNimProgress;
function updateNimProgress(message) {
    progressBarEntry.text = message;
}
exports.updateNimProgress = updateNimProgress;
//# sourceMappingURL=nimStatus.js.map