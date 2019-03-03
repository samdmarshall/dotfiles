'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
// Dependencies
const vscode_1 = require("vscode");
// Modules
const osa_1 = require("./osa");
const task_1 = require("./task");
const activate = (context) => {
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.applescript.run', () => {
        return osa_1.osascript();
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.applescript.compile', () => {
        return osa_1.osacompile('scpt');
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.applescript.compileBundle', () => {
        return osa_1.osacompile('scptd');
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.applescript.compileApp', () => {
        return osa_1.osacompile('app');
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.applescript.createBuildTask', () => {
        return task_1.createBuildTask();
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.jxa.run', () => {
        return osa_1.osascript({ isJXA: true });
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.jxa.compile', () => {
        return osa_1.osacompile('scpt', { isJXA: true });
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.jxa.compileBundle', () => {
        return osa_1.osacompile('scptd', { isJXA: true });
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.jxa.compileApp', () => {
        return osa_1.osacompile('app', { isJXA: true });
    }));
    context.subscriptions.push(vscode_1.commands.registerTextEditorCommand('extension.jxa.createBuildTask', () => {
        return task_1.createBuildTask(true);
    }));
};
exports.activate = activate;
//# sourceMappingURL=main.js.map