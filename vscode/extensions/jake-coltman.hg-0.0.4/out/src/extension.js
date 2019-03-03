'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    let terminalStack = [];
    terminalStack.push(vscode.window.createTerminal(`Ext Terminal #${terminalStack.length + 1}`));
    var dir = vscode.workspace.rootPath;
    run_command("cd " + dir);
    function getLatestTerminal() {
        return terminalStack[terminalStack.length - 1];
    }
    function run_command(command) {
        getLatestTerminal().sendText(command);
    }
    let disposable_commit = vscode.commands.registerCommand('extension.commit', () => {
        run_command("hg add .");
        vscode.window.showInputBox().then(x => run_command("hg commit -m \"" + x + "\""));
    });
    let disposable_push = vscode.commands.registerCommand('extension.push', () => {
        run_command("hg push");
    });
    let disposable_pull = vscode.commands.registerCommand('extension.pull', () => {
        run_command("hg pull");
        run_command("hg update");
    });
    context.subscriptions.push(disposable_commit);
    context.subscriptions.push(disposable_pull);
    context.subscriptions.push(disposable_push);
}
exports.activate = activate;
// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map