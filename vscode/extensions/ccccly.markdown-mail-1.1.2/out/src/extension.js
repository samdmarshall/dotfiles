'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require("vscode");
const word_counter_1 = require("./word-counter");
const word_counter_controller_1 = require("./word-counter-controller");
const email_1 = require("./email");
const smtp_config_1 = require("./smtp-config");
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "markdown-mail" is now active!');
    let wordCounter = new word_counter_1.default();
    let controller = new word_counter_controller_1.default(wordCounter);
    const accountConfig = vscode.workspace.getConfiguration("markdown-mail").get("account");
    const smtpConfig = smtp_config_1.default(accountConfig["user"]);
    const emailConnectionConfig = Object.assign({}, accountConfig, smtpConfig);
    const email = new email_1.default(emailConnectionConfig);
    const emailDisposable = vscode.commands.registerCommand('extension.sendMarkDownEmail', () => {
        email.send();
    });
    context.subscriptions.push(wordCounter);
    context.subscriptions.push(controller);
    context.subscriptions.push(emailDisposable);
}
exports.activate = activate;
process.on('uncaughtException', (err) => {
    vscode.window.setStatusBarMessage('发送失败,请查看扩展配置', 2000);
});
// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map