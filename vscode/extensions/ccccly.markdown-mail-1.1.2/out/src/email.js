"use strict";
const vscode_1 = require("vscode");
const markdown_1 = require("./markdown");
const email = require('emailjs');
class Email {
    constructor(smtpConfiguration) {
        this._server = email.server.connect(smtpConfiguration);
        this._markdownRender = new markdown_1.default();
    }
    send() {
        const editor = vscode_1.window.activeTextEditor;
        if (!editor) {
            vscode_1.window.showErrorMessage("没有活动的 markdown 文档");
            return;
        }
        const doc = editor.document;
        if (doc.isDirty) {
            vscode_1.window.showErrorMessage('文档未保存');
            return;
        }
        if (doc.isUntitled) {
            vscode_1.window.showErrorMessage('请先保存为 markdown 文件');
            return;
        }
        const content = doc.getText();
        const html = this._markdownRender.renderMarkdownToHtml(content);
        const emailContent = vscode_1.workspace.getConfiguration("markdown-mail").get("email");
        const accountConfig = vscode_1.workspace.getConfiguration("markdown-mail").get("account");
        if (validateAccountConfig(accountConfig)) {
            vscode_1.window.showErrorMessage('请先到工作区设置或用户设置中配置 email 账号');
        }
        emailContent.from = getUserFromAccountConfig(accountConfig);
        emailContent.attachment = [{ data: html, alternative: true }];
        vscode_1.window.setStatusBarMessage('正在发送...', 2000);
        this._server.send(emailContent, (err, message) => {
            if (err) {
                vscode_1.window.setStatusBarMessage('发送失败,请查看扩展配置', 2000);
                return;
            }
            vscode_1.window.setStatusBarMessage(`邮件已发送至${emailContent.to}`, 2000);
        });
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = Email;
function getUserFromAccountConfig(accountConfig) {
    return `${accountConfig["user"].split('@')[0]} <${accountConfig["user"]}>`;
}
function validateAccountConfig(accountConfig) {
    return accountConfig["user"] === "username@your-email.com";
}
//# sourceMappingURL=email.js.map