'use strict';
import * as vscode from 'vscode';
import { ManContentProvider } from './ManContentProvider';

export async function activate(context: vscode.ExtensionContext) {
    let provider = new ManContentProvider();
    let registration = vscode.workspace.registerTextDocumentContentProvider("manvs", provider);

    let manPage = vscode.commands.registerCommand('manvs.man', async () => {
        let cmd = await vscode.window.showInputBox();
        if(cmd == undefined)
            return;

        cmd = cmd.trim();
        if(cmd == "")
            return;

        await provider.update(`man ${cmd} | col -b`);

        return vscode.commands.executeCommand('vscode.previewHtml', vscode.Uri.parse('manvs://authority/manvs'), vscode.ViewColumn.Active, `MAN ${cmd}`).then(_ => {}, _ => {
            vscode.window.showErrorMessage("Can't open man page.");
        });
    });

    context.subscriptions.push(registration, manPage);
}

export function deactivate() {
}
