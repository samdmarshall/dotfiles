'use strict';

import * as vscode from 'vscode';
import * as path from 'path';
import { exec } from 'child_process';
import * as mustache from 'mustache';
import * as fs from 'fs';

export class ManContentProvider implements vscode.TextDocumentContentProvider {
    private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
    private uri = vscode.Uri.parse('manvs://authority/manvs');
    private templatePath = path.join(__dirname, '../resources/template.html');

    private data: string = "";
    private template: string = "";

    constructor() {
        fs.readFile(this.templatePath, (_, data: Buffer) => {
            this.template = data.toString();
        });
    }

    get onDidChange(): vscode.Event<vscode.Uri> {
        return this._onDidChange.event;
    }

    public async update(command: string) {
        let res: any = await this.sh(command)
        this.data = res.stdout;

        this._onDidChange.fire(this.uri);
    }

    private sh(cmd: string) {
        return new Promise(function (resolve, reject) {
            exec(cmd, (err, stdout: string, stderr: string) => {
                if (err)
                    reject(err);
                else
                    resolve({stdout, stderr});
            });
        });
    }

    provideTextDocumentContent(_: vscode.Uri): vscode.ProviderResult<string> {
        if(this.template == "" || this.data == "") {
            vscode.window.showErrorMessage("Internal error. Try restarting vs code.");
            return;
        }

        return mustache.render(this.template, {manpage: this.data});
    }
}
