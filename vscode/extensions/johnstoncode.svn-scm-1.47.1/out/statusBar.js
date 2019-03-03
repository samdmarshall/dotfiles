"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
class SvnStatusBar {
    constructor(repository) {
        this.repository = repository;
        this.disposables = [];
        this._onDidChange = new vscode_1.EventEmitter();
        repository.onDidChangeStatus(this._onDidChange.fire, this._onDidChange, this.disposables);
        repository.onDidChangeOperations(this._onDidChange.fire, this._onDidChange, this.disposables);
    }
    get onDidChange() {
        return this._onDidChange.event;
    }
    get commands() {
        const result = [];
        if (this.repository.currentBranch) {
            result.push({
                command: "svn.switchBranch",
                tooltip: "switch branch",
                title: `$(git-branch) ${this.repository.currentBranch}`,
                arguments: [this.repository]
            });
        }
        const isIdle = this.repository.operations.isIdle();
        let icon = "sync";
        let title = "Updated";
        let command = "svn.update";
        let tooltip = "Update Revision";
        if (!isIdle) {
            icon = "sync~spin";
            title = "Running";
            tooltip = "Running";
            command = "";
        }
        else if (this.repository.needCleanUp) {
            icon = "alert";
            title = "Need cleanup";
            tooltip = "Run cleanup command";
            command = "svn.cleanup";
        }
        else if (this.repository.isIncomplete) {
            icon = "issue-reopened";
            title = "Incomplete (Need finish checkout)";
            tooltip = "Run update to complete";
            command = "svn.finishCheckout";
        }
        else if (this.repository.remoteChangedFiles > 0) {
            title = `${this.repository.remoteChangedFiles} remote changes`;
        }
        result.push({
            command,
            tooltip,
            title: `$(${icon}) ${title}`,
            arguments: [this.repository]
        });
        return result;
    }
    dispose() {
        this.disposables.forEach(disposable => disposable.dispose());
    }
}
exports.SvnStatusBar = SvnStatusBar;
//# sourceMappingURL=statusBar.js.map