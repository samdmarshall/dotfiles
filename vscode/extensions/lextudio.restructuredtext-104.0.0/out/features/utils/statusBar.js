'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const configuration_1 = require("./configuration");
const selector_1 = require("./selector");
/**
 * Status bar updates. Shows the selected RstTransformerConfig when a
 * restructuredtext document is active. If you click on the status bar
 * then the RstTransformerConfig is reset and you will need to select from
 * the menu when the preview is generated next time.
 */
class RstTransformerStatus {
    constructor(python, logger) {
        this._statusBarItem = vscode_1.window.createStatusBarItem(vscode_1.StatusBarAlignment.Left);
        this._statusBarItem.command = 'restructuredtext.resetStatus';
        this._statusBarItem.tooltip = 'The active rst to html transformer (click to reset)';
        this._logger = logger;
        this.python = python;
    }
    setLabel() {
        if (this.config) {
            this._statusBarItem.text = this.config.label;
        }
    }
    async update() {
        const editor = vscode_1.window.activeTextEditor;
        if (editor != null && editor.document.languageId === 'restructuredtext') {
            const resource = editor.document.uri;
            const workspaceRoot = configuration_1.Configuration.GetRootPath(resource);
            if (!this.config || this.config.workspaceRoot !== workspaceRoot) {
                await this.refreshConfig(resource);
                this.python.setup(resource);
            }
            this.setLabel();
            this._statusBarItem.show();
        }
        else {
            this._statusBarItem.hide();
        }
    }
    async reset() {
        const editor = vscode_1.window.activeTextEditor;
        if (editor != null && editor.document.languageId === 'restructuredtext') {
            let resource = editor.document.uri;
            const newValue = await configuration_1.Configuration.setConfPath(undefined, resource, false);
            if (newValue !== undefined) {
                this._logger.appendLine("reset failed.");
            }
            this.refreshConfig(resource);
            this.setLabel();
        }
    }
    async refreshConfig(resource) {
        const rstTransformerConf = await selector_1.RstTransformerSelector.findConfDir(resource, this._logger);
        if (rstTransformerConf == null) {
            return null;
        }
        this.config = rstTransformerConf;
        await configuration_1.Configuration.setConfPath(rstTransformerConf.confPyDirectory, resource, true);
        return rstTransformerConf;
    }
}
exports.default = RstTransformerStatus;
//# sourceMappingURL=statusBar.js.map