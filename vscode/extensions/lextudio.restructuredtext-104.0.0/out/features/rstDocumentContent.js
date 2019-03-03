'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const child_process_1 = require("child_process");
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const configuration_1 = require("./utils/configuration");
const selector_1 = require("./utils/selector");
class RstDocumentContentProvider {
    constructor(context, channel, status) {
        this._onDidChange = new vscode_1.EventEmitter();
        this._waiting = false;
        this._channel = channel;
        this._rstTransformerStatus = status;
        context.subscriptions.push(this._channel);
    }
    async provideTextDocumentContent(resource) {
        const uri = this.getOriginalUri(resource);
        this._timeout = configuration_1.Configuration.loadAnySetting('updateDelay', 300, uri);
        // Get path to the source RST file
        const rstPath = uri.fsPath;
        this._channel.appendLine('Source file: ' + rstPath);
        // Get the directory where the conf.py file is located
        const rstTransformerConf = await this.refreshConfig(uri);
        if (rstTransformerConf == null) {
            this.showError('You must select a RST -> HTML transformer from the menu that was shown', '');
        }
        let htmlPath = '';
        let fixStyle = false; // force bg color to white and foreground to black
        let readStdout = false; // Get HTML from stdout
        // Configure Sphinx
        if (rstTransformerConf.confPyDirectory !== '') {
            this._input = rstTransformerConf.confPyDirectory;
            this._channel.appendLine('Sphinx conf.py directory: ' + this._input);
            // Make sure the conf.py file exists
            let confFile = path.join(this._input, 'conf.py');
            if (!fs.existsSync(confFile)) {
                await this.resetRstTransformerConfig(uri);
                this._channel.appendLine('conf.py not found. Refresh the settings.');
                this._input = rstTransformerConf.confPyDirectory;
                this._channel.appendLine('Sphinx conf.py directory: ' + this._input);
                confFile = path.join(this._input, 'conf.py');
            }
            // The directory where Sphinx will write the html output
            const out = configuration_1.Configuration.loadSetting('builtDocumentationPath', null, uri);
            if (out == null) {
                this._output = path.join(this._input, '_build', 'html');
            }
            else {
                this._output = out;
            }
            this._channel.appendLine('Sphinx html directory: ' + this._output);
            const quotedOutput = '"' + this._output + '"';
            let build = configuration_1.Configuration.loadSetting('sphinxBuildPath', null, uri);
            if (build == null) {
                const python = configuration_1.Configuration.loadSetting('pythonPath', null, uri, 'python');
                if (python != null) {
                    build = python + ' -m sphinx';
                }
            }
            if (build == null) {
                build = 'sphinx-build';
            }
            // Configure the sphinx-build command
            this._options = { cwd: this._input };
            this._cmd = [
                build,
                '-b html',
                '.',
                quotedOutput,
            ].join(' ');
            // Calculate full path to built html file.
            let whole = rstPath;
            const ext = whole.lastIndexOf('.');
            whole = whole.substring(0, ext) + '.html';
            htmlPath = path.join(this._output, this.relativeDocumentationPath(whole));
        }
        else {
            // Configure rst2html.py
            let build = configuration_1.Configuration.loadSetting('rst2htmlCommand', null, uri);
            if (build == null) {
                build = 'rst2html.py';
            }
            // Configure the rst2html.py command
            this._cmd = [
                build,
                '"' + rstPath + '"',
            ].join(' ');
            fixStyle = true;
            readStdout = true;
            htmlPath = rstPath + '.html';
        }
        return this.preview(htmlPath, fixStyle, readStdout);
    }
    get onDidChange() {
        return this._onDidChange.event;
    }
    update(uri) {
        if (!this._waiting) {
            this._waiting = true;
            setTimeout(() => {
                this._waiting = false;
                this._onDidChange.fire(uri);
            }, this._timeout);
        }
    }
    async showStatus(uri, status) {
        const setting = configuration_1.Configuration.loadSetting('confPath', null, uri);
        if (setting == null) {
            return;
        }
        const rstTransformerConf = await this.getRstTransformerConfig(uri);
        //status.setLabel(rstTransformerConf.label);
        status.update();
    }
    async resetRstTransformerConfig(uri) {
        this._rstTransformerConfig = null;
        await configuration_1.Configuration.saveSetting('confPath', undefined, uri);
        this.update(uri);
        if (!vscode_1.window.activeTextEditor) {
            return;
        }
        // we are relaxed and don't check for markdown files
        await this.refreshConfig(vscode_1.window.activeTextEditor.document.uri);
    }
    fixLinks(document, documentPath) {
        return document.replace(new RegExp('((?:src|href)=[\'\"])(.*?)([\'\"])', 'gmi'), (subString, p1, p2, p3) => {
            const fileUrl = require('file-url');
            return [
                p1,
                fileUrl(path.join(path.dirname(documentPath), p2)),
                p3,
            ].join('');
        });
    }
    async getRstTransformerConfig(resource) {
        if (this._rstTransformerConfig) {
            return this._rstTransformerConfig;
        }
        else {
            return selector_1.RstTransformerSelector.findConfDir(resource, this._channel);
        }
    }
    showHelp(description, error) {
        const help = '<body>\
          <section>\
            <article>\
              <header>\
                <h2>Cannot show preview page.</h2>\
                <h4>Description:</h4>\
                ' + description + '\
                <h4>Detailed error message</h4>\
                <pre>' + error + '</pre>\
                <h4>More Information</h4>\
                <p>Diagnostics information has been written to OUTPUT | reStructuredText panel.</p>\
                <p>The troubleshooting guide can be found at</p>\
                <pre>https://docs.restructuredtext.net/articles/troubleshooting.html</pre>\
              </header>\
            </article>\
          </section>\
        </body>';
        return help;
    }
    showError(description, errorMessage) {
        this._channel.appendLine('Description: ' + description);
        this._channel.appendLine('Error: ' + errorMessage);
        return this.showHelp(description, errorMessage);
    }
    relativeDocumentationPath(whole) {
        return whole.substring(this._input.length);
    }
    preview(htmlPath, fixStyle, readStdout) {
        this._channel.appendLine('Compiler: ' + this._cmd);
        this._channel.appendLine('Working directory: ' + this._input);
        this._channel.appendLine('HTML file: ' + htmlPath);
        // Build and display file.
        return new Promise((resolve, reject) => {
            child_process_1.exec(this._cmd, this._options, (error, stdout, stderr) => {
                if (error) {
                    const description = '<p>Cannot generate preview page.</p>\
                        <p>Possible causes are,</p>\
                        <ul>\
                        <li>Python is not installed properly.</li>\
                        <li>Sphinx is not installed properly (if preview uses "conf.py").</li>\
                        <li>Wrong value is set on "restructuredtext.sphinxBuildPath".</li>\
                        <li>A wrong "conf.py" file is selected.</li>\
                        <li>DocUtil is not installed properly (if preview uses "rst2html.py").</li>\
                        </ul>';
                    const errorMessage = [
                        error.name,
                        error.message,
                        error.stack,
                        '',
                        stderr.toString(),
                    ].join('\n');
                    resolve(this.showError(description, errorMessage));
                }
                if (process.platform === 'win32' && stderr) {
                    const errText = stderr.toString();
                    if (errText.indexOf('Exception occurred:') > -1) {
                        const description = '<p>Cannot generate preview page on Windows.</p>\
                            <p>Possible causes are,</p>\
                            <ul>\
                            <li>Python is not installed properly.</li>\
                            <li>Sphinx is not installed properly (if preview uses "conf.py").</li>\
                            <li>Wrong value is set on "restructuredtext.sphinxBuildPath".</li>\
                            <li>A wrong "conf.py" file is selected.</li>\
                            <li>DocUtil is not installed properly (if preview uses "rst2html.py").</li>\
                            </ul>';
                        resolve(this.showError(description, errText));
                    }
                }
                if (readStdout) {
                    resolve(this.prepareHtml(stdout.toString(), htmlPath, fixStyle));
                }
                else {
                    fs.readFile(htmlPath, 'utf8', (err, data) => {
                        if (err === null) {
                            resolve(this.prepareHtml(data, htmlPath, fixStyle));
                        }
                        else {
                            const description = '<p>Cannot read preview page "' + htmlPath + '".</p>\
                                <p>Possible causes are,</p>\
                                <ul>\
                                <li>A wrong "conf.py" file is selected.</li>\
                                <li>Wrong value is set on "restructuredtext.builtDocumentationPath".</li>\
                                </ul>';
                            const errorMessage = [
                                err.name,
                                err.message,
                                err.stack,
                            ].join('\n');
                            resolve(this.showError(description, errorMessage));
                        }
                    });
                }
            });
        });
    }
    prepareHtml(html, htmlPath, fixStyle) {
        let fixed = this.fixLinks(html, htmlPath);
        if (fixStyle) {
            fixed += '<style>html, body {background: #fff;color: #000;}</style>';
        }
        return fixed;
    }
    async refreshConfig(resource) {
        const rstTransformerConf = await this.getRstTransformerConfig(resource);
        if (rstTransformerConf == null) {
            return null;
        }
        //this._rstTransformerStatus.setLabel(rstTransformerConf.label);
        this._rstTransformerConfig = rstTransformerConf;
        await configuration_1.Configuration.saveSetting('confPath', rstTransformerConf.confPyDirectory, resource);
        return rstTransformerConf;
    }
    getOriginalUri(uri) {
        return uri.with({ scheme: 'file', path: uri.path, query: uri.toString() });
    }
}
exports.default = RstDocumentContentProvider;
//# sourceMappingURL=rstDocumentContent.js.map