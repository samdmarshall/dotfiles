'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
class Configuration {
    static getConflictingExtensions(resource = null) {
        return Configuration.loadAnySetting('conflictingExtensions', null, resource);
    }
    static getSphinxPath(resource = null) {
        return Configuration.loadSetting('sphinxBuildPath', null, resource);
    }
    static getConfPath(resource = null) {
        return Configuration.loadSetting('confPath', null, resource);
    }
    static getOutputFolder(resource = null) {
        return Configuration.loadSetting('builtDocumentationPath', null, resource);
    }
    static getLinterPath(resource = null) {
        return Configuration.loadSetting('linter.executablePath', null, resource);
    }
    static getExtraArgs(resource = null) {
        return Configuration.loadAnySetting('linter.extraArgs', null, resource);
    }
    static getRunType(resource = null) {
        return Configuration.loadAnySetting('linter.run', 'onType', resource);
    }
    static getPythonPath(resource = null) {
        return Configuration.loadSetting('pythonPath', null, resource, 'python');
    }
    static getLanguageServerDisabled(resource = null) {
        return Configuration.loadAnySetting('languageServer.disabled', true, null);
    }
    static getSupportedPlatforms(resource = null) {
        return Configuration.loadAnySetting("languageServer.supportedPlatforms", [], null);
    }
    static getUpdateDelay(resource = null) {
        return Configuration.loadAnySetting("updateDelay", 300, resource);
    }
    static async setConfPath(value, resource = null, insertMacro) {
        return await Configuration.saveSetting('confPath', value, resource, insertMacro);
    }
    static async setRoot(resource = null) {
        const old = this.loadSetting('workspaceRoot', null, resource);
        if (old.indexOf('${workspaceRoot}') > -1) {
            await this.saveSetting('workspaceRoot', this.expandMacro(old, resource), resource);
        }
    }
    static loadAnySetting(configSection, defaultValue, resource, header = 'restructuredtext') {
        return vscode_1.workspace.getConfiguration(header, resource).get(configSection, defaultValue);
    }
    static async saveAnySetting(configSection, value, resource, header = 'restructuredtext') {
        await vscode_1.workspace.getConfiguration(header, resource).update(configSection, value);
        return value;
    }
    static loadSetting(configSection, defaultValue, resource, header = 'restructuredtext', expand = true) {
        const result = this.loadAnySetting(configSection, defaultValue, resource, header);
        if (expand && result != null) {
            return this.expandMacro(result, resource);
        }
        return result;
    }
    static async saveSetting(configSection, value, resource, insertMacro = false, header = 'restructuredtext') {
        if (insertMacro) {
            value = this.insertMacro(value, resource);
        }
        return await this.saveAnySetting(configSection, value, resource, header);
    }
    static insertMacro(input, resource) {
        if (resource == null) {
            return input;
        }
        let path;
        if (!vscode_1.workspace.workspaceFolders) {
            path = vscode_1.workspace.rootPath;
        }
        else {
            let root;
            if (vscode_1.workspace.workspaceFolders.length === 1) {
                root = vscode_1.workspace.workspaceFolders[0];
            }
            else {
                root = vscode_1.workspace.getWorkspaceFolder(resource);
            }
            path = root.uri.fsPath;
        }
        if (input.startsWith(path)) {
            return input
                .replace(path, '${workspaceFolder}');
        }
        return input;
    }
    static expandMacro(input, resource) {
        if (resource == null || input.indexOf('${') === -1) {
            return input;
        }
        let expanded;
        if (input.indexOf('${env:') > -1) {
            expanded = input.replace(/\$\{env\:(.+)\}/, (match, p1) => {
                const variable = process.env[p1];
                return variable == null ? '' : variable;
            });
        }
        else {
            expanded = input;
        }
        if (expanded.indexOf('${') > -1) {
            const path = this.GetRootPath(resource);
            if (path != null) {
                return expanded
                    .replace('${workspaceRoot}', path)
                    .replace('${workspaceFolder}', path);
            }
        }
        return expanded;
    }
    static GetRootPath(resource) {
        let path;
        if (!vscode_1.workspace.workspaceFolders) {
            path = vscode_1.workspace.rootPath;
        }
        else {
            let root;
            if (vscode_1.workspace.workspaceFolders.length === 1) {
                root = vscode_1.workspace.workspaceFolders[0];
            }
            else {
                root = vscode_1.workspace.getWorkspaceFolder(resource);
            }
            if (root != null) {
                path = root.uri.fsPath;
            }
        }
        return path;
    }
}
exports.Configuration = Configuration;
//# sourceMappingURL=configuration.js.map