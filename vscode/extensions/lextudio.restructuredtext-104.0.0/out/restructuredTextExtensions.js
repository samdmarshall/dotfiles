"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
const resolveExtensionResource = (extension, resourcePath) => {
    return vscode.Uri.file(path.join(extension.extensionPath, resourcePath))
        .with({ scheme: 'vscode-resource' });
};
const resolveExtensionResources = (extension, resourcePaths) => {
    const result = [];
    if (Array.isArray(resourcePaths)) {
        for (const resource of resourcePaths) {
            try {
                result.push(resolveExtensionResource(extension, resource));
            }
            catch (e) {
                // noop
            }
        }
    }
    return result;
};
class ReStructuredTextEExtensionContributions {
    constructor(extensionPath) {
        this.extensionPath = extensionPath;
        this._scripts = [];
        this._styles = [];
        this._previewResourceRoots = [];
        this._plugins = [];
        this._loaded = false;
    }
    get previewScripts() {
        this.ensureLoaded();
        return this._scripts;
    }
    get previewStyles() {
        this.ensureLoaded();
        return this._styles;
    }
    get previewResourceRoots() {
        this.ensureLoaded();
        return this._previewResourceRoots;
    }
    get restructuredTextItPlugins() {
        this.ensureLoaded();
        return this._plugins;
    }
    ensureLoaded() {
        if (this._loaded) {
            return;
        }
        this._loaded = true;
        for (const extension of vscode.extensions.all) {
            const contributes = extension.packageJSON && extension.packageJSON.contributes;
            if (!contributes) {
                continue;
            }
            this.tryLoadPreviewStyles(contributes, extension);
            this.tryLoadPreviewScripts(contributes, extension);
            this.tryLoadReStructuredTextItPlugins(contributes, extension);
            if (contributes['restructuredtext.previewScripts'] || contributes['restructuredtext.previewStyles']) {
                this._previewResourceRoots.push(vscode.Uri.file(extension.extensionPath));
            }
        }
    }
    tryLoadReStructuredTextItPlugins(contributes, extension) {
        if (contributes['restructuredtext.reStructuredItPlugins']) {
            this._plugins.push(extension.activate().then(() => {
                if (extension.exports && extension.exports.extendReStructuredIt) {
                    return (md) => extension.exports.extendReStructuredIt(md);
                }
                return (md) => md;
            }));
        }
    }
    tryLoadPreviewScripts(contributes, extension) {
        this._scripts.push(...resolveExtensionResources(extension, contributes['restructuredtext.previewScripts']));
    }
    tryLoadPreviewStyles(contributes, extension) {
        this._styles.push(...resolveExtensionResources(extension, contributes['restructuredtext.previewStyles']));
    }
}
function getReStructuredTextExtensionContributions(context) {
    return new ReStructuredTextEExtensionContributions(context.extensionPath);
}
exports.getReStructuredTextExtensionContributions = getReStructuredTextExtensionContributions;
//# sourceMappingURL=restructuredTextExtensions.js.map