"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
const nls = require("vscode-nls");
const localize = nls.loadMessageBundle();
const security_1 = require("../security");
/**
 * Strings used inside the html preview.
 *
 * Stored here and then injected in the preview so that they
 * can be localized using our normal localization process.
 */
const previewStrings = {
    cspAlertMessageText: localize('preview.securityMessage.text', 'Some content has been disabled in this document'),
    cspAlertMessageTitle: localize('preview.securityMessage.title', 'Potentially unsafe or insecure content has been disabled in the html preview. Change the HTML preview security setting to allow insecure content or enable scripts'),
    cspAlertMessageLabel: localize('preview.securityMessage.label', 'Content Disabled Security Warning')
};
class RSTContentProvider {
    constructor(context, cspArbiter, engine, logger) {
        this.context = context;
        this.cspArbiter = cspArbiter;
        this.engine = engine;
        this.logger = logger;
        this.TAG_RegEx = /^\s*?\<(p|h[1-6]|img|code|blockquote|li)((\s+.*?)(class="(.*?)")(.*?\>)|\>|\>|\/\>|\s+.*?\>)/;
    }
    async provideTextDocumentContent(rstDocument, previewConfigurations, initialLine = undefined, state) {
        const sourceUri = rstDocument.uri;
        const config = previewConfigurations.loadAndCacheConfiguration(sourceUri);
        const initialData = {
            source: sourceUri.toString(),
            line: initialLine,
            lineCount: rstDocument.lineCount,
            scrollPreviewWithEditor: config.scrollPreviewWithEditor,
            scrollEditorWithPreview: config.scrollEditorWithPreview,
            doubleClickToSwitchToEditor: config.doubleClickToSwitchToEditor,
            disableSecurityWarnings: this.cspArbiter.shouldDisableSecurityWarnings()
        };
        this.logger.log('provideTextDocumentContent', initialData);
        const body = await this.engine.preview(rstDocument);
        const useSphinx = body.search('</head>') > -1;
        // Content Security Policy
        const nonce = new Date().getTime() + '' + new Date().getMilliseconds();
        const csp = this.getCspForResource(sourceUri, nonce, useSphinx);
        if (useSphinx) {
            // sphinx based preview.
            let elementCount = 0;
            let canStart = false;
            const parsedDoc = body.split(/\r?\n/).map((l, i) => {
                if (l.search('<div itemprop="articleBody">') > -1) {
                    canStart = true;
                }
                if (!canStart) {
                    return l;
                }
                return l.replace(this.TAG_RegEx, (match, p1, p2, p3, p4, p5, p6, offset) => {
                    elementCount++;
                    return typeof p5 !== "string" ?
                        `<${p1} class="code-line" data-line="${elementCount}" ${p2}` :
                        `<${p1} ${p3} class="${p5} code-line" data-line="${elementCount}" ${p6}`;
                });
            }).join("\n");
            const newHead = parsedDoc.replace('</head>', `
			<meta id="vscode-rst-preview-data"
			data-settings="${JSON.stringify(initialData).replace(/"/g, '&quot;')}"
			data-strings="${JSON.stringify(previewStrings).replace(/"/g, '&quot;')}"
			data-state="${JSON.stringify(state || {}).replace(/"/g, '&quot;')}">
		<script src="${this.extensionResourcePath('pre.js')}" nonce="${nonce}"></script>
		<script src="${this.extensionResourcePath('index.js')}" nonce="${nonce}"></script>
		<base href="${rstDocument.uri.with({ scheme: 'vscode-resource' }).toString(true)}">
		</head>
			`);
            const newBody = newHead.replace('<body class="', `<body class="vscode-body ${config.scrollBeyondLastLine ? 'scrollBeyondLastLine' : ''} ${config.wordWrap ? 'wordWrap' : ''} ${config.rstEditorSelection ? 'showEditorSelection' : ''} `);
            const newAll = newBody.replace('</body>', `
			    <div class="code-line" data-line="${rstDocument.lineCount}"></div>
			</body>
			`);
            this.logger.log("Document line count: " + rstDocument.lineCount + "; element count: " + elementCount);
            if (rstDocument.lineCount < elementCount) {
                this.logger.log("WARN: documentl line count is less than element count.");
            }
            return newAll;
        }
        else {
            const parsedDoc = body.split(/\r?\n/).map((l, i) => l.replace(this.TAG_RegEx, (match, p1, p2, p3, p4, p5, p6, offset) => typeof p5 !== "string" ?
                `<${p1} class="code-line" data-line="${i + 1}" ${p2}` :
                `<${p1} ${p3} class="${p5} code-line" data-line="${i + 1}" ${p6}`)).join("\n");
            return `<!DOCTYPE html>
				<html>
				<head>
					<meta http-equiv="Content-type" content="text/html;charset=UTF-8">
					${csp}
					<meta id="vscode-rst-preview-data"
						data-settings="${JSON.stringify(initialData).replace(/"/g, '&quot;')}"
						data-strings="${JSON.stringify(previewStrings).replace(/"/g, '&quot;')}"
						data-state="${JSON.stringify(state || {}).replace(/"/g, '&quot;')}">
					<script src="${this.extensionResourcePath('pre.js')}" nonce="${nonce}"></script>
					<script src="${this.extensionResourcePath('index.js')}" nonce="${nonce}"></script>
					${this.getStyles(sourceUri, nonce, config)}
					<base href="${rstDocument.uri.with({ scheme: 'vscode-resource' }).toString(true)}">
				</head>
				<body class="vscode-body ${config.scrollBeyondLastLine ? 'scrollBeyondLastLine' : ''} ${config.wordWrap ? 'wordWrap' : ''} ${config.rstEditorSelection ? 'showEditorSelection' : ''}">
					${parsedDoc}
					<div class="code-line" data-line="${rstDocument.lineCount}"></div>
				</body>
				</html>`;
        }
    }
    extensionResourcePath(mediaFile) {
        return vscode.Uri.file(this.context.asAbsolutePath(path.join('media', mediaFile)))
            .with({ scheme: 'vscode-resource' })
            .toString();
    }
    fixHref(resource, href) {
        if (!href) {
            return href;
        }
        // Use href if it is already an URL
        const hrefUri = vscode.Uri.parse(href);
        if (['http', 'https'].indexOf(hrefUri.scheme) >= 0) {
            return hrefUri.toString();
        }
        // Use href as file URI if it is absolute
        if (path.isAbsolute(href) || hrefUri.scheme === 'file') {
            return vscode.Uri.file(href)
                .with({ scheme: 'vscode-resource' })
                .toString();
        }
        // Use a workspace relative path if there is a workspace
        let root = vscode.workspace.getWorkspaceFolder(resource);
        if (root) {
            return vscode.Uri.file(path.join(root.uri.fsPath, href))
                .with({ scheme: 'vscode-resource' })
                .toString();
        }
        // Otherwise look relative to the html file
        return vscode.Uri.file(path.join(path.dirname(resource.fsPath), href))
            .with({ scheme: 'vscode-resource' })
            .toString();
    }
    computeCustomStyleSheetIncludes(resource, config) {
        if (Array.isArray(config.styles)) {
            return config.styles.map(style => {
                return `<link rel="stylesheet" class="code-user-style" data-source="${style.replace(/"/g, '&quot;')}" href="${this.fixHref(resource, style)}" type="text/css" media="screen">`;
            }).join('\n');
        }
        return '';
    }
    getSettingsOverrideStyles(nonce, config) {
        return `<style nonce="${nonce}">
			body {
				${config.fontFamily ? `font-family: ${config.fontFamily};` : ''}
				${isNaN(config.fontSize) ? '' : `font-size: ${config.fontSize}px;`}
				${isNaN(config.lineHeight) ? '' : `line-height: ${config.lineHeight};`}
			}
		</style>`;
    }
    getStyles(resource, nonce, config) {
        const fix = (href) => vscode.Uri.file(href)
            .with({ scheme: "vscode-resource" })
            .toString();
        const baseStyles = config.baseStyles
            .map(href => `<link rel="stylesheet" type="text/css" href="${fix(href)}">`)
            .join("\n");
        return `${baseStyles}
			${this.getSettingsOverrideStyles(nonce, config)}
			${this.computeCustomStyleSheetIncludes(resource, config)}`;
    }
    getCspForResource(resource, nonce, useSphinx) {
        let securityLevel = this.cspArbiter.getSecurityLevelForResource(resource);
        if (useSphinx) {
            securityLevel = security_1.RSTPreviewSecurityLevel.AllowScriptsAndAllContent;
        }
        switch (securityLevel) {
            case security_1.RSTPreviewSecurityLevel.AllowInsecureContent:
                return `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src vscode-resource: http: https: data:; media-src vscode-resource: http: https: data:; script-src 'nonce-${nonce}'; style-src vscode-resource: 'unsafe-inline' http: https: data:; font-src vscode-resource: http: https: data:;">`;
            case security_1.RSTPreviewSecurityLevel.AllowInsecureLocalContent:
                return `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src vscode-resource: https: data: http://localhost:* http://127.0.0.1:*; media-src vscode-resource: https: data: http://localhost:* http://127.0.0.1:*; script-src 'nonce-${nonce}'; style-src vscode-resource: 'unsafe-inline' https: data: http://localhost:* http://127.0.0.1:*; font-src vscode-resource: https: data: http://localhost:* http://127.0.0.1:*;">`;
            case security_1.RSTPreviewSecurityLevel.AllowScriptsAndAllContent:
                return '';
            case security_1.RSTPreviewSecurityLevel.Strict:
            default:
                return `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src vscode-resource: https: data:; media-src vscode-resource: https: data:; script-src 'nonce-${nonce}'; style-src vscode-resource: 'unsafe-inline' https: data:; font-src vscode-resource: https: data:;">`;
        }
    }
}
exports.RSTContentProvider = RSTContentProvider;
//# sourceMappingURL=previewContentProvider.js.map