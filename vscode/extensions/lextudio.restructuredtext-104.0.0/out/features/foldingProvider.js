"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const tableOfContentsProvider_1 = require("../tableOfContentsProvider");
const rangeLimit = 5000;
class ReStructuredTextFoldingProvider {
    constructor(engine) {
        this.engine = engine;
    }
    async getRegions(document) {
        const isStartRegion = (t) => /^\s*<!--\s*#?region\b.*-->/.test(t);
        const isEndRegion = (t) => /^\s*<!--\s*#?endregion\b.*-->/.test(t);
        const isRegionMarker = (token) => token.type === 'html_block' &&
            (isStartRegion(token.content) || isEndRegion(token.content));
        const tokens = await this.engine.parse(document.uri, document.getText());
        const regionMarkers = tokens.filter(isRegionMarker)
            .map(token => ({ line: token.map[0], isStart: isStartRegion(token.content) }));
        const nestingStack = [];
        return regionMarkers
            .map(marker => {
            if (marker.isStart) {
                nestingStack.push(marker);
            }
            else if (nestingStack.length && nestingStack[nestingStack.length - 1].isStart) {
                return new vscode.FoldingRange(nestingStack.pop().line, marker.line, vscode.FoldingRangeKind.Region);
            }
            else {
                // noop: invalid nesting (i.e. [end, start] or [start, end, end])
            }
            return null;
        })
            .filter((region) => !!region);
    }
    async provideFoldingRanges(document, _, _token) {
        const foldables = await Promise.all([
            this.getRegions(document),
            this.getHeaderFoldingRanges(document),
            this.getBlockFoldingRanges(document)
        ]);
        return [].concat.apply([], foldables).slice(0, rangeLimit);
    }
    async getHeaderFoldingRanges(document) {
        const tocProvider = new tableOfContentsProvider_1.TableOfContentsProvider(this.engine, document);
        const toc = await tocProvider.getToc();
        return toc.map(entry => {
            let endLine = entry.location.range.end.line;
            if (document.lineAt(endLine).isEmptyOrWhitespace && endLine >= entry.line + 1) {
                endLine = endLine - 1;
            }
            return new vscode.FoldingRange(entry.line, endLine);
        });
    }
    async getBlockFoldingRanges(document) {
        const isFoldableToken = (token) => {
            switch (token.type) {
                case 'fence':
                case 'list_item_open':
                    return token.map[1] > token.map[0];
                case 'html_block':
                    return token.map[1] > token.map[0] + 1;
                default:
                    return false;
            }
        };
        const tokens = await this.engine.parse(document.uri, document.getText());
        const multiLineListItems = tokens.filter(isFoldableToken);
        return multiLineListItems.map(listItem => {
            const start = listItem.map[0];
            let end = listItem.map[1] - 1;
            if (document.lineAt(end).isEmptyOrWhitespace && end >= start + 1) {
                end = end - 1;
            }
            return new vscode.FoldingRange(start, end);
        });
    }
}
exports.default = ReStructuredTextFoldingProvider;
//# sourceMappingURL=foldingProvider.js.map