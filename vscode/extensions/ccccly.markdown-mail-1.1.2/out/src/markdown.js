"use strict";
const highlight_js_1 = require("highlight.js");
const MarkdownIt = require('markdown-it');
class MarkDown {
    constructor() {
        this._md = new MarkdownIt({
            html: true,
            linkify: true,
            typographer: true,
            highlight: function (str, lang) {
                if (lang && highlight_js_1.default.getLanguage(lang)) {
                    try {
                        return highlight_js_1.default.highlight(lang, str).value;
                    }
                    catch (__) { }
                }
                return '';
            }
        });
    }
    renderMarkdownToHtml(content) {
        return this._md.render(content);
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = MarkDown;
//# sourceMappingURL=markdown.js.map