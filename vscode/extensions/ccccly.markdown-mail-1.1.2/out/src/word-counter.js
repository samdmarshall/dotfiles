"use strict";
const vscode_1 = require("vscode");
class WordCounter {
    updateWordCount() {
        // Create as needed
        if (!this._statusBarItem) {
            this._statusBarItem = vscode_1.window.createStatusBarItem(vscode_1.StatusBarAlignment.Left);
        }
        // Get the current text editor
        let editor = vscode_1.window.activeTextEditor;
        if (!editor) {
            this._statusBarItem.hide();
            return;
        }
        let doc = editor.document;
        // Only update status if an Markdown file
        if (doc.languageId === "markdown") {
            let wordCount = this._getWordCount(doc);
            // Update the status bar
            this._statusBarItem.text = wordCount !== 1 ? `${wordCount} Words` : '1 Word';
            this._statusBarItem.show();
        }
        else {
            this._statusBarItem.hide();
        }
    }
    _getWordCount(doc) {
        let docContent = doc.getText();
        let cnWordCounter = docContent.match(/[\u4E00-\u9FA5]/g) || [];
        docContent = docContent.replace(/[\u4E00-\u9FA5]/g, '');
        docContent = docContent.replace(/(< ([^>]+)<)/g, '').replace(/\s+/g, ' ');
        docContent = docContent.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
        let wordCount = 0;
        if (docContent != "") {
            wordCount = docContent.split(" ").length + cnWordCounter.length;
        }
        return wordCount;
    }
    dispose() {
        this._statusBarItem.dispose();
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = WordCounter;
//# sourceMappingURL=word-counter.js.map