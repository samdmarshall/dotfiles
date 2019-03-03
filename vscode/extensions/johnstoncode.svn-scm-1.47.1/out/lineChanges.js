"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
function applyLineChanges(original, modified, diffs) {
    const result = [];
    let currentLine = 0;
    for (const diff of diffs) {
        const isInsertion = diff.originalEndLineNumber === 0;
        const isDeletion = diff.modifiedEndLineNumber === 0;
        result.push(original.getText(new vscode_1.Range(currentLine, 0, isInsertion
            ? diff.originalStartLineNumber
            : diff.originalStartLineNumber - 1, 0)));
        if (!isDeletion) {
            let fromLine = diff.modifiedStartLineNumber - 1;
            let fromCharacter = 0;
            if (isInsertion && diff.originalStartLineNumber === original.lineCount) {
                fromLine = original.lineCount - 1;
                fromCharacter = original.lineAt(fromLine).range.end.character;
            }
            result.push(modified.getText(new vscode_1.Range(fromLine, fromCharacter, diff.modifiedEndLineNumber, 0)));
        }
        currentLine = isInsertion
            ? diff.originalStartLineNumber
            : diff.originalEndLineNumber;
    }
    result.push(original.getText(new vscode_1.Range(currentLine, 0, original.lineCount, 0)));
    return result.join("");
}
exports.applyLineChanges = applyLineChanges;
//# sourceMappingURL=lineChanges.js.map