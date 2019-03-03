"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
class IgnoreSingleItem {
    constructor(expression, recursive = false) {
        this.expression = expression;
        this.recursive = recursive;
    }
    get label() {
        const text = this.recursive ? " (Recursive)" : "";
        return `${this.expression}${text}`;
    }
    get description() {
        const text = this.recursive ? " (Recursive)" : "";
        return `Add '${this.expression}' to 'svn:ignore'${text}`;
    }
}
exports.IgnoreSingleItem = IgnoreSingleItem;
function inputIgnoreList(repository, uris) {
    return __awaiter(this, void 0, void 0, function* () {
        if (uris.length === 0) {
            return false;
        }
        const regexExtension = new RegExp("\\.[^\\.]+(\\.map)?$", "i");
        if (uris.length === 1) {
            const uri = uris[0];
            const matchExt = uri.fsPath.match(regexExtension);
            const ext = matchExt && matchExt[0] ? matchExt[0] : "";
            const fileName = path.basename(uri.fsPath);
            const dirName = path.dirname(uri.fsPath);
            const picks = [];
            picks.push(new IgnoreSingleItem(fileName));
            if (ext) {
                picks.push(new IgnoreSingleItem("*" + ext));
            }
            picks.push(new IgnoreSingleItem(fileName, true));
            if (ext) {
                picks.push(new IgnoreSingleItem("*" + ext, true));
            }
            const pick = yield vscode_1.window.showQuickPick(picks);
            if (!pick) {
                return false;
            }
            return repository.addToIgnore([pick.expression], dirName, pick.recursive);
        }
        const count = uris.length;
        const recursive = "(Recursive)";
        const ignoreByFileName = `Ignore ${count} by filename`;
        const ignoreByExtension = `Ignore ${count} by extension`;
        const ignoreByFileNameRecursive = `Ignore ${count} by filename ${recursive}`;
        const ignoreByExtensionRecursive = `Ignore ${count} by extension ${recursive}`;
        const picks = [
            ignoreByFileName,
            ignoreByExtension,
            ignoreByFileNameRecursive,
            ignoreByExtensionRecursive
        ];
        const pick = yield vscode_1.window.showQuickPick(picks);
        if (!pick) {
            return false;
        }
        const isByFile = pick.startsWith(ignoreByFileName);
        const isRecursive = pick.endsWith(recursive);
        const byDir = {};
        for (const uri of uris) {
            const dirname = path.dirname(uri.fsPath);
            const filename = path.basename(uri.fsPath);
            const matchExt = uri.fsPath.match(regexExtension);
            const ext = matchExt && matchExt[0] ? matchExt[0] : "";
            if (typeof byDir[dirname] === "undefined") {
                byDir[dirname] = [];
            }
            if (isByFile) {
                byDir[dirname].push(filename);
            }
            else if (ext) {
                byDir[dirname].push("*" + ext);
            }
        }
        for (const dir in byDir) {
            if (byDir.hasOwnProperty(dir)) {
                const files = [...new Set(byDir[dir])]; // Unique list
                yield repository.addToIgnore(files, dir, isRecursive);
            }
        }
        return true;
    });
}
exports.inputIgnoreList = inputIgnoreList;
//# sourceMappingURL=ignoreitems.js.map