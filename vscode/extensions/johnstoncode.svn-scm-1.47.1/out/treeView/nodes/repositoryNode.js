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
const uri_1 = require("../../uri");
const incomingChangesNode_1 = require("./incomingChangesNode");
class RepositoryNode {
    constructor(repository, svnProvider) {
        this.repository = repository;
        this.svnProvider = svnProvider;
        repository.onDidChangeStatus(() => {
            svnProvider.update(this);
        });
    }
    get label() {
        return path.basename(this.repository.workspaceRoot);
    }
    getTreeItem() {
        const item = new vscode_1.TreeItem(this.label, vscode_1.TreeItemCollapsibleState.Collapsed);
        item.iconPath = {
            dark: uri_1.getIconUri("repo", "dark"),
            light: uri_1.getIconUri("repo", "light")
        };
        return item;
    }
    getChildren() {
        return __awaiter(this, void 0, void 0, function* () {
            return [new incomingChangesNode_1.default(this.repository)];
        });
    }
}
exports.default = RepositoryNode;
//# sourceMappingURL=repositoryNode.js.map