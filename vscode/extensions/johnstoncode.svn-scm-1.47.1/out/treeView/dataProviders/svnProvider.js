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
const vscode_1 = require("vscode");
const repositoryNode_1 = require("../nodes/repositoryNode");
class SvnProvider {
    constructor(model) {
        this.model = model;
        this._onDidChangeTreeData = new vscode_1.EventEmitter();
        this.onDidChangeTreeData = this
            ._onDidChangeTreeData.event;
        vscode_1.commands.registerCommand("svn.treeview.refreshProvider", () => this.refresh());
    }
    refresh() {
        this._onDidChangeTreeData.fire();
    }
    getTreeItem(element) {
        return element.getTreeItem();
    }
    getChildren(element) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!this.model || this.model.openRepositories.length === 0) {
                return Promise.resolve([]);
            }
            if (element) {
                return element.getChildren();
            }
            const repositories = this.model.openRepositories.map(repository => {
                return new repositoryNode_1.default(repository.repository, this);
            });
            return repositories;
        });
    }
    update(node) {
        this._onDidChangeTreeData.fire(node);
    }
}
exports.default = SvnProvider;
//# sourceMappingURL=svnProvider.js.map