"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
const resource_1 = require("../../resource");
const uri_1 = require("../../uri");
class IncomingChangeNode {
    constructor(uri, type, repository) {
        this.uri = uri;
        this.type = type;
        this.repository = repository;
    }
    get props() {
        return undefined;
    }
    get label() {
        return path.relative(this.repository.workspaceRoot, this.uri.fsPath);
    }
    get contextValue() {
        return `incomingChange:${this.type}`;
    }
    getTreeItem() {
        const item = new vscode_1.TreeItem(this.label, vscode_1.TreeItemCollapsibleState.None);
        item.iconPath = {
            dark: uri_1.getIconUri(`status-${this.type}`, "dark"),
            light: uri_1.getIconUri(`status-${this.type}`, "light")
        };
        item.contextValue = this.contextValue;
        item.command = this.getCommand();
        return item;
    }
    getChildren() {
        return Promise.resolve([]);
    }
    getCommand() {
        switch (this.type) {
            case "modified":
                return {
                    command: "svn.openChangeHead",
                    title: "Open Changes with HEAD",
                    arguments: [this.uri]
                };
            case "deleted":
                return {
                    command: "svn.openFile",
                    title: "Open File",
                    arguments: [this.uri]
                };
            case "added":
                return {
                    command: "svn.openHEADFile",
                    title: "Open File (HEAD)",
                    arguments: [
                        new resource_1.Resource(this.uri, this.type, undefined, "none", true)
                    ]
                };
        }
    }
}
exports.default = IncomingChangeNode;
//# sourceMappingURL=incomingChangeNode.js.map