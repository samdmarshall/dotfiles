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
const types_1 = require("../common/types");
const folderItem_1 = require("../quickPickItems/folderItem");
const newFolderItem_1 = require("../quickPickItems/newFolderItem");
const parentFolderItem_1 = require("../quickPickItems/parentFolderItem");
const configuration_1 = require("./configuration");
function getBranchName(folder) {
    const confs = [
        "layout.trunkRegex",
        "layout.branchesRegex",
        "layout.tagsRegex"
    ];
    for (const conf of confs) {
        const layout = configuration_1.configuration.get(conf);
        if (!layout) {
            continue;
        }
        const group = configuration_1.configuration.get(`${conf}Name`, 1) + 2;
        const regex = new RegExp(`(^|/)(${layout})$`);
        const matches = folder.match(regex);
        if (matches && matches[2] && matches[group]) {
            return {
                name: matches[group],
                path: matches[2]
            };
        }
    }
}
exports.getBranchName = getBranchName;
function selectBranch(repository, allowNew = false, folder) {
    return __awaiter(this, void 0, void 0, function* () {
        const promise = repository.repository.list(folder);
        vscode_1.window.withProgress({ location: vscode_1.ProgressLocation.Window, title: "Checking remote branches" }, () => promise);
        const list = yield promise;
        const dirs = list.filter(item => item.kind === types_1.SvnKindType.DIR);
        const picks = [];
        if (folder) {
            const parts = folder.split("/");
            parts.pop();
            const parent = parts.join("/");
            picks.push(new parentFolderItem_1.default(parent));
        }
        if (allowNew && folder && !!getBranchName(`${folder}/test`)) {
            picks.push(new newFolderItem_1.default(folder));
        }
        picks.push(...dirs.map(dir => new folderItem_1.default(dir, folder)));
        const choice = yield vscode_1.window.showQuickPick(picks);
        if (!choice) {
            return;
        }
        if (choice instanceof parentFolderItem_1.default) {
            return selectBranch(repository, allowNew, choice.path);
        }
        if (choice instanceof folderItem_1.default) {
            if (choice.branch) {
                return choice.branch;
            }
            return selectBranch(repository, allowNew, choice.path);
        }
        if (choice instanceof newFolderItem_1.default) {
            const result = yield vscode_1.window.showInputBox({
                prompt: "Please provide a branch name",
                ignoreFocusOut: true
            });
            if (!result) {
                return;
            }
            const name = result.replace(/^\.|\/\.|\.\.|~|\^|:|\/$|\.lock$|\.lock\/|\\|\*|\s|^\s*$|\.$/g, "-");
            const newBranch = getBranchName(`${folder}/${name}`);
            if (newBranch) {
                newBranch.isNew = true;
            }
            return newBranch;
        }
    });
}
exports.selectBranch = selectBranch;
//# sourceMappingURL=branch.js.map