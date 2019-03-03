"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const decorators_1 = require("../decorators");
const branch_1 = require("../helpers/branch");
class FolderItem {
    constructor(dir, parent) {
        this.dir = dir;
        this.parent = parent;
    }
    get label() {
        if (this.branch) {
            return `$(git-branch) ${this.dir.name}`;
        }
        return `$(file-directory) ${this.dir.name}`;
    }
    get description() {
        return `r${this.dir.commit.revision} | ${this.dir.commit.author} | ${new Date(this.dir.commit.date).toLocaleString()}`;
    }
    get path() {
        if (this.parent) {
            return `${this.parent}/${this.dir.name}`;
        }
        return this.dir.name;
    }
    get branch() {
        return branch_1.getBranchName(this.path);
    }
}
__decorate([
    decorators_1.memoize
], FolderItem.prototype, "branch", null);
exports.default = FolderItem;
//# sourceMappingURL=folderItem.js.map