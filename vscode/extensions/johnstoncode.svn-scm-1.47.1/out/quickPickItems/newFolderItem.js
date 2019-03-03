"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class NewFolderItem {
    constructor(_parent) {
        this._parent = _parent;
    }
    get label() {
        return `$(plus) Create new branch`;
    }
    get description() {
        return `Create new branch in "${this._parent}"`;
    }
}
exports.default = NewFolderItem;
//# sourceMappingURL=newFolderItem.js.map