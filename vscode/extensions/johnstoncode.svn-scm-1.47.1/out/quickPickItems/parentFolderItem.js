"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class ParentFolderItem {
    constructor(path) {
        this.path = path;
    }
    get label() {
        return `$(arrow-left) back to /${this.path}`;
    }
    get description() {
        return `Back to parent`;
    }
}
exports.default = ParentFolderItem;
//# sourceMappingURL=parentFolderItem.js.map