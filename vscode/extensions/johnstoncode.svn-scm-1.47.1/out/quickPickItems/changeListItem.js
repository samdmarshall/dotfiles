"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class ChangeListItem {
    constructor(group) {
        this.group = group;
    }
    get label() {
        return this.group.id.replace(/^changelist-/, "");
    }
    get description() {
        return this.group.label;
    }
    get resourceGroup() {
        return this.group;
    }
}
exports.default = ChangeListItem;
//# sourceMappingURL=changeListItem.js.map