"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class IgnoredChangeListItem {
    constructor(_id) {
        this._id = _id;
    }
    get label() {
        return this._id;
    }
    get description() {
        return "Ignored on commit";
    }
}
exports.default = IgnoredChangeListItem;
//# sourceMappingURL=ignoredChangeListItem.js.map