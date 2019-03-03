"use strict";
/* tslint:disable:max-line-length */
Object.defineProperty(exports, "__esModule", { value: true });
const conflictOptions = [
    {
        label: "base",
        description: "Choose the file that was the (unmodified) BASE revision before you tried to integrate changes"
    },
    {
        label: "working",
        description: "Assuming that you've manually handled the conflict resolution, choose the version of the file as it currently stands in your working copy."
    },
    {
        label: "mine-full",
        description: "Preserve all local modifications and discarding all changes fetched"
    },
    {
        label: "theirs-full",
        description: "Discard all local modifications and integrating all changes fetched"
    },
    {
        label: "mine-conflict",
        description: "Resolve conflicted files by preferring local modifications over the changes fetched"
    },
    {
        label: "theirs-conflict",
        description: "Resolve conflicted files by preferring the changes fetched from the server over local modifications"
    }
];
class ConflictItem {
    constructor(option) {
        this.option = option;
    }
    get label() {
        return this.option.label;
    }
    get description() {
        return this.option.description;
    }
}
function getConflictPickOptions() {
    return conflictOptions.map(option => new ConflictItem(option));
}
exports.getConflictPickOptions = getConflictPickOptions;
//# sourceMappingURL=conflictItems.js.map