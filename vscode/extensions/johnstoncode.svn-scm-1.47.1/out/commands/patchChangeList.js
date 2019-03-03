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
const changelistItems_1 = require("../changelistItems");
const command_1 = require("./command");
class PatchChangeList extends command_1.Command {
    constructor() {
        super("svn.patchChangeList", { repository: true });
    }
    execute(repository) {
        return __awaiter(this, void 0, void 0, function* () {
            const changelistName = yield changelistItems_1.getPatchChangelist(repository);
            if (!changelistName) {
                return;
            }
            const content = yield repository.patchChangelist(changelistName);
            yield this.showDiffPath(repository, content);
        });
    }
}
exports.PatchChangeList = PatchChangeList;
//# sourceMappingURL=patchChangeList.js.map