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
const command_1 = require("./command");
class AddToIgnoreSCM extends command_1.Command {
    constructor() {
        super("svn.addToIgnoreSCM");
    }
    execute(...resourceStates) {
        return __awaiter(this, void 0, void 0, function* () {
            const selection = yield this.getResourceStates(resourceStates);
            if (selection.length === 0) {
                return;
            }
            const uris = selection.map(resource => resource.resourceUri);
            return this.addToIgnore(uris);
        });
    }
}
exports.AddToIgnoreSCM = AddToIgnoreSCM;
//# sourceMappingURL=addToIgnoreSCM.js.map