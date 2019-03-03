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
const configuration_1 = require("../helpers/configuration");
const command_1 = require("./command");
class Update extends command_1.Command {
    constructor() {
        super("svn.update", { repository: true });
    }
    execute(repository) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const ignoreExternals = configuration_1.configuration.get("update.ignoreExternals", false);
                const showUpdateMessage = configuration_1.configuration.get("showUpdateMessage", true);
                const result = yield repository.updateRevision(ignoreExternals);
                if (showUpdateMessage) {
                    vscode_1.window.showInformationMessage(result);
                }
            }
            catch (error) {
                console.error(error);
                vscode_1.window.showErrorMessage("Unable to update");
            }
        });
    }
}
exports.Update = Update;
//# sourceMappingURL=update.js.map