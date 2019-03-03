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
const path = require("path");
const vscode_1 = require("vscode");
const configuration_1 = require("../helpers/configuration");
const command_1 = require("./command");
class Resolved extends command_1.Command {
    constructor() {
        super("svn.resolved");
    }
    execute(uri) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!uri) {
                return;
            }
            const autoResolve = configuration_1.configuration.get("conflict.autoResolve");
            if (!autoResolve) {
                const basename = path.basename(uri.fsPath);
                const pick = yield vscode_1.window.showWarningMessage(`Mark the conflict as resolved for "${basename}"?`, { modal: true }, "Yes", "No");
                if (pick !== "Yes") {
                    return;
                }
            }
            const uris = [uri];
            yield this.runByRepository(uris, (repository, resources) => __awaiter(this, void 0, void 0, function* () {
                if (!repository) {
                    return;
                }
                const files = resources.map(resource => resource.fsPath);
                yield repository.resolve(files, "working");
            }));
        });
    }
}
exports.Resolved = Resolved;
//# sourceMappingURL=resolved.js.map