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
const pathNormalizer_1 = require("./pathNormalizer");
class RemoteRepository {
    constructor(repo) {
        this.repo = repo;
        this.info = repo.info;
    }
    static open(svn, uri) {
        return __awaiter(this, void 0, void 0, function* () {
            const repo = yield svn.open(uri.toString(true), "");
            return new RemoteRepository(repo);
        });
    }
    getPathNormalizer() {
        return new pathNormalizer_1.PathNormalizer(this.info);
    }
    get branchRoot() {
        return vscode_1.Uri.parse(this.info.url);
    }
    log(rfrom, rto, limit, target) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.repo.log(rfrom, rto, limit, target);
        });
    }
    show(filePath, revision) {
        return __awaiter(this, void 0, void 0, function* () {
            return this.repo.show(filePath, revision);
        });
    }
}
exports.RemoteRepository = RemoteRepository;
//# sourceMappingURL=remoteRepository.js.map