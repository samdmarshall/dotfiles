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
// use import { promises as fs } from "fs"; when nodejs will be updated
const fs = require("fs");
const os = require("os");
const path = require("path");
const util = require("util");
const vscode_1 = require("vscode");
const writeFile = util.promisify(fs.writeFile);
exports.tempdir = path.join(os.tmpdir(), "vscode-svn");
function dumpSvnFile(snvUri, revision, payload) {
    return __awaiter(this, void 0, void 0, function* () {
        if (!fs.existsSync(exports.tempdir)) {
            yield fs.mkdirSync(exports.tempdir);
        }
        const fname = `r${revision}_${path.basename(snvUri.fsPath)}`;
        const fpath = path.join(exports.tempdir, fname);
        yield writeFile(fpath, payload);
        return vscode_1.Uri.file(fpath);
    });
}
exports.dumpSvnFile = dumpSvnFile;
//# sourceMappingURL=tempFiles.js.map