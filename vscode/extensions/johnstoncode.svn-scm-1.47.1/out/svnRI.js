"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = require("path");
const vscode_1 = require("vscode");
const decorators_1 = require("./decorators");
function pathOrRoot(uri) {
    return uri.path || "/";
}
exports.pathOrRoot = pathOrRoot;
class SvnRI {
    constructor(remoteRoot, branchRoot, checkoutRoot, 
    /** path relative from remoteRoot */
    _path, _revision) {
        this.remoteRoot = remoteRoot;
        this.branchRoot = branchRoot;
        this.checkoutRoot = checkoutRoot;
        this._path = _path;
        this._revision = _revision;
        if (_path.length === 0 || _path.charAt(0) === "/") {
            throw new Error("Invalid _path " + _path);
        }
    }
    get remoteFullPath() {
        return vscode_1.Uri.parse(this.remoteRoot.toString() + "/" + this._path);
    }
    get localFullPath() {
        if (this.checkoutRoot === undefined) {
            return undefined;
        }
        return vscode_1.Uri.file(path_1.posix.join(this.checkoutRoot.path, path_1.posix.relative(this.fromRepoToBranch, this._path)));
    }
    get relativeFromBranch() {
        return path_1.posix.relative(this.fromRepoToBranch, this._path);
    }
    get fromRepoToBranch() {
        return path_1.posix.relative(pathOrRoot(this.remoteRoot), pathOrRoot(this.branchRoot));
    }
    get revision() {
        return this._revision;
    }
    toString(withRevision) {
        return this.remoteFullPath + (withRevision ? this._revision || "" : "");
    }
}
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "remoteFullPath", null);
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "localFullPath", null);
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "relativeFromBranch", null);
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "fromRepoToBranch", null);
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "revision", null);
__decorate([
    decorators_1.memoize
], SvnRI.prototype, "toString", null);
exports.SvnRI = SvnRI;
//# sourceMappingURL=svnRI.js.map