"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = require("path");
const nativepath = require("path");
const vscode_1 = require("vscode");
const decorators_1 = require("./decorators");
const svnRI_1 = require("./svnRI");
var ResourceKind;
(function (ResourceKind) {
    ResourceKind[ResourceKind["LocalRelative"] = 0] = "LocalRelative";
    ResourceKind[ResourceKind["LocalFull"] = 1] = "LocalFull";
    ResourceKind[ResourceKind["RemoteFull"] = 2] = "RemoteFull";
})(ResourceKind = exports.ResourceKind || (exports.ResourceKind = {}));
/**
 * create from Repository class
 */
class PathNormalizer {
    constructor(repoInfo) {
        this.repoInfo = repoInfo;
        this.repoRoot = vscode_1.Uri.parse(repoInfo.repository.root);
        this.branchRoot = vscode_1.Uri.parse(repoInfo.url);
        if (repoInfo.wcInfo) {
            this.checkoutRoot = vscode_1.Uri.file(repoInfo.wcInfo.wcrootAbspath);
        }
    }
    /** svn://foo.org/domain/trunk/x -> trunk/x */
    getFullRepoPathFromUrl(fpath) {
        if (fpath.startsWith("/")) {
            return fpath.substr(1);
        }
        else if (fpath.startsWith("svn://") || fpath.startsWith("file://")) {
            const target = vscode_1.Uri.parse(fpath).path;
            return path_1.posix.relative(svnRI_1.pathOrRoot(this.repoRoot), target);
        }
        else {
            throw new Error("unknown path");
        }
    }
    parse(fpath, kind = ResourceKind.RemoteFull, rev) {
        let target;
        if (kind === ResourceKind.RemoteFull) {
            target = this.getFullRepoPathFromUrl(fpath);
        }
        else if (kind === ResourceKind.LocalFull) {
            if (!path_1.posix.isAbsolute(fpath)) {
                throw new Error("Path isn't absolute");
            }
            if (this.checkoutRoot === undefined) {
                throw new Error("Local paths are not");
            }
            target = nativepath.relative(this.checkoutRoot.fsPath, fpath);
            target = path_1.posix.join(this.fromRootToBranch(), target);
        }
        else if (kind === ResourceKind.LocalRelative) {
            if (path_1.posix.isAbsolute(fpath)) {
                throw new Error("Path is absolute");
            }
            if (this.checkoutRoot === undefined) {
                throw new Error("Local paths are not");
            }
            target = path_1.posix.join(this.fromRootToBranch(), fpath);
        }
        else {
            throw new Error("unsupported kind");
        }
        return new svnRI_1.SvnRI(this.repoRoot, this.branchRoot, this.checkoutRoot, target, rev);
    }
    fromRootToBranch() {
        return path_1.posix.relative(svnRI_1.pathOrRoot(this.repoRoot), svnRI_1.pathOrRoot(this.branchRoot));
    }
    fromBranchToRoot() {
        return path_1.posix.relative(svnRI_1.pathOrRoot(this.branchRoot), svnRI_1.pathOrRoot(this.repoRoot));
    }
}
__decorate([
    decorators_1.memoize
], PathNormalizer.prototype, "fromRootToBranch", null);
__decorate([
    decorators_1.memoize
], PathNormalizer.prototype, "fromBranchToRoot", null);
exports.PathNormalizer = PathNormalizer;
//# sourceMappingURL=pathNormalizer.js.map