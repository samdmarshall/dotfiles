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
const crypto_1 = require("crypto");
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const configuration_1 = require("../helpers/configuration");
const tempFiles_1 = require("../tempFiles");
var LogTreeItemKind;
(function (LogTreeItemKind) {
    LogTreeItemKind[LogTreeItemKind["Repo"] = 1] = "Repo";
    LogTreeItemKind[LogTreeItemKind["Commit"] = 2] = "Commit";
    LogTreeItemKind[LogTreeItemKind["CommitDetail"] = 3] = "CommitDetail";
    LogTreeItemKind[LogTreeItemKind["TItem"] = 4] = "TItem";
})(LogTreeItemKind = exports.LogTreeItemKind || (exports.LogTreeItemKind = {}));
// svn:// or ^/ or WC-path
class SvnPath {
    constructor(path) {
        this.path = path;
    }
    toString() {
        return this.path;
    }
}
exports.SvnPath = SvnPath;
function transform(array, kind, parent) {
    return array.map(data => {
        return { kind, data, parent };
    });
}
exports.transform = transform;
function getIconObject(iconName) {
    // XXX Maybe use full path to extension?
    const iconsRootPath = path.join(__dirname, "..", "..", "icons");
    const toUri = (theme) => vscode_1.Uri.file(path.join(iconsRootPath, theme, `${iconName}.svg`));
    return {
        light: toUri("light"),
        dark: toUri("dark")
    };
}
exports.getIconObject = getIconObject;
function copyCommitToClipboard(what, item) {
    return __awaiter(this, void 0, void 0, function* () {
        const clipboard = vscode_1.env.clipboard;
        if (clipboard === undefined) {
            vscode_1.window.showErrorMessage("Clipboard is supported in VS Code 1.30 and newer");
            return;
        }
        if (item.kind === LogTreeItemKind.Commit) {
            const commit = item.data;
            switch (what) {
                case "msg":
                case "revision":
                    yield clipboard.writeText(commit[what]);
            }
        }
    });
}
exports.copyCommitToClipboard = copyCommitToClipboard;
function needFetch(cached, fetched, limit) {
    if (cached.length && cached[cached.length - 1].revision === "1") {
        return false;
    }
    if (fetched.length === 0 || fetched[fetched.length - 1].revision === "1") {
        return false;
    }
    if (fetched.length < limit) {
        return false;
    }
    return true;
}
function insertBaseMarker(item, entries, out) {
    const baseRev = item.persisted.baseRevision;
    if (entries.length &&
        baseRev &&
        parseInt(entries[0].revision, 10) > baseRev) {
        let i = 1;
        while (entries.length > i && parseInt(entries[i].revision, 10) > baseRev) {
            i++;
        }
        const titem = new vscode_1.TreeItem("BASE");
        titem.tooltip = "Log entries above do not exist in working copy";
        out.splice(i, 0, { kind: LogTreeItemKind.TItem, data: titem });
    }
    return undefined;
}
exports.insertBaseMarker = insertBaseMarker;
function checkIfFile(e, local) {
    if (e.localFullPath === undefined) {
        if (local) {
            vscode_1.window.showErrorMessage("No working copy for this path");
        }
        return undefined;
    }
    let stat;
    try {
        stat = fs.lstatSync(e.localFullPath.fsPath);
    }
    catch (_a) {
        vscode_1.window.showWarningMessage("Not available from this working copy: " + e.localFullPath);
        return false;
    }
    if (!stat.isFile()) {
        vscode_1.window.showErrorMessage("This target is not a file");
        return false;
    }
    return true;
}
exports.checkIfFile = checkIfFile;
/// @note: cached.svnTarget should be valid
function fetchMore(cached) {
    return __awaiter(this, void 0, void 0, function* () {
        let rfrom = cached.persisted.commitFrom;
        const entries = cached.entries;
        if (entries.length) {
            rfrom = entries[entries.length - 1].revision;
            rfrom = (Number.parseInt(rfrom, 10) - 1).toString();
        }
        let moreCommits = [];
        const limit = getLimit();
        try {
            moreCommits = yield cached.repo.log(rfrom, "1", limit, cached.svnTarget);
        }
        catch (_a) {
            // Item didn't exist
        }
        if (!needFetch(entries, moreCommits, limit)) {
            cached.isComplete = true;
        }
        entries.push(...moreCommits);
    });
}
exports.fetchMore = fetchMore;
function getLimit() {
    const limit = Number.parseInt(configuration_1.configuration.get("log.length") || "50", 10);
    if (isNaN(limit) || limit <= 0) {
        throw new Error("Invalid log.length setting value");
    }
    return limit;
}
exports.getLimit = getLimit;
const gravatarCache = new Map();
function md5(s) {
    const data = crypto_1.createHash("md5");
    data.write(s);
    return data.digest().toString("hex");
}
function getCommitIcon(author, size = 16) {
    if (!configuration_1.configuration.get("gravatars.enabled", true)) {
        return getIconObject("icon-commit");
    }
    let gravatar = gravatarCache.get(author);
    if (gravatar !== undefined) {
        return gravatar;
    }
    gravatar = vscode_1.Uri.parse(`https://www.gravatar.com/avatar/${md5(author)}.jpg?s=${size}&d=robohash`);
    gravatarCache.set(author, gravatar);
    return gravatar;
}
exports.getCommitIcon = getCommitIcon;
function getCommitLabel(commit) {
    const fstLine = commit.msg.split(/\r?\n/, 1)[0];
    return `${fstLine} • r${commit.revision}`;
}
exports.getCommitLabel = getCommitLabel;
function getCommitToolTip(commit) {
    let date = commit.date;
    if (!isNaN(Date.parse(date))) {
        date = new Date(date).toString();
    }
    return `Author: ${commit.author}
${date}
Revision: ${commit.revision}
Message: ${commit.msg}`;
}
exports.getCommitToolTip = getCommitToolTip;
function downloadFile(repo, arg, revision) {
    return __awaiter(this, void 0, void 0, function* () {
        if (revision === "BASE") {
            const nm = repo.getPathNormalizer();
            const ri = nm.parse(arg.toString(true));
            const localPath = ri.localFullPath;
            if (localPath === undefined || !fs.existsSync(localPath.path)) {
                const errorMsg = "BASE revision doesn't exist for " +
                    (localPath ? localPath.path : "remote path");
                vscode_1.window.showErrorMessage(errorMsg);
                throw new Error(errorMsg);
            }
            return localPath;
        }
        let out;
        try {
            out = yield repo.show(arg, revision);
        }
        catch (e) {
            vscode_1.window.showErrorMessage("Failed to open path");
            throw e;
        }
        return tempFiles_1.dumpSvnFile(arg, revision, out);
    });
}
function openDiff(repo, arg, r1, r2) {
    return __awaiter(this, void 0, void 0, function* () {
        const uri1 = yield downloadFile(repo, arg, r1);
        const uri2 = yield downloadFile(repo, arg, r2);
        const opts = {
            preview: true
        };
        const title = `${path.basename(arg.path)} (${r1} : ${r2})`;
        return vscode_1.commands.executeCommand("vscode.diff", uri1, uri2, title, opts);
    });
}
exports.openDiff = openDiff;
function openFileRemote(repo, arg, against) {
    return __awaiter(this, void 0, void 0, function* () {
        let out;
        try {
            out = yield repo.show(arg, against);
        }
        catch (_a) {
            vscode_1.window.showErrorMessage("Failed to open path");
            return;
        }
        const localUri = yield tempFiles_1.dumpSvnFile(arg, against, out);
        const opts = {
            preview: true
        };
        return vscode_1.commands.executeCommand("vscode.open", localUri, opts);
    });
}
exports.openFileRemote = openFileRemote;
//# sourceMappingURL=common.js.map