"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const types_1 = require("./common/types");
function done(promise) {
    return promise.then(() => void 0);
}
exports.done = done;
function anyEvent(...events) {
    return (listener, thisArgs = null, disposables) => {
        const result = combinedDisposable(events.map(event => event((i) => listener.call(thisArgs, i))));
        if (disposables) {
            disposables.push(result);
        }
        return result;
    };
}
exports.anyEvent = anyEvent;
function filterEvent(event, filter) {
    return (listener, thisArgs = null, disposables) => event((e) => filter(e) && listener.call(thisArgs, e), null, disposables);
}
exports.filterEvent = filterEvent;
function dispose(disposables) {
    disposables.forEach(disposable => disposable.dispose());
    return [];
}
exports.dispose = dispose;
function combinedDisposable(disposables) {
    return toDisposable(() => dispose(disposables));
}
exports.combinedDisposable = combinedDisposable;
function toDisposable(dispose) {
    return { dispose };
}
exports.toDisposable = toDisposable;
function onceEvent(event) {
    return (listener, thisArgs = null, disposables) => {
        const result = event((e) => {
            result.dispose();
            return listener.call(thisArgs, e);
        }, null, disposables);
        return result;
    };
}
exports.onceEvent = onceEvent;
function eventToPromise(event) {
    return new Promise(c => onceEvent(event)(c));
}
exports.eventToPromise = eventToPromise;
const regexNormalizePath = new RegExp(path.sep === "/" ? "\\\\" : "/", "g");
const regexNormalizeWindows = new RegExp("^\\\\(\\w:)", "g");
function fixPathSeparator(file) {
    file = file.replace(regexNormalizePath, path.sep);
    file = file.replace(regexNormalizeWindows, "$1"); // "\t:\test" => "t:\test"
    return file;
}
exports.fixPathSeparator = fixPathSeparator;
function normalizePath(file) {
    file = fixPathSeparator(file);
    // IF Windows
    if (path.sep === "\\") {
        file = file.toLowerCase();
    }
    return file;
}
exports.normalizePath = normalizePath;
function isDescendant(parent, descendant) {
    parent = parent.replace(/[\\\/]/g, path.sep);
    descendant = descendant.replace(/[\\\/]/g, path.sep);
    // IF Windows
    if (path.sep === "\\") {
        parent = parent.replace(/^\\/, "").toLowerCase();
        descendant = descendant.replace(/^\\/, "").toLowerCase();
    }
    if (parent === descendant) {
        return true;
    }
    if (parent.charAt(parent.length - 1) !== path.sep) {
        parent += path.sep;
    }
    return descendant.startsWith(parent);
}
exports.isDescendant = isDescendant;
function camelcase(name) {
    return name
        .replace(/(?:^\w|[A-Z]|\b\w)/g, (letter, index) => {
        return index === 0 ? letter.toLowerCase() : letter.toUpperCase();
    })
        .replace(/[\s\-]+/g, "");
}
exports.camelcase = camelcase;
/* tslint:disable:no-empty */
let hasDecorationProvider = false;
function hasSupportToDecorationProvider() {
    return hasDecorationProvider;
}
exports.hasSupportToDecorationProvider = hasSupportToDecorationProvider;
try {
    const fake = {
        onDidChangeDecorations: (value) => toDisposable(() => { }),
        provideDecoration: (uri, token) => { }
    };
    const disposable = vscode_1.window.registerDecorationProvider(fake);
    hasDecorationProvider = true;
    // disposable.dispose(); // Not dispose to prevent: Cannot read property 'provideDecoration' of undefined
}
catch (error) { }
let hasRegisterDiffCommand = false;
function hasSupportToRegisterDiffCommand() {
    return hasRegisterDiffCommand;
}
exports.hasSupportToRegisterDiffCommand = hasSupportToRegisterDiffCommand;
try {
    const disposable = vscode_1.commands.registerDiffInformationCommand("svn.testDiff", () => { });
    hasRegisterDiffCommand = true;
    disposable.dispose();
}
catch (error) { }
function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
exports.timeout = timeout;
function isReadOnly(operation) {
    switch (operation) {
        case types_1.Operation.CurrentBranch:
        case types_1.Operation.Log:
        case types_1.Operation.Show:
        case types_1.Operation.Info:
            return true;
        default:
            return false;
    }
}
exports.isReadOnly = isReadOnly;
/**
 * Remove directory recursively
 * @param {string} dirPath
 * @see https://stackoverflow.com/a/42505874/3027390
 */
function deleteDirectory(dirPath) {
    if (fs.existsSync(dirPath) && fs.lstatSync(dirPath).isDirectory()) {
        fs.readdirSync(dirPath).forEach((entry) => {
            const entryPath = path.join(dirPath, entry);
            if (fs.lstatSync(entryPath).isDirectory()) {
                deleteDirectory(entryPath);
            }
            else {
                fs.unlinkSync(entryPath);
            }
        });
        fs.rmdirSync(dirPath);
    }
}
exports.deleteDirectory = deleteDirectory;
function unwrap(maybeT) {
    if (maybeT === undefined) {
        throw new Error("undefined unwrap");
    }
    return maybeT;
}
exports.unwrap = unwrap;
//# sourceMappingURL=util.js.map