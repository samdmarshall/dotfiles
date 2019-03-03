"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const minimatch = require("minimatch");
function matchAll(path, patterns, opts = {}) {
    let match = false;
    patterns.forEach(pattern => {
        const isExclusion = pattern[0] === "!";
        // If we've got a match, only re-test for exclusions.
        // if we don't have a match, only re-test for inclusions.
        if (match !== isExclusion) {
            return;
        }
        match = minimatch(path, pattern, opts);
    });
    return match;
}
exports.matchAll = matchAll;
function match(pattern) {
    return new minimatch.Minimatch(pattern);
}
exports.match = match;
//# sourceMappingURL=globMatch.js.map