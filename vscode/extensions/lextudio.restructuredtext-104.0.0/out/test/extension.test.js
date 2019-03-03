"use strict";
//
// Note: This example test is leveraging the Mocha test framework.
// Please refer to their documentation on https://mochajs.org/ for help.
//
Object.defineProperty(exports, "__esModule", { value: true });
// The module 'assert' provides assertion methods from node
const assert = require("assert");
// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
const rstEngine_1 = require("../rstEngine");
const path = require("path");
const fs = require("fs");
const initialize_1 = require("./initialize");
const python_1 = require("../python");
// Defines a Mocha test suite to group tests of similar kind together
let engine;
let python;
let logger = {
    log: () => void 0,
    updateConfiguration: () => void 0
};
suite("Extension Tests", function () {
    suiteSetup(async function () {
        this.timeout(30000);
        try {
            await initialize_1.initialize();
            python = new python_1.Python(logger);
            engine = new rstEngine_1.RSTEngine(python, logger, null);
        }
        catch (e) {
            throw e;
        }
    });
    suiteTeardown(function (done) {
        initialize_1.closeActiveWindows().then(done, done);
    });
    teardown(function (done) {
        initialize_1.closeActiveWindows().then(done, done);
    });
    test("Example 1 full preview", async function () {
        this.timeout(30000);
        const editor = await initialize_1.openFile(path.join(initialize_1.samplePath, "docutils", "example1.rst"));
        const val = await engine.preview(editor.document);
        return new Promise((res, rej) => {
            fs.readFile(path.join(initialize_1.samplePath, "docutils", "example1Full.html"), "utf8", (err, expected) => {
                if (err) {
                    rej(err);
                }
                assert.equal(val.split(/\r?\n/).join("\n"), expected.split(/\r?\n/).join("\n"), "Preview Generated HTML does not match expected");
                res();
            });
        });
    });
    test("Example 1 to HTML", async function () {
        this.timeout(30000);
        const editor = await initialize_1.openFile(path.join(initialize_1.samplePath, "docutils", "example1.rst"));
        const val = await engine.compile(path.join(initialize_1.samplePath, "docutils", "example1.rst"), editor.document.uri, '', true);
        return new Promise((res, rej) => {
            fs.readFile(path.join(initialize_1.samplePath, "docutils", "example1.html"), "utf8", (err, expected) => {
                if (err) {
                    rej(err);
                }
                assert.equal(val.split(/\r?\n/).join("\n"), expected.split(/\r?\n/).join("\n"), "Generated HTML does not match expected");
                res();
            });
        });
    });
    test("Sphinx to HTML", async function () {
        this.timeout(30000);
        const editor = await initialize_1.openFile(path.join(initialize_1.samplePath, "sphinx", "index.rst"));
        const val = await engine.compile(path.join(initialize_1.samplePath, "sphinx", "index.rst"), editor.document.uri, path.join(initialize_1.samplePath, 'sphinx'), false);
        return new Promise((res, rej) => {
            fs.readFile(path.join(initialize_1.samplePath, "index.html"), "utf8", (err, expected) => {
                if (err) {
                    rej(err);
                }
                assert.equal(val.split(/\r?\n/).join("\n"), expected.split(/\r?\n/).join("\n"), "Generated HTML does not match expected");
                res();
            });
        });
    });
});
//# sourceMappingURL=extension.test.js.map