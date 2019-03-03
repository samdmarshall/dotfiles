//    Copyright 2016, 2017 Yoshi Yamaguchi
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// The module "vscode" contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require("vscode");
const notation_1 = require("./notation");
const custom_1 = require("./licenses/custom");
const al2_1 = require("./licenses/al2");
const bsd3_1 = require("./licenses/bsd3");
const bsd2_1 = require("./licenses/bsd2");
const bsl1_1 = require("./licenses/bsl1");
const gplv2_1 = require("./licenses/gplv2");
const gplv3_1 = require("./licenses/gplv3");
const lgplv3_1 = require("./licenses/lgplv3");
const agplv3_1 = require("./licenses/agplv3");
const mit_1 = require("./licenses/mit");
const mplv2_1 = require("./licenses/mplv2");
const ccby30_1 = require("./licenses/ccby30");
const ccby40_1 = require("./licenses/ccby40");
const ccbync30_1 = require("./licenses/ccbync30");
const ccbync40_1 = require("./licenses/ccbync40");
const ccbyncnd30_1 = require("./licenses/ccbyncnd30");
const ccbyncnd40_1 = require("./licenses/ccbyncnd40");
const ccbyncsa30_1 = require("./licenses/ccbyncsa30");
const ccbyncsa40_1 = require("./licenses/ccbyncsa40");
const ccbynd30_1 = require("./licenses/ccbynd30");
const ccbynd40_1 = require("./licenses/ccbynd40");
const ccbysa30_1 = require("./licenses/ccbysa30");
const ccbysa40_1 = require("./licenses/ccbysa40");
const cczero1_1 = require("./licenses/cczero1");
const wtfpl_1 = require("./licenses/wtfpl");
const zlib_1 = require("./licenses/zlib");
const path = require("path");
const os = require("os");
const util_1 = require("util");
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log("'licenser' is activated.");
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json
    let licenser = new Licenser();
    context.subscriptions.push(licenser);
}
exports.activate = activate;
// constants for default properties.
const defaultLicenseType = "AL2";
const defaultLicenseFilename = "LICENSE";
const chooseFromList = "choose from list";
const availableLicenses = new Map([
    ["AL2", { displayName: "AL2", creatorFn: (author, _) => new al2_1.AL2(author) }],
    ["BSD3", { displayName: "BSD3", creatorFn: (author, _) => new bsd3_1.BSD3(author) }],
    ["BSD2", { displayName: "BSD2", creatorFn: (author, _) => new bsd2_1.BSD2(author) }],
    ["BSL1", { displayName: "BSL1", creatorFn: (author, _) => new bsl1_1.BSL1(author) }],
    ["GPLV2", { displayName: "GPLv2", creatorFn: (author, projectName) => new gplv2_1.GPLv2(author, projectName) }],
    ["GPLV3", { displayName: "GPLv3", creatorFn: (author, projectName) => new gplv3_1.GPLv3(author, projectName) }],
    ["LGPLV3", { displayName: "LGPLv3", creatorFn: (author, projectName) => new lgplv3_1.LGPLv3(author, projectName) }],
    ["AGPLV3", { displayName: "AGPLv3", creatorFn: (author, _) => new agplv3_1.AGPLv3(author) }],
    ["MIT", { displayName: "MIT", creatorFn: (author, _) => new mit_1.MIT(author) }],
    ["MPLV2", { displayName: "MPLv2", creatorFn: (author, _) => new mplv2_1.MPLv2(author) }],
    ["CC-BY-3", { displayName: "CC-BY-3", creatorFn: (author, projectName) => new ccby30_1.CCBY3(author, projectName) }],
    ["CC-BY-4", { displayName: "CC-BY-4", creatorFn: (author, projectName) => new ccby40_1.CCBY4(author, projectName) }],
    ["CC-BY-NC-3", { displayName: "CC-BY-NC-3", creatorFn: (author, projectName) => new ccbync30_1.CCBYNC3(author, projectName) }],
    ["CC-BY-NC-4", { displayName: "CC-BY-NC-4", creatorFn: (author, projectName) => new ccbync40_1.CCBYNC4(author, projectName) }],
    ["CC-BY-NC-ND-3", { displayName: "CC-BY-NC-ND-3", creatorFn: (author, projectName) => new ccbyncnd30_1.CCBYNCND3(author, projectName) }],
    ["CC-BY-NC-ND-4", { displayName: "CC-BY-NC-ND-4", creatorFn: (author, projectName) => new ccbyncnd40_1.CCBYNCND4(author, projectName) }],
    ["CC-BY-NC-SA-3", { displayName: "CC-BY-NC-SA-3", creatorFn: (author, projectName) => new ccbyncsa30_1.CCBYNCSA3(author, projectName) }],
    ["CC-BY-NC-SA-4", { displayName: "CC-BY-NC-SA-4", creatorFn: (author, projectName) => new ccbyncsa40_1.CCBYNCSA4(author, projectName) }],
    ["CC-BY-ND-3", { displayName: "CC-BY-ND-3", creatorFn: (author, projectName) => new ccbynd30_1.CCBYND3(author, projectName) }],
    ["CC-BY-ND-4", { displayName: "CC-BY-ND-4", creatorFn: (author, projectName) => new ccbynd40_1.CCBYND4(author, projectName) }],
    ["CC-BY-SA-3", { displayName: "CC-BY-SA-3", creatorFn: (author, projectName) => new ccbysa30_1.CCBYSA3(author, projectName) }],
    ["CC-BY-SA-4", { displayName: "CC-BY-SA-4", creatorFn: (author, projectName) => new ccbysa40_1.CCBYSA4(author, projectName) }],
    ["CC0-1", { displayName: "CC0-1", creatorFn: (author, projectName) => new cczero1_1.CC01(author, projectName) }],
    ["WTFPL", { displayName: "WTFPL", creatorFn: (author, _) => new wtfpl_1.WTFPL(author) }],
    ["ZLIB", { displayName: "zlib", creatorFn: (author, _) => new zlib_1.Zlib(author) }],
]);
// Licenser handles LICENSE file creation and license header insertion.
class Licenser {
    constructor() {
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        let licenseType = licenserSetting.get("license", undefined);
        if (licenseType === undefined) {
            vscode.window.showWarningMessage("set your preferred license as 'licenser.license' in configuration. Apache License version 2.0 will be used as default.");
            licenseType = defaultLicenseType;
        }
        this.author = this.getAuthor();
        console.log("Licenser.author: " + this.author);
        const subscriptions = [];
        vscode.commands.registerCommand("extension.createLicenseFile", () => { this.create(); });
        vscode.commands.registerCommand("extension.anyLicenseHeader", () => { this.arbitrary(); });
        vscode.commands.registerCommand("extension.insertLicenseHeader", () => { this.insert(); });
        vscode.window.onDidChangeActiveTextEditor(this._onDidChangeActiveTextEditor, this, subscriptions);
    }
    /**
     * create generates LICENSE file and save it in opened workspace.
     */
    create() {
        const root = vscode.workspace.rootPath;
        if (root === undefined) {
            vscode.window.showErrorMessage("No directory is opened.");
            return;
        }
        this._chooseLicenseType().then(licenseType => {
            if (!util_1.isNullOrUndefined(licenseType)) {
                const license = this.getLicense(licenseType);
                this._doCreateLicense(root, license);
            }
        });
    }
    _chooseLicenseType() {
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        let licenseType = licenserSetting.get("license");
        if (util_1.isNullOrUndefined(licenseType) || licenseType.toLowerCase() == chooseFromList) {
            return vscode.window.showQuickPick(Array.from(availableLicenses.values()).map(info => info.displayName));
        }
        return new Promise((resolve, _) => resolve(licenseType));
    }
    _doCreateLicense(root, license) {
        const uri = vscode.Uri.parse("untitled:" + root + path.sep + defaultLicenseFilename);
        vscode.workspace.openTextDocument(uri).then((doc) => {
            vscode.window.showTextDocument(doc).then((editor) => {
                editor.edit((ed) => {
                    ed.insert(doc.positionAt(0), license.termsAndConditions());
                }).then((done) => {
                    if (done) {
                        doc.save().then((saved) => {
                            vscode.window.showInformationMessage(`Successfully saved: ${uri}`);
                        }, (reason) => {
                            console.log("saved", reason);
                        });
                    }
                }, (reason) => {
                    console.log("ed.insert", reason);
                    vscode.window.showErrorMessage(reason);
                });
            });
        }, (reason) => {
            console.log("openTextDocument", reason);
            vscode.window.showErrorMessage(reason);
        });
    }
    _insert(license) {
        const editor = vscode.window.activeTextEditor;
        const doc = editor.document;
        const langId = editor.document.languageId;
        const header = this.getLicenseHeader(license, langId);
        // handle shebang
        const firstLine = doc.getText(new vscode.Range(0, 0, 1, 0));
        const position = this.findInsertionPosition(firstLine, langId);
        editor.edit((ed) => {
            console.log("header:", header);
            ed.insert(doc.positionAt(position), header);
        }).then((done) => {
            if (done) {
                doc.save().then((saved) => {
                    console.log("Inserted license header");
                }, (reason) => {
                    console.log("doc.save", reason);
                });
            }
        }, (reason) => {
            console.log("editor.edit", reason);
            vscode.window.showErrorMessage(reason);
        });
    }
    /**
     * insert embeds license header text into the first line of the opened file.
     */
    insert() {
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        let licenseType = licenserSetting.get("license");
        const license = this.getLicense(licenseType);
        this._insert(license);
    }
    arbitrary() {
        vscode.window.showInputBox({
            prompt: "Specify the license short name to insert. (see package.json for all the candidates)",
            placeHolder: "AL2",
        }).then((shortName) => {
            if (shortName !== undefined) {
                const license = this.getLicense(shortName);
                this._insert(license);
            }
        });
    }
    /**
     * findInsertionPosition returns the position to which insert() should insert
     * @param range header text area (usually first line of the file.)
     * @param langId language ID
     */
    findInsertionPosition(range, langId) {
        console.log("firstLine: " + range);
        switch (langId) {
            case "php":
                return range.startsWith("<?php") ? range.length : 0;
            default:
                return range.startsWith("#!") ? range.length : 0;
        }
    }
    _onDidChangeActiveTextEditor() {
        vscode.window.onDidChangeActiveTextEditor(e => {
            let licenserSetting = vscode.workspace.getConfiguration("licenser");
            let autoInsertionDisabled = licenserSetting.get("disableAutoHeaderInsertion");
            if (autoInsertionDisabled) {
                return;
            }
            const fileName = path.win32.basename(e.document.fileName);
            if (fileName !== defaultLicenseFilename) {
                const doc = e.document;
                const contents = doc.getText();
                if (contents.length > 0) {
                    return;
                }
                for (let id in notation_1.notations) {
                    if (id === doc.languageId) {
                        this.insert();
                    }
                }
            }
        });
    }
    /**
     * getLicense returns License instance with licenser.license setting.
     * @param typ License type specified in settings.json.
     */
    getLicense(typ) {
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        let projectName = licenserSetting.get("projectName", undefined);
        console.log("Project Name from settings: " + projectName);
        if (projectName !== undefined && projectName === "") {
            let root = vscode.workspace.rootPath;
            projectName = path.basename(root);
        }
        console.log("Project Name used: " + projectName);
        const licenseKey = typ.toUpperCase();
        if (licenseKey === "CUSTOM") {
            let customTermsAndConditions = licenserSetting.get("customTermsAndConditions");
            let customHeader = licenserSetting.get("customHeader");
            let fileName = vscode.window.activeTextEditor.document.fileName;
            return new custom_1.Custom(this.author, projectName, customTermsAndConditions, customHeader, fileName);
        }
        let info = availableLicenses.get(licenseKey);
        if (util_1.isNullOrUndefined(info)) {
            info = availableLicenses.get(defaultLicenseType);
        }
        return info.creatorFn(this.author, projectName);
    }
    /**
     * getLicenseHeader returns license header string.
     * @param license License instance initialized from lincenser.license.
     * @param langId language ID for the file working on.
     */
    getLicenseHeader(license, langId) {
        let notation = notation_1.notations[langId] ? notation_1.notations[langId] : notation_1.notations["plaintext"]; // return plaintext's comment when langId is unexpected.
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        const preferSingleLineStyle = licenserSetting.get("useSingleLineStyle", true);
        const [l, r] = notation.multi;
        if (preferSingleLineStyle) {
            if (notation.hasSingle()) {
                return this.singleLineCommentHeader(license, notation.single);
            }
            else if (notation.hasMulti()) {
                return this.multiLineCommentHeader(license, l, r, notation.ornament);
            }
        }
        else {
            if (notation.hasMulti()) {
                return this.multiLineCommentHeader(license, l, r, notation.ornament);
            }
            else if (notation.hasSingle()) {
                return this.singleLineCommentHeader(license, notation.single);
            }
        }
    }
    /**
     * singleLineCommentHeader returns license header string with single line comment style.
     * @param license License instance initialzed from licenser.license.
     * @param token single line comment token.
     */
    singleLineCommentHeader(license, token) {
        let original = license.header().split("\n");
        let header = "";
        for (const line of original) {
            if (original.length > 0) {
                header += token + " " + line + "\n";
            }
            else {
                header += token;
            }
        }
        return header + "\n";
    }
    /**
     * multiLineCommentHeader returns license header string with multiple line comment style.
     * @param license License instance initialized from licenser.License.
     * @param start multiplie line comment start string.
     * @param end multiple line comment end string.
     * @param ornament multiple line comment ornament string.
     */
    multiLineCommentHeader(license, start, end, ornament) {
        let original = license.header().split("\n");
        let header = start + "\n";
        for (const line of original) {
            if (original.length > 0) {
                header += ornament + " " + line + "\n";
            }
        }
        header += end + "\n";
        return header + "\n";
    }
    /**
     * getAuthor fetches author name string from one of the followings in this order.
     *   1. licenser.author
     *   2. OS environment.
     */
    getAuthor() {
        let licenserSetting = vscode.workspace.getConfiguration("licenser");
        let author = licenserSetting.get("author", undefined);
        console.log("Author from setting: " + author);
        if (author !== undefined && author.length !== 0) {
            return author;
        }
        vscode.window.showWarningMessage("set author name as ’licenser.author’ in configuration. OS username will be used as default.");
        switch (os.platform()) {
            case "win32":
                const userprofile = process.env.USERPROFILE;
                if (userprofile === undefined) {
                    vscode.window.showErrorMessage("Set USERPROFILE in your environment variables.");
                }
                author = userprofile.split(path.sep)[2];
                break;
            case "darwin":
                author = process.env.USER;
                break;
            case "linux":
                author = process.env.USER;
                break;
            default:
                vscode.window.showErrorMessage("Unsupported OS.");
                break;
        }
        return author;
    }
    dispose() {
        this._disposable.dispose();
    }
}
// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map