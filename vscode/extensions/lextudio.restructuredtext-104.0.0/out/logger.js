"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const lazy_1 = require("./util/lazy");
var Trace;
(function (Trace) {
    Trace[Trace["Off"] = 0] = "Off";
    Trace[Trace["Verbose"] = 1] = "Verbose";
})(Trace = exports.Trace || (exports.Trace = {}));
(function (Trace) {
    function fromString(value) {
        value = value.toLowerCase();
        switch (value) {
            case 'off':
                return Trace.Off;
            case 'verbose':
                return Trace.Verbose;
            default:
                return Trace.Off;
        }
    }
    Trace.fromString = fromString;
})(Trace = exports.Trace || (exports.Trace = {}));
function isString(value) {
    return Object.prototype.toString.call(value) === '[object String]';
}
class Logger {
    constructor() {
        this.outputChannel = lazy_1.lazy(() => vscode.window.createOutputChannel('reStructuredText'));
        this.updateConfiguration();
    }
    log(message, data) {
        if (this.trace === Trace.Verbose) {
            this.appendLine(`[Log - ${(new Date().toLocaleTimeString())}] ${message}`);
            if (data) {
                this.appendLine(Logger.data2String(data));
            }
        }
    }
    updateConfiguration() {
        this.trace = this.readTrace();
    }
    appendLine(value = '') {
        return this.outputChannel.value.appendLine(value);
    }
    append(value) {
        return this.outputChannel.value.append(value);
    }
    show() {
        this.outputChannel.value.show();
    }
    readTrace() {
        return Trace.fromString(vscode.workspace.getConfiguration().get('restructuredtext.trace', 'off'));
    }
    static data2String(data) {
        if (data instanceof Error) {
            if (isString(data.stack)) {
                return data.stack;
            }
            return data.message;
        }
        if (isString(data)) {
            return data;
        }
        return JSON.stringify(data, undefined, 2);
    }
}
exports.Logger = Logger;
//# sourceMappingURL=logger.js.map