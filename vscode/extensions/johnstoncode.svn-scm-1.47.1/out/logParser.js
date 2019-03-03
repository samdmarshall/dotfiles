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
const xml2js = require("xml2js");
const constants_1 = require("./common/constants");
function parseSvnLog(content) {
    return __awaiter(this, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            xml2js.parseString(content, constants_1.xml2jsParseSettings, (err, result) => {
                if (err) {
                    reject();
                }
                let transformed = [];
                if (Array.isArray(result.logentry)) {
                    transformed = result.logentry;
                }
                else if (typeof result.logentry === "object") {
                    transformed = [result.logentry];
                }
                for (const logentry of transformed) {
                    if (logentry.paths === undefined) {
                        logentry.paths = [];
                    }
                    else if (Array.isArray(logentry.paths.path)) {
                        logentry.paths = logentry.paths.path;
                    }
                    else {
                        logentry.paths = [logentry.paths.path];
                    }
                }
                resolve(transformed);
            });
        });
    });
}
exports.parseSvnLog = parseSvnLog;
//# sourceMappingURL=logParser.js.map