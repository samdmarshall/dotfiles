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
/**
 * Load local first, after load the from VSCode modules
 * == 0 - is svn-scm/out/node_modules
 * == 1 - is svn-scm/node_modules
 * == 2 - is vscode folder
 * >= 3 - parent folders of svn-scm
 */
const vscode = require("vscode");
module.paths.splice(2, 0, `${vscode.env.appRoot}/node_modules.asar`);
module.paths.splice(2, 0, `${vscode.env.appRoot}/node_modules`); // VSCode < 1.21.0
const cp = require("child_process");
const events_1 = require("events");
const iconv = require("iconv-lite");
const isUtf8 = require("is-utf8");
const jschardet = require("jschardet");
const proc = require("process");
const types_1 = require("./common/types");
const configuration_1 = require("./helpers/configuration");
const infoParser_1 = require("./infoParser");
const svnError_1 = require("./svnError");
const svnRepository_1 = require("./svnRepository");
const util_1 = require("./util");
exports.svnErrorCodes = {
    AuthorizationFailed: "E170001",
    RepositoryIsLocked: "E155004",
    NotASvnRepository: "E155007",
    NotShareCommonAncestry: "E195012",
    WorkingCopyIsTooOld: "E155036"
};
function getSvnErrorCode(stderr) {
    for (const name in exports.svnErrorCodes) {
        if (exports.svnErrorCodes.hasOwnProperty(name)) {
            const code = exports.svnErrorCodes[name];
            const regex = new RegExp(`svn: ${code}`);
            if (regex.test(stderr)) {
                return code;
            }
        }
    }
    if (/No more credentials or we tried too many times/.test(stderr)) {
        return exports.svnErrorCodes.AuthorizationFailed;
    }
    return void 0;
}
function cpErrorHandler(cb) {
    return err => {
        if (/ENOENT/.test(err.message)) {
            err = new svnError_1.default({
                error: err,
                message: "Failed to execute svn (ENOENT)",
                svnErrorCode: "NotASvnRepository"
            });
        }
        cb(err);
    };
}
exports.cpErrorHandler = cpErrorHandler;
class Svn {
    constructor(options) {
        this.lastCwd = "";
        this._onOutput = new events_1.EventEmitter();
        this.svnPath = options.svnPath;
        this.version = options.version;
    }
    get onOutput() {
        return this._onOutput;
    }
    logOutput(output) {
        this._onOutput.emit("log", output);
    }
    exec(cwd, args, options = {}) {
        return __awaiter(this, void 0, void 0, function* () {
            if (cwd) {
                this.lastCwd = cwd;
                options.cwd = cwd;
            }
            if (options.log !== false) {
                const argsOut = args.map(arg => (/ |^$/.test(arg) ? `'${arg}'` : arg));
                this.logOutput(`[${this.lastCwd.split(/[\\\/]+/).pop()}]$ svn ${argsOut.join(" ")}\n`);
            }
            if (options.username) {
                args.push("--username", options.username);
            }
            if (options.password) {
                args.push("--password", options.password);
            }
            // Force non interactive environment
            args.push("--non-interactive");
            let encoding = options.encoding || "utf8";
            delete options.encoding;
            const defaults = {
                env: proc.env
            };
            if (cwd) {
                defaults.cwd = cwd;
            }
            const process = cp.spawn(this.svnPath, args, defaults);
            const disposables = [];
            const once = (ee, name, fn) => {
                ee.once(name, fn);
                disposables.push(util_1.toDisposable(() => ee.removeListener(name, fn)));
            };
            const on = (ee, name, fn) => {
                ee.on(name, fn);
                disposables.push(util_1.toDisposable(() => ee.removeListener(name, fn)));
            };
            const [exitCode, stdout, stderr] = yield Promise.all([
                new Promise((resolve, reject) => {
                    once(process, "error", reject);
                    once(process, "exit", resolve);
                }),
                new Promise(resolve => {
                    const buffers = [];
                    on(process.stdout, "data", (b) => buffers.push(b));
                    once(process.stdout, "close", () => resolve(Buffer.concat(buffers)));
                }),
                new Promise(resolve => {
                    const buffers = [];
                    on(process.stderr, "data", (b) => buffers.push(b));
                    once(process.stderr, "close", () => resolve(Buffer.concat(buffers).toString()));
                })
            ]);
            util_1.dispose(disposables);
            // SVN with '--xml' always return 'UTF-8', and jschardet detects this encoding: 'TIS-620'
            if (args.includes("--xml")) {
                encoding = "utf8";
            }
            else {
                const defaultEncoding = configuration_1.configuration.get("default.encoding");
                if (defaultEncoding) {
                    if (!iconv.encodingExists(defaultEncoding)) {
                        this.logOutput("svn.default.encoding: Invalid Parameter: '" +
                            defaultEncoding +
                            "'.\n");
                    }
                    else if (!isUtf8(stdout)) {
                        encoding = defaultEncoding;
                    }
                }
                else {
                    jschardet.MacCyrillicModel.mTypicalPositiveRatio += 0.001;
                    const encodingGuess = jschardet.detect(stdout);
                    if (encodingGuess.confidence > 0.8 &&
                        iconv.encodingExists(encodingGuess.encoding)) {
                        encoding = encodingGuess.encoding;
                    }
                }
            }
            const decodedStdout = iconv.decode(stdout, encoding);
            if (options.log !== false && stderr.length > 0) {
                this.logOutput(`${stderr}\n`);
            }
            if (exitCode) {
                return Promise.reject(new svnError_1.default({
                    message: "Failed to execute svn",
                    stdout: decodedStdout,
                    stderr,
                    stderrFormated: stderr.replace(/^svn: E\d+: +/gm, ""),
                    exitCode,
                    svnErrorCode: getSvnErrorCode(stderr),
                    svnCommand: args[0]
                }));
            }
            return { exitCode, stdout: decodedStdout, stderr };
        });
    }
    getRepositoryRoot(path) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                const result = yield this.exec(path, ["info", "--xml"]);
                const info = yield infoParser_1.parseInfoXml(result.stdout);
                if (info && info.wcInfo && info.wcInfo.wcrootAbspath) {
                    return info.wcInfo.wcrootAbspath;
                }
                // SVN 1.6 not has "wcroot-abspath"
                return path;
            }
            catch (error) {
                if (error instanceof svnError_1.default) {
                    throw error;
                }
                console.error(error);
                throw new Error("Unable to find repository root path");
            }
        });
    }
    open(repositoryRoot, workspaceRoot) {
        return __awaiter(this, void 0, void 0, function* () {
            return new svnRepository_1.Repository(this, repositoryRoot, workspaceRoot, types_1.ConstructorPolicy.Async);
        });
    }
}
exports.Svn = Svn;
//# sourceMappingURL=svn.js.map