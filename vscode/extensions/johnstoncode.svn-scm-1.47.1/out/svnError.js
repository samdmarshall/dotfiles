"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class SvnError {
    constructor(data) {
        if (data.error) {
            this.error = data.error;
            this.message = data.error.message;
        }
        else {
            this.error = void 0;
        }
        this.message = data.message || "SVN error";
        this.stdout = data.stdout;
        this.stderr = data.stderr;
        this.stderrFormated = data.stderrFormated;
        this.exitCode = data.exitCode;
        this.svnErrorCode = data.svnErrorCode;
        this.svnCommand = data.svnCommand;
    }
    toString() {
        let result = this.message +
            " " +
            JSON.stringify({
                exitCode: this.exitCode,
                svnErrorCode: this.svnErrorCode,
                svnCommand: this.svnCommand,
                stdout: this.stdout,
                stderr: this.stderr
            }, null, 2);
        if (this.error) {
            result += this.error.stack;
        }
        return result;
    }
}
exports.default = SvnError;
//# sourceMappingURL=svnError.js.map