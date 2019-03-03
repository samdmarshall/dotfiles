'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const lintingProvider_1 = require("./utils/lintingProvider");
const configuration_1 = require("./utils/configuration");
class RstLintingProvider {
    constructor(logger) {
        this.logger = logger;
        this.languageId = 'restructuredtext';
    }
    activate(subscriptions) {
        let provider = new lintingProvider_1.LintingProvider(this, this.logger);
        provider.activate(subscriptions);
    }
    loadConfiguration(resource) {
        var module = [];
        var build = configuration_1.Configuration.getLinterPath(resource);
        if (build == null) {
            var python = configuration_1.Configuration.getPythonPath(resource);
            if (python != null) {
                build = python;
                module = module.concat(["-m", "doc8.main"]);
            }
        }
        if (build == null) {
            build = "doc8";
        }
        return {
            executable: build,
            module: module,
            fileArgs: [],
            bufferArgs: [],
            extraArgs: configuration_1.Configuration.getExtraArgs(resource).map((value, index) => { return configuration_1.Configuration.expandMacro(value, resource); }),
            runTrigger: configuration_1.Configuration.getRunType(resource),
            rootPath: configuration_1.Configuration.GetRootPath(resource)
        };
    }
    process(lines) {
        let diagnostics = [];
        lines.forEach(function (line) {
            if (line.includes("No module named")) {
                diagnostics.push({
                    range: new vscode_1.Range(0, 0, 0, Number.MAX_VALUE),
                    severity: vscode_1.DiagnosticSeverity.Warning,
                    message: line,
                    code: null,
                    source: 'restructuredtext'
                });
                return;
            }
            const regex = /(.+?):([0-9]+):\s(.+)/;
            const matches = regex.exec(line);
            if (matches === null) {
                return;
            }
            if (matches[1].endsWith(".py")) {
                // doc8 internal issues.
                return;
            }
            let lineNumber = parseInt(matches[2]) - 1;
            diagnostics.push({
                range: new vscode_1.Range(lineNumber, 0, lineNumber, Number.MAX_VALUE),
                severity: vscode_1.DiagnosticSeverity.Warning,
                message: matches[3],
                code: null,
                source: 'restructuredtext'
            });
        });
        return diagnostics;
    }
}
exports.default = RstLintingProvider;
//# sourceMappingURL=rstLinter.js.map