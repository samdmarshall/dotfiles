"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const child_process_1 = require("child_process");
const configuration_1 = require("./features/utils/configuration");
const common_1 = require("./common");
class Python {
    constructor(logger) {
        this.logger = logger;
        this.version = null;
        this.ready = false;
    }
    isReady() {
        return this.ready;
    }
    async setup(resource) {
        this.pythonPath = `"${configuration_1.Configuration.getPythonPath(resource)}"`;
        await this.getVersion();
        if (configuration_1.Configuration.getConfPath(resource) === '') {
            if (!(await this.checkDocutilsInstall())) {
                var choice = await vscode.window.showInformationMessage("Preview engine docutil is not installed.", "Install", "No now");
                if (choice === "Install") {
                    this.logger.log("Started to install docutils...");
                    await this.installDocUtils();
                }
            }
        }
        else {
            const sphinx = configuration_1.Configuration.getSphinxPath(resource);
            if (!(await this.checkSphinxInstall() || (sphinx != null && await common_1.fileExists(sphinx)))) {
                var choice = await vscode.window.showInformationMessage("Preview engine sphinx is not installed.", "Install", "Not now");
                if (choice === "Install") {
                    this.logger.log("Started to install sphinx...");
                    await this.installSphinx();
                }
            }
        }
        const doc8 = configuration_1.Configuration.getLinterPath(resource);
        if (!(await this.checkDoc8Install() || (doc8 != null && await common_1.fileExists(doc8)))) {
            var choice = await vscode.window.showInformationMessage("Linter doc8 is not installed.", "Install", "Not now");
            if (choice === "Install") {
                this.logger.log("Started to install doc8...");
                await this.installDoc8();
            }
        }
        this.ready = true;
    }
    async installDocUtils() {
        try {
            await this.exec("-m", "pip", "install", "docutils");
            this.logger.log("Finished installing docutils");
        }
        catch (e) {
            this.logger.log("Failed to install docutils");
            vscode.window.showErrorMessage("Could not install docutils. Please run `pip install docutils` to use this " +
                "extension, or check your python path.");
        }
    }
    async checkDocutilsInstall() {
        try {
            await this.exec("-c", '"import docutils;"');
            return true;
        }
        catch (e) {
            return false;
        }
    }
    async installDoc8() {
        try {
            await this.exec("-m", "pip", "install", "doc8");
            this.logger.log("Finished installing doc8");
        }
        catch (e) {
            this.logger.log("Failed to install doc8");
            vscode.window.showErrorMessage("Could not install doc8. Please run `pip install doc8` to use this " +
                "extension, or check your python path.");
        }
    }
    async checkDoc8Install() {
        try {
            await this.exec("-c", '"import doc8.main;"');
            return true;
        }
        catch (e) {
            return false;
        }
    }
    async installSphinx() {
        try {
            await this.exec("-m", "pip", "install", "sphinx", "sphinx-autobuild");
            this.logger.log("Finished installing sphinx");
        }
        catch (e) {
            this.logger.log("Failed to install sphinx");
            vscode.window.showErrorMessage("Could not install sphinx. Please run `pip install sphinx sphinx-autobuild` to use this " +
                "extension, or check your python path.");
        }
    }
    async checkSphinxInstall() {
        try {
            await this.exec("-c", '"import sphinx;"');
            return true;
        }
        catch (e) {
            return false;
        }
    }
    async getVersion() {
        if (this.version !== null) {
            return;
        }
        const version = await this.exec("-c", '"import sys; print(sys.version_info[0])"');
        switch (Number.parseInt(version)) {
            case 2:
                this.version = 2;
                return;
            case 3:
                this.version = 3;
                return;
            default:
                throw new Error("Could not get python version");
        }
    }
    exec(...args) {
        const cmd = [this.pythonPath, ...args];
        return new Promise((resolve, reject) => {
            this.logger.log(`Running cmd: ${this.pythonPath} ${args.join(" ")}`);
            child_process_1.exec(cmd.join(" "), (error, stdout, stderr) => {
                if (error) {
                    let errorMessage = [
                        error.name,
                        error.message,
                        error.stack,
                        "",
                        stderr.toString()
                    ].join("\n");
                    this.logger.log(errorMessage);
                    reject(errorMessage);
                }
                else {
                    this.logger.log("Successful exec", stdout.toString());
                    resolve(stdout.toString());
                }
            });
        });
    }
}
exports.Python = Python;
//# sourceMappingURL=python.js.map