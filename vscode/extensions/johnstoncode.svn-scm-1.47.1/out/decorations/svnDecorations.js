"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const configuration_1 = require("../helpers/configuration");
const svnDecorationProvider_1 = require("./svnDecorationProvider");
const svnIgnoreDecorationProvider_1 = require("./svnIgnoreDecorationProvider");
class SvnDecorations {
    constructor(model) {
        this.model = model;
        this.enabled = false;
        this.modelListener = [];
        this.providers = new Map();
        this.configListener = vscode_1.workspace.onDidChangeConfiguration(() => this.update());
        this.update();
    }
    update() {
        const enabled = configuration_1.configuration.get("decorations.enabled");
        if (this.enabled === enabled) {
            return;
        }
        if (enabled) {
            this.enable();
        }
        else {
            this.disable();
        }
        this.enabled = enabled;
    }
    enable() {
        this.modelListener = [];
        this.model.onDidOpenRepository(this.onDidOpenRepository, this, this.modelListener);
        this.model.onDidCloseRepository(this.onDidCloseRepository, this, this.modelListener);
        this.model.repositories.forEach(this.onDidOpenRepository, this);
    }
    disable() {
        this.modelListener.forEach(d => d.dispose());
        this.providers.forEach(value => value.dispose());
        this.providers.clear();
    }
    onDidOpenRepository(repository) {
        const provider = new svnDecorationProvider_1.default(repository);
        const ignoreProvider = new svnIgnoreDecorationProvider_1.default(repository);
        this.providers.set(repository, vscode_1.Disposable.from(provider, ignoreProvider));
    }
    onDidCloseRepository(repository) {
        const provider = this.providers.get(repository);
        if (provider) {
            provider.dispose();
            this.providers.delete(repository);
        }
    }
    dispose() {
        this.configListener.dispose();
        this.disable();
    }
}
exports.default = SvnDecorations;
//# sourceMappingURL=svnDecorations.js.map