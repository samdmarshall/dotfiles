"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
const types_1 = require("./common/types");
const decorators_1 = require("./decorators");
const configuration_1 = require("./helpers/configuration");
const util_1 = require("./util");
const iconsRootPath = path.join(__dirname, "..", "icons");
function getIconUri(iconName, theme) {
    return vscode_1.Uri.file(path.join(iconsRootPath, theme, `${iconName}.svg`));
}
class Resource {
    constructor(_resourceUri, _type, _renameResourceUri, _props, _remote = false) {
        this._resourceUri = _resourceUri;
        this._type = _type;
        this._renameResourceUri = _renameResourceUri;
        this._props = _props;
        this._remote = _remote;
    }
    get resourceUri() {
        return this._resourceUri;
    }
    get type() {
        return this._type;
    }
    get renameResourceUri() {
        return this._renameResourceUri;
    }
    get props() {
        return this._props;
    }
    get remote() {
        return this._remote;
    }
    get decorations() {
        // TODO@joh, still requires restart/redraw in the SCM viewlet
        const decorations = util_1.hasSupportToDecorationProvider() &&
            configuration_1.configuration.get("decorations.enabled");
        const light = !decorations
            ? { iconPath: this.getIconPath("light") }
            : undefined;
        const dark = !decorations
            ? { iconPath: this.getIconPath("dark") }
            : undefined;
        const tooltip = this.tooltip;
        const strikeThrough = this.strikeThrough;
        const faded = this.faded;
        const letter = this.letter;
        const color = this.color;
        return {
            strikeThrough,
            faded,
            tooltip,
            light,
            dark,
            letter,
            color,
            source: "svn.resource"
        };
    }
    get command() {
        const diffHead = configuration_1.configuration.get("diff.withHead", true);
        const changesLeftClick = configuration_1.configuration.get("sourceControl.changesLeftClick", "open diff");
        if (!this.remote && changesLeftClick === "open") {
            return {
                command: "svn.openFile",
                title: "Open file",
                arguments: [this]
            };
        }
        if (this.remote || diffHead) {
            return {
                command: "svn.openResourceHead",
                title: "Open Diff With Head",
                arguments: [this]
            };
        }
        return {
            command: "svn.openResourceBase",
            title: "Open Diff With Base",
            arguments: [this]
        };
    }
    getIconPath(theme) {
        if (this.type === types_1.Status.ADDED && this.renameResourceUri) {
            return Resource.icons[theme].Renamed;
        }
        const type = this.type.charAt(0).toUpperCase() + this.type.slice(1);
        if (typeof Resource.icons[theme][type] !== "undefined") {
            return Resource.icons[theme][type];
        }
        return void 0;
    }
    get tooltip() {
        if (this.type === types_1.Status.ADDED && this.renameResourceUri) {
            return "Renamed from " + this.renameResourceUri.fsPath;
        }
        if (this.type === types_1.Status.NORMAL &&
            this.props &&
            this.props !== types_1.PropStatus.NONE) {
            return ("Property " + this.props.charAt(0).toUpperCase() + this.props.slice(1));
        }
        return this.type.charAt(0).toUpperCase() + this.type.slice(1);
    }
    get strikeThrough() {
        if (this.type === types_1.Status.DELETED) {
            return true;
        }
        return false;
    }
    get faded() {
        return false;
    }
    get letter() {
        switch (this.type) {
            case types_1.Status.ADDED:
                if (this.renameResourceUri) {
                    return "R";
                }
                return "A";
            case types_1.Status.CONFLICTED:
                return "C";
            case types_1.Status.DELETED:
                return "D";
            case types_1.Status.EXTERNAL:
                return "E";
            case types_1.Status.IGNORED:
                return "I";
            case types_1.Status.MODIFIED:
                return "M";
            case types_1.Status.REPLACED:
                return "R";
            case types_1.Status.UNVERSIONED:
                return "U";
            case types_1.Status.MISSING:
                return "!";
            default:
                return undefined;
        }
    }
    get color() {
        switch (this.type) {
            case types_1.Status.MODIFIED:
            case types_1.Status.REPLACED:
                return new vscode_1.ThemeColor("gitDecoration.modifiedResourceForeground");
            case types_1.Status.DELETED:
            case types_1.Status.MISSING:
                return new vscode_1.ThemeColor("gitDecoration.deletedResourceForeground");
            case types_1.Status.ADDED:
            case types_1.Status.UNVERSIONED:
                return new vscode_1.ThemeColor("gitDecoration.untrackedResourceForeground");
            case types_1.Status.EXTERNAL:
            case types_1.Status.IGNORED:
                return new vscode_1.ThemeColor("gitDecoration.ignoredResourceForeground");
            case types_1.Status.CONFLICTED:
                return new vscode_1.ThemeColor("gitDecoration.conflictingResourceForeground");
            default:
                return undefined;
        }
    }
    get priority() {
        switch (this.type) {
            case types_1.Status.MODIFIED:
                return 2;
            case types_1.Status.IGNORED:
                return 3;
            case types_1.Status.DELETED:
            case types_1.Status.ADDED:
            case types_1.Status.REPLACED:
            case types_1.Status.MISSING:
                return 4;
            default:
                return 1;
        }
    }
    get resourceDecoration() {
        const title = this.tooltip;
        const abbreviation = this.letter;
        const color = this.color;
        const priority = this.priority;
        const decoration = {
            bubble: true,
            source: "svn.resource",
            title,
            abbreviation,
            color,
            priority
        };
        /**
         * @note Set letter in explorer for VSCode >= 1.27
         * In VSCode 1.27 has renamed the abbreviation to letter
         */
        decoration.letter = abbreviation;
        return decoration;
    }
}
Resource.icons = {
    light: {
        Added: getIconUri("status-added", "light"),
        Conflicted: getIconUri("status-conflicted", "light"),
        Deleted: getIconUri("status-deleted", "light"),
        Ignored: getIconUri("status-ignored", "light"),
        Missing: getIconUri("status-missing", "light"),
        Modified: getIconUri("status-modified", "light"),
        Renamed: getIconUri("status-renamed", "light"),
        Replaced: getIconUri("status-replaced", "light"),
        Unversioned: getIconUri("status-unversioned", "light")
    },
    dark: {
        Added: getIconUri("status-added", "dark"),
        Conflicted: getIconUri("status-conflicted", "dark"),
        Deleted: getIconUri("status-deleted", "dark"),
        Ignored: getIconUri("status-ignored", "dark"),
        Missing: getIconUri("status-missing", "dark"),
        Modified: getIconUri("status-modified", "dark"),
        Renamed: getIconUri("status-renamed", "dark"),
        Replaced: getIconUri("status-replaced", "dark"),
        Unversioned: getIconUri("status-unversioned", "dark")
    }
};
__decorate([
    decorators_1.memoize
], Resource.prototype, "resourceUri", null);
__decorate([
    decorators_1.memoize
], Resource.prototype, "type", null);
__decorate([
    decorators_1.memoize
], Resource.prototype, "command", null);
exports.Resource = Resource;
//# sourceMappingURL=resource.js.map