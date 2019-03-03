"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/** Marker for constructors returning Promise<this> */
var ConstructorPolicy;
(function (ConstructorPolicy) {
    ConstructorPolicy[ConstructorPolicy["Async"] = 0] = "Async";
    ConstructorPolicy[ConstructorPolicy["LateInit"] = 1] = "LateInit";
})(ConstructorPolicy = exports.ConstructorPolicy || (exports.ConstructorPolicy = {}));
var SvnKindType;
(function (SvnKindType) {
    SvnKindType["FILE"] = "file";
    SvnKindType["DIR"] = "dir";
})(SvnKindType = exports.SvnKindType || (exports.SvnKindType = {}));
var RepositoryState;
(function (RepositoryState) {
    RepositoryState[RepositoryState["Idle"] = 0] = "Idle";
    RepositoryState[RepositoryState["Disposed"] = 1] = "Disposed";
})(RepositoryState = exports.RepositoryState || (exports.RepositoryState = {}));
var Operation;
(function (Operation) {
    Operation["Add"] = "Add";
    Operation["AddChangelist"] = "AddChangelist";
    Operation["CleanUp"] = "CleanUp";
    Operation["Commit"] = "Commit";
    Operation["CurrentBranch"] = "CurrentBranch";
    Operation["Info"] = "Info";
    Operation["Ignore"] = "Ignore";
    Operation["Log"] = "Log";
    Operation["NewBranch"] = "NewBranch";
    Operation["Patch"] = "Patch";
    Operation["Remove"] = "Remove";
    Operation["RemoveChangelist"] = "RemoveChangelist";
    Operation["Rename"] = "Rename";
    Operation["Resolve"] = "Resolve";
    Operation["Resolved"] = "Resolved";
    Operation["Revert"] = "Revert";
    Operation["Show"] = "Show";
    Operation["Status"] = "Status";
    Operation["StatusRemote"] = "StatusRemote";
    Operation["SwitchBranch"] = "SwitchBranch";
    Operation["Update"] = "Update";
})(Operation = exports.Operation || (exports.Operation = {}));
var Status;
(function (Status) {
    Status["ADDED"] = "added";
    Status["CONFLICTED"] = "conflicted";
    Status["DELETED"] = "deleted";
    Status["EXTERNAL"] = "external";
    Status["IGNORED"] = "ignored";
    Status["INCOMPLETE"] = "incomplete";
    Status["MERGED"] = "merged";
    Status["MISSING"] = "missing";
    Status["MODIFIED"] = "modified";
    Status["NONE"] = "none";
    Status["NORMAL"] = "normal";
    Status["OBSTRUCTED"] = "obstructed";
    Status["REPLACED"] = "replaced";
    Status["UNVERSIONED"] = "unversioned";
})(Status = exports.Status || (exports.Status = {}));
var PropStatus;
(function (PropStatus) {
    PropStatus["CONFLICTED"] = "conflicted";
    PropStatus["MODIFIED"] = "modified";
    PropStatus["NONE"] = "none";
    PropStatus["NORMAL"] = "normal";
})(PropStatus = exports.PropStatus || (exports.PropStatus = {}));
var SvnUriAction;
(function (SvnUriAction) {
    SvnUriAction["LOG"] = "LOG";
    SvnUriAction["PATCH"] = "PATCH";
    SvnUriAction["SHOW"] = "SHOW";
})(SvnUriAction = exports.SvnUriAction || (exports.SvnUriAction = {}));
//# sourceMappingURL=types.js.map