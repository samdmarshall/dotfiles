'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
/**
 * Configuration for how to transform rst files to html. Either use Sphinx
 * with a gven conf.py file, or use docutils without any configuration
 */
class RstTransformerConfig {
    constructor() {
        this.description = 'Use Sphinx with the selected conf.py path';
    }
}
exports.RstTransformerConfig = RstTransformerConfig;
/**
 * Returns a list of conf.py files in the workspace
 */
async function findConfPyFiles(resource) {
    if (!vscode_1.workspace.workspaceFolders) {
        return [];
    }
    const items = await vscode_1.workspace.findFiles(
    /*include*/ '{**/conf.py}', 
    /*exclude*/ '{}', 
    /*maxResults*/ 100);
    return urisToPaths(items, resource);
}
exports.findConfPyFiles = findConfPyFiles;
function urisToPaths(uris, resource) {
    const paths = [];
    const workspaceFolder = vscode_1.workspace.getWorkspaceFolder(resource);
    uris.forEach((uri) => {
        const folder = vscode_1.workspace.getWorkspaceFolder(uri);
        if (folder === workspaceFolder) {
            paths.push(uri.fsPath);
        }
    });
    return paths;
}
/**
 * Find conf.py files by looking at parent directories. Useful in case
 * a single rst file is opened without a workspace
 */
function findConfPyFilesInParentDirs(rstPath) {
    const paths = [];
    // Walk the directory up from the RST file directory looking for the conf.py file
    let dirName = rstPath;
    while (true) {
        // Get the name of the parent directory
        const parentDir = path.normalize(dirName + '/..');
        // Check if we are at the root directory already to avoid an infinte loop
        if (parentDir === dirName) {
            break;
        }
        // Sanity check - the parent directory must exist
        if (!fs.existsSync(parentDir) || !fs.statSync(parentDir).isDirectory) {
            break;
        }
        // Check this directory for conf.py
        const confPath = path.join(parentDir, 'conf.py');
        if (fs.existsSync(confPath) && fs.statSync(confPath).isFile) {
            paths.push(confPath);
        }
        dirName = parentDir;
    }
    return paths;
}
exports.findConfPyFilesInParentDirs = findConfPyFilesInParentDirs;
//# sourceMappingURL=confPyFinder.js.map