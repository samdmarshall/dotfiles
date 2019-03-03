'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const vscode_1 = require("vscode");
const configuration_1 = require("./configuration");
const confPyFinder_1 = require("./confPyFinder");
/**
 *
 */
class RstTransformerSelector {
    static async findConfDir(resource, logger) {
        const rstPath = resource.fsPath;
        // Sanity check - the file we are previewing must exist
        if (!fs.existsSync(rstPath) || !fs.statSync(rstPath).isFile) {
            return Promise.reject('RST extension got invalid file name: ' + rstPath);
        }
        const configurations = [];
        const pathStrings = [];
        // A path may be configured in the settings. Include this path
        const confPathFromSettings = configuration_1.Configuration.getConfPath(resource);
        const workspaceRoot = configuration_1.Configuration.GetRootPath(resource);
        const docutils = new confPyFinder_1.RstTransformerConfig();
        docutils.label = '$(code) Use docutils';
        docutils.description = 'Do not use Sphinx, but docutils instead';
        docutils.confPyDirectory = '';
        docutils.workspaceRoot = workspaceRoot;
        if (confPathFromSettings != null) {
            if (confPathFromSettings === '') {
                return docutils;
            }
            const pth = path.join(path.normalize(confPathFromSettings), 'conf.py');
            const qpSettings = new confPyFinder_1.RstTransformerConfig();
            qpSettings.label = '$(gear) Sphinx: ' + pth;
            qpSettings.description += ' (from restructuredtext.confPath setting)';
            qpSettings.confPyDirectory = path.dirname(pth);
            qpSettings.workspaceRoot = workspaceRoot;
            return qpSettings;
        }
        // Add path to a directory containing conf.py if it is not already stored
        function addPaths(pathsToAdd) {
            pathsToAdd.forEach((confPath) => {
                const pth = path.normalize(confPath);
                if (pathStrings.indexOf(pth) === -1) {
                    const qp = new confPyFinder_1.RstTransformerConfig();
                    qp.label = '$(gear) Sphinx: ' + pth;
                    qp.confPyDirectory = path.dirname(pth);
                    qp.workspaceRoot = workspaceRoot;
                    configurations.push(qp);
                    pathStrings.push(pth);
                }
            });
        }
        // Search for unique conf.py paths in the workspace and in parent
        // directories (useful when opening a single file, not a workspace)
        const paths1 = await confPyFinder_1.findConfPyFiles(resource);
        const paths2 = confPyFinder_1.findConfPyFilesInParentDirs(rstPath);
        addPaths(paths1);
        addPaths(paths2);
        logger.appendLine('Found conf.py paths: ' + JSON.stringify(pathStrings));
        // The user can choose to use docutils instead of Sphinx
        configurations.push(docutils);
        if (configurations.length === 1) {
            // no conf.py.
            return configurations[0];
        }
        // Found multiple conf.py files, let the user decide
        return vscode_1.window.showQuickPick(configurations, {
            placeHolder: 'Select how to generate html from rst files',
        });
    }
}
exports.RstTransformerSelector = RstTransformerSelector;
//# sourceMappingURL=selector.js.map