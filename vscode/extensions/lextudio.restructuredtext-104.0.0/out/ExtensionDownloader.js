"use strict";
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
//import TelemetryReporter from 'vscode-extension-telemetry';
const util = require("./common");
const packages_1 = require("./packages");
const platform_1 = require("./platform");
/*
 * Class used to download the runtime dependencies of the C# Extension
 */
class ExtensionDownloader {
    constructor(logger, 
    //private reporter: TelemetryReporter /* optional */,
    packageJSON) {
        this.logger = logger;
        this.packageJSON = packageJSON;
    }
    installRuntimeDependencies() {
        this.logger.log('Installing reStructuredText dependencies...');
        this.logger.show();
        let statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right);
        let status = {
            setMessage: text => {
                statusItem.text = text;
                statusItem.show();
            },
            setDetail: text => {
                statusItem.tooltip = text;
                statusItem.show();
            }
        };
        // Sends "AcquisitionStart" telemetry to indicate an acquisition  started.
        //if (this.reporter) {
        //    this.reporter.sendTelemetryEvent("AcquisitionStart");
        //}
        let platformInfo;
        let packageManager;
        let installationStage = 'touchBeginFile';
        let errorMessage = '';
        let success = false;
        let telemetryProps = {};
        return util.touchInstallFile(util.InstallFileType.Begin)
            .then(() => {
            installationStage = 'getPlatformInfo';
            return platform_1.PlatformInformation.GetCurrent();
        })
            .then(info => {
            platformInfo = info;
            packageManager = new packages_1.PackageManager(info, this.packageJSON);
            this.logger.appendLine();
            // Display platform information and RID followed by a blank line
            this.logger.appendLine(`Platform: ${info.toString()}`);
            this.logger.appendLine();
            installationStage = 'downloadPackages';
            const config = vscode.workspace.getConfiguration();
            const proxy = config.get('http.proxy');
            const strictSSL = config.get('http.proxyStrictSSL', true);
            return packageManager.DownloadPackages(this.logger, status, proxy, strictSSL);
        })
            .then(() => {
            this.logger.appendLine();
            installationStage = 'installPackages';
            return packageManager.InstallPackages(this.logger, status);
        })
            .then(() => {
            installationStage = 'touchLockFile';
            return util.touchInstallFile(util.InstallFileType.Lock);
        })
            .then(() => {
            installationStage = 'completeSuccess';
            success = true;
        })
            .catch(error => {
            if (error instanceof packages_1.PackageError) {
                // we can log the message in a PackageError to telemetry as we do not put PII in PackageError messages
                telemetryProps['error.message'] = error.message;
                if (error.innerError) {
                    errorMessage = error.innerError.toString();
                }
                else {
                    errorMessage = error.message;
                }
                if (error.pkg) {
                    telemetryProps['error.packageUrl'] = error.pkg.url;
                }
            }
            else {
                // do not log raw errorMessage in telemetry as it is likely to contain PII.
                errorMessage = error.toString();
            }
            this.logger.appendLine(`Failed at stage: ${installationStage}`);
            this.logger.appendLine(errorMessage);
        })
            .then(() => {
            telemetryProps['installStage'] = installationStage;
            telemetryProps['platform.architecture'] = platformInfo.architecture;
            telemetryProps['platform.platform'] = platformInfo.platform;
            if (platformInfo.distribution) {
                telemetryProps['platform.distribution'] = platformInfo.distribution.toTelemetryString();
            }
            //if (this.reporter) {
            //    this.reporter.sendTelemetryEvent('Acquisition', telemetryProps);
            //}
            this.logger.appendLine();
            installationStage = '';
            this.logger.appendLine('Finished');
            statusItem.dispose();
        })
            .then(() => {
            // We do this step at the end so that we clean up the begin file in the case that we hit above catch block
            // Attach a an empty catch to this so that errors here do not propogate
            return util.deleteInstallFile(util.InstallFileType.Begin).catch((error) => { });
        }).then(() => {
            return success;
        });
    }
}
exports.ExtensionDownloader = ExtensionDownloader;
//# sourceMappingURL=ExtensionDownloader.js.map