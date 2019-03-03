/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const net = require("net");
const nimBuild_1 = require("./nimBuild");
var TestMode;
(function (TestMode) {
    TestMode[TestMode["Load"] = 0] = "Load";
    TestMode[TestMode["Exec"] = 1] = "Exec";
})(TestMode || (TestMode = {}));
class NimTestAdapter {
    constructor(workspace) {
        this.workspace = workspace;
        this.disposables = [];
        this.testsEmitter = new vscode.EventEmitter();
        this.testStatesEmitter = new vscode.EventEmitter();
        this.autorunEmitter = new vscode.EventEmitter();
        this.commandMode = TestMode.Load;
        console.log('Initializing example adapter');
        this.disposables.push(this.testsEmitter);
        this.disposables.push(this.testStatesEmitter);
        this.disposables.push(this.autorunEmitter);
        let self = this;
        var currentSuite;
        this.server = net.createServer(function (socket) {
            socket.on('data', function (data) {
                if (self.commandMode === TestMode.Load) {
                    let str = data.toString();
                    if (str.startsWith('!SUITE!')) {
                        currentSuite = self.makeSuite('1', str.substr('!SUITE!'.length), '');
                        if (self.rootSuite) {
                            self.rootSuite.children.push(currentSuite);
                        }
                    }
                    7;
                }
                console.log('Received: ' + data);
            });
            socket.on('error', function (err) {
                console.log(err);
            });
        });
        this.server.listen(1000, '127.0.0.1');
    }
    get tests() { return this.testsEmitter.event; }
    get testStates() { return this.testStatesEmitter.event; }
    get autorun() { return this.autorunEmitter.event; }
    load() {
        return __awaiter(this, void 0, void 0, function* () {
            console.log('Loading example tests');
            this.testsEmitter.fire({ type: 'started' });
            this.commandMode = TestMode.Load;
            let config = vscode.workspace.getConfiguration('nim');
            this.rootSuite = this.makeSuite('@root@', 'Root', config.get('test-project', ''));
            nimBuild_1.nimExec('@unittest@', 'c', ['-r', config.get('test-project', '')], true, (lines) => { })
                .then(result => {
                console.log('!!!!!!!!!!!!!!');
                this.testsEmitter.fire({ type: 'finished', suite: this.rootSuite });
            });
            // const loadedTests = await loadFakeTests();
            // var loadedTests = this.makeSuite('wqeqwe', 'qweqwe');
            // loadedTests.children.push(this.makeTest('asdad', 'asdasdasd'));
            // var suite2 = this.makeSuite('suite2', 'suite2');
            // loadedTests.children.push(suite2);
            // suite2.children.push(this.makeTest('test1', 'test22'));
            // this.testsEmitter.fire(<TestLoadFinishedEvent>{ type: 'finished', suite: loadedTests });
        });
    }
    run(tests) {
        return __awaiter(this, void 0, void 0, function* () {
            console.log(`Running example tests ${JSON.stringify(tests)}`);
            this.testStatesEmitter.fire({ type: 'started', tests });
            // in a "real" TestAdapter this would start a test run in a child process
            // await runFakeTests(tests, this.testStatesEmitter);
            this.testStatesEmitter.fire({ type: 'finished' });
        });
    }
    debug(tests) {
        return __awaiter(this, void 0, void 0, function* () {
            // in a "real" TestAdapter this would start a test run in a child process and attach the debugger to it
            console.log('debug() not implemented yet');
            throw new Error('Method not implemented.');
        });
    }
    cancel() {
        // in a "real" TestAdapter this would kill the child process for the current test run (if there is any)
        throw new Error('Method not implemented.');
    }
    dispose() {
        this.cancel();
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
        this.disposables = [];
    }
    makeSuite(suite_id, suite_name, file) {
        return {
            type: 'suite',
            id: suite_id,
            label: suite_name,
            file: file,
            children: []
        };
    }
    makeTest(test_id, test_name) {
        return {
            type: 'test',
            id: test_id,
            label: test_name
        };
    }
}
exports.NimTestAdapter = NimTestAdapter;
//# sourceMappingURL=nimTestAdapter.js.map