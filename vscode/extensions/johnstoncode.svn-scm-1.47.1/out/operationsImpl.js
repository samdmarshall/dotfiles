"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
class OperationsImpl {
    constructor() {
        this.operations = new Map();
    }
    start(operation) {
        this.operations.set(operation, (this.operations.get(operation) || 0) + 1);
    }
    end(operation) {
        const count = (this.operations.get(operation) || 0) - 1;
        if (count <= 0) {
            this.operations.delete(operation);
        }
        else {
            this.operations.set(operation, count);
        }
    }
    isRunning(operation) {
        return this.operations.has(operation);
    }
    isIdle() {
        const operations = this.operations.keys();
        for (const operation of operations) {
            if (!util_1.isReadOnly(operation)) {
                return false;
            }
        }
        return true;
    }
}
exports.default = OperationsImpl;
//# sourceMappingURL=operationsImpl.js.map