"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/*---------------------------------------------------------
 * Copyright (C) Xored Software Inc., RSDuck All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 *--------------------------------------------------------*/
const net = require("net");
const sexp = require("./sexp");
function envelope(content) {
    return ('000000' + content.length.toString(16)).slice(-6) + content;
}
function generateUID() {
    return Math.floor(Math.random() * 10000);
}
class EPCPeer {
    constructor(socket) {
        this.sessions = new Map();
        this.socketClosed = false;
        this.socket = socket;
        this.receivedBuffer = new Buffer(0);
        this.socket.on('data', data => {
            this.receivedBuffer = Buffer.concat([this.receivedBuffer, data]);
            while (this.receivedBuffer.length > 0) {
                if (this.receivedBuffer.length >= 6) {
                    let length = parseInt(this.receivedBuffer.toString('utf8', 0, 6), 16);
                    if (this.receivedBuffer.length >= length + 6) {
                        let content = sexp.parseSExp(this.receivedBuffer.toString('utf8', 6, 6 + length));
                        if (content) {
                            let guid = (content[0][1]);
                            let handle = this.sessions.get(guid);
                            handle(content[0]);
                            this.sessions.delete(guid);
                        }
                        else {
                            this.sessions.forEach(session => {
                                session('Received invalid SExp data');
                            });
                        }
                        this.receivedBuffer = this.receivedBuffer.slice(6 + length);
                    }
                    else
                        return;
                }
            }
        });
        this.socket.on('close', (error) => {
            console.error('Connection closed' + (error ? ' due to an error' : ''));
            this.sessions.forEach(session => {
                session('Connection closed');
            });
            this.socketClosed = true;
        });
    }
    callMethod(method, ...parameter) {
        return new Promise((resolve, reject) => {
            if (this.socketClosed)
                reject('Connection closed');
            let guid = generateUID();
            let payload = '(call ' + guid + ' ' + method + ' ' + sexp.toString({ kind: 'list', elements: parameter }) + ')';
            this.sessions.set(guid, (data) => {
                if (!(data instanceof Array)) {
                    reject(data);
                }
                else {
                    switch (data[0]) {
                        case 'return':
                            resolve(data[2]);
                            break;
                        case 'return-error':
                        case 'epc-error':
                            reject(data[2]);
                            break;
                    }
                }
            });
            this.socket.write(envelope(payload));
        });
    }
    stop() {
        if (!this.socketClosed)
            this.socket.destroy();
    }
}
exports.EPCPeer = EPCPeer;
function startClient(port) {
    return new Promise((resolve, reject) => {
        try {
            let socket = net.createConnection(port, 'localhost', () => {
                resolve(new EPCPeer(socket));
            });
        }
        catch (e) {
            reject(e);
        }
    });
}
exports.startClient = startClient;
//# sourceMappingURL=elrpc.js.map