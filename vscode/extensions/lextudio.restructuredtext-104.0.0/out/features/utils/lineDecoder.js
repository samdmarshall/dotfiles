"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const string_decoder_1 = require("string_decoder");
class LineDecoder {
    constructor(encoding = 'utf8') {
        this.stringDecoder = new string_decoder_1.StringDecoder(encoding);
        this.remaining = null;
        this.lines = [];
    }
    write(buffer) {
        var result = [];
        var value = this.remaining
            ? this.remaining + this.stringDecoder.write(buffer)
            : this.stringDecoder.write(buffer);
        if (value.length < 1) {
            this.lines = this.lines.concat(value);
            return result;
        }
        var start = 0;
        var ch;
        while (start < value.length && ((ch = value.charCodeAt(start)) === 13 || ch === 10)) {
            start++;
        }
        var idx = start;
        while (idx < value.length) {
            ch = value.charCodeAt(idx);
            if (ch === 13 || ch === 10) {
                result.push(value.substring(start, idx));
                idx++;
                while (idx < value.length && ((ch = value.charCodeAt(idx)) === 13 || ch === 10)) {
                    idx++;
                }
                start = idx;
            }
            else {
                idx++;
            }
        }
        this.remaining = start < value.length ? value.substr(start) : null;
        this.lines = this.lines.concat(result);
        return result;
    }
    end() {
        if (this.remaining && this.remaining.length > 0) {
            this.lines = this.lines.concat(this.remaining);
        }
        return this.remaining;
    }
    getLines() {
        return this.lines;
    }
}
exports.LineDecoder = LineDecoder;
//# sourceMappingURL=lineDecoder.js.map