"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function toJS(sexp) {
    switch (sexp.kind) {
        case 'cons':
            return [toJS(sexp.car), toJS(sexp.cdr)];
        case 'list':
            return sexp.elements.map(element => toJS(element));
        case 'number':
            return sexp.n;
        case 'ident':
            return sexp.ident;
        case 'string':
            return sexp.str;
        case 'nil':
            return null;
    }
}
exports.toJS = toJS;
function toString(sexp) {
    switch (sexp.kind) {
        case 'cons':
            return '(' + toString(sexp.car) + ' . ' + toString(sexp.cdr) + ')';
        case 'list':
            let stringRepr = '(';
            sexp.elements.forEach(element => {
                stringRepr += toString(element) + ' ';
            });
            return stringRepr.substr(0, stringRepr.length - 1) + ')';
        case 'number':
            return sexp.n.toString();
        case 'ident':
            return sexp.ident;
        case 'string':
            return JSON.stringify(sexp.str);
        case 'nil':
            return 'nil';
    }
}
exports.toString = toString;
// outputs the SExp directly as a JS object to reduce the processing time
function parseSExp(input) {
    let ptr = 0;
    function parseSymbol() {
        let symbolStart = ptr;
        while (ptr < input.length && !(input[ptr] === ' ' || input[ptr] === ')')) {
            if (input[ptr] === '\\' && input[ptr + 1] === ' ')
                ptr += 2;
            else
                ptr++;
        }
        let sym = input.substring(symbolStart, ptr);
        if (/^-?\d+$/.test(sym))
            // return { kind: "number", n: parseInt(sym) };
            return parseInt(sym);
        // else if (/^-?\d+.\d+$/.test(sym))
        //    return { kind: "number", n: parseFloat(sym) };
        else if (sym === 'nil')
            // return { kind: "nil" };
            return null;
        // else if (/^#x-?[\da-fA-F]+$/.test(sym))
        //    return { kind: "number", n: parseInt(sym.substr(2), 16) };
        // return { kind: "ident", ident: sym };
        return sym;
    }
    function parseString() {
        let hasEscapes = false;
        let startPos = ptr;
        while (ptr < input.length && input[ptr] !== '\"') {
            if (input[ptr] === '\\') {
                if (ptr + 1 >= input.length)
                    throw 'Expected character after a escape seqence introducing backslash';
                // string += input[ptr] + input[ptr + 1];
                ptr += 2;
                hasEscapes = true;
            }
            else
                // string += input[ptr++];
                ptr++;
        }
        // let str = input.substring(startPos, ptr);
        // return { kind: "string", str: hasEscapes ? JSON.parse("\"" + string + "\"") : string };
        return hasEscapes ? JSON.parse(input.substring(startPos - 1, ptr + 1)) : input.substring(startPos, ptr);
    }
    function parseListOrCon(root) {
        // let items: SExp[] = [];
        let items = [];
        let cons = false;
        while (ptr < input.length) {
            if (input[ptr] !== '(' && input[ptr] !== ')' && input[ptr] !== ' ' && input[ptr] !== '"') {
                let sym = parseSymbol();
                // if (sym.kind == "ident" && sym.ident == ".") {
                if (sym === '.') {
                    if (items.length === 1)
                        cons = true;
                    else
                        throw 'Invalid cons cell syntax';
                }
                items.push(sym);
            }
            else if (input[ptr] === '(') {
                ptr++;
                items.push(parseListOrCon());
            }
            else if (input[ptr] === '\"') {
                ptr++;
                items.push(parseString());
            }
            if (input[ptr++] === ')')
                break;
            // ptr++;
        }
        if (input[ptr - 1] !== ')' && !root)
            throw 'Premature end, expected closing bracket';
        if (cons) {
            if (items.length === 3) {
                // return { kind: "cons", car: items[0], cdr: items[2] };
                return [items[0], items[2]];
            }
            else
                throw 'Invalid cons cell syntax';
        }
        // return { kind: "list", elements: items };
        return items;
    }
    try {
        return parseListOrCon(true);
    }
    catch (e) {
        return e;
    }
}
exports.parseSExp = parseSExp;
//# sourceMappingURL=sexp.js.map