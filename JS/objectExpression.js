"use strict";

function Unary(exp) {
    this.exp = exp;
}
Unary.prototype.evaluate = function (x, y, z) {
    return this.calc(x, y, z);
}
Unary.prototype.toString = function () {
    return this.exp.toString() + (this.char === undefined ? "" : " " + this.char);
};
Unary.prototype.prefix = function (x, y, z) {
    return(this.char === undefined ? this.exp.prefix() : "(" + this.char + " " + this.exp.prefix() + ")");
};
function Uop(calc, char) {
    function Operation(first) {
        Unary.call(this, first);
    }
    Operation.prototype = Object.create(Unary.prototype);
    Operation.prototype.char = char;
    Operation.prototype.calc = calc;
    return Operation;
}

function Binary(first, second) {
    this.first = first;
    this.second = second;
}

Binary.prototype.evaluate = function (x, y, z) {
    return this.calc(this.first.evaluate(x, y, z), this.second.evaluate(x, y, z));
};
Binary.prototype.toString = function () {
    return this.first.toString() + " " + this.second.toString() + " " + this.char;
};
Binary.prototype.prefix = function () {
    return "(" + this.char + " " + this.first.prefix() + " " + this.second.prefix() + ")";
};


function Bop(calc, char) {
    function Operation(first, second) {
        Binary.call(this, first, second);
    }
    Operation.prototype = Object.create(Binary.prototype);
    Operation.prototype.char = char;
    Operation.prototype.calc = calc;
    return Operation;
}

const variables = ["x", "y", "z"];
const Variable = Uop(function (...values) {return values[variables.indexOf(this.exp)]});
const Const = Uop(function (x, y, z) {return this.exp;});
const Negate = Uop(function (x, y, z) {return this.exp.evaluate(x, y, z) * -1;}, "negate");
Const.prototype.prefix = function () {return this.exp.toString();}
Variable.prototype.prefix = function () {return this.exp.toString();}


const ArcTan = Uop(function (x, y, z) {return Math.atan(this.exp.evaluate(x, y, z));}, "atan");
const Exp = Uop(function (x, y, z) {return Math.exp(this.exp.evaluate(x, y, z));} , "exp");

const Add = Bop((a, b) => (a + b), "+");
const Subtract = Bop((a, b) => (a - b), "-");
const Multiply = Bop((a, b) => (a * b), "*");
const Divide = Bop((a, b) => (a / b), "/");


let quantity = new Map([
    ["negate", 1],
    ["atan", 1],
    ["exp", 1],
    ["+", 2],
    ["-", 2],
    ["/", 2],
    ["*", 2],
]);

let operations = new Map([
        ["+", Add],
        ["-", Subtract],
        ["/", Divide],
        ["*", Multiply],
        ["negate", Negate],
        ["atan", ArcTan],
        ["exp", Exp]

    ]
);

function parsePrefix(string) {
    let pos = 0;

    function parser(balance) {
        let op = "empty";
        let args = [];
        while (pos < string.length) {
            while (string[pos] === " ") pos++;
            switch (string[pos]) {
                case "(": {
                    pos++;
                    args.push(parser(balance + 1));
                    break;
                }
                case ")": {
                    if (balance <= 0) throw new Error("Closing parenthesis");
                    pos++;
                    return wrap(op, ...args);
                }
                default: {
                    let token = "";
                    while (pos < string.length && string[pos] !== " " && string[pos] !== ")" && string[pos] !== "(") token += string[pos++];
                    if (token === "x" || token === "y" || token === "z") args.push(new Variable(token));
                    else {
                        if (Number.isInteger(+token)) args.push(new Const(Number(token)));
                        else if (operations.has(token)) {
                            if (op !== "empty") throw new Error(token + " tries to replace " + op);
                            op = token;
                        } else throw new Error(token + "Unknown character");
                    }
                }
            }
            while (string[pos] === " ") pos++;
        }
        if (balance !== 0) throw new Error("Not-zero balance of brackets");
        if (args.length !== 1 || op !== "empty") {
            throw new Error(args + " : " + op + " - Invalid result");
        }
        return args[0];
    };

    function wrap(op, ...args) {
        if (args.length !== quantity.get(op)) {
            if (quantity.get(op) > 1) throw new Error("Error in the number of arguments in BinOp  in \"" + pos +"\"");
            throw new Error("Error in the number of arguments in UnaryOp  in \"" + pos + "\"");
        }
        return new (operations.get(op))(...args);
    };

    return parser(0);
};




////////////////////////////////////////////////////////////////////////////////////////
function Min3(first, middle, last) {
    this.first = first;
    this.middle = middle;
    this.last = last;
}

Min3.prototype.evaluate = function (x, y, z) {
    return Math.min(this.first.evaluate(x, y, z), this.middle.evaluate(x, y, z), this.last.evaluate(x, y, z));
}
Min3.prototype.toString = function () {
    return this.first.toString() + " " + this.middle.toString() + " " + this.last.toString() + " min3";
}

function Max5(first, second, third, fourth, fifth) {
    this.first = first;
    this.second = second;
    this.third = third;
    this.fourth = fourth;
    this.fifth = fifth;

}

Max5.prototype.evaluate = function (x, y, z) {
    return Math.max(this.first.evaluate(x, y, z), this.second.evaluate(x, y, z), this.third.evaluate(x, y, z),
        this.fourth.evaluate(x, y, z), this.fifth.evaluate(x, y, z));
}
Max5.prototype.toString = function () {
    return this.first.toString() + " " + this.second.toString() + " " + this.third.toString() + " " + this.fourth.toString() + " " + this.fifth.toString() + " max5";
}
