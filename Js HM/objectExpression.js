"use strict";

function Unary(exp) {
    this.exp = exp;
}

Unary.prototype.toString = function (x, y, z) {
    return this.exp.toString();
};
Unary.prototype.prefix = function (x, y, z) {
    return this.exp.toString();
};

function Const(x) {
    Unary.call(this, x);
}

Const.prototype = Object.create(Unary.prototype);
Const.prototype.evaluate = function (x, y, z) {
    return this.exp;
};

function Negate(x) {
    Unary.call(this, x);
}

Negate.prototype = Object.create(Unary.prototype);
Negate.prototype.toString = function (x, y, z) {
    return this.exp.toString() + " negate";
};
Negate.prototype.prefix = function (x, y, z) {
    return "(negate " + this.exp.prefix() + ")";
};
Negate.prototype.evaluate = function (x, y, z) {
    return this.exp.evaluate(x, y, z) * -1;
};

function ArcTan(x) {
    Unary.call(this, x);
}

ArcTan.prototype = Object.create(Unary.prototype);
ArcTan.prototype.toString = function (x, y, z) {
    return this.exp.toString() + " atan";
};
ArcTan.prototype.prefix = function (x, y, z) {
    return "(atan " + this.exp.prefix() + ")";
};
ArcTan.prototype.evaluate = function (x, y, z) {
    return Math.atan(this.exp.evaluate(x, y, z));
};

function Exp(x) {
    Unary.call(this, x);
}

Exp.prototype = Object.create(Unary.prototype);
Exp.prototype.toString = function (x, y, z) {
    return this.exp.toString() + " exp";
};
Exp.prototype.prefix = function (x, y, z) {
    return "(exp " + this.exp.prefix() + ")";
};
Exp.prototype.evaluate = function (x, y, z) {
    return Math.exp(this.exp.evaluate(x, y, z));
};

function Variable(name) {
    Unary.call(this, name);
}

Variable.prototype = Object.create(Unary.prototype);
Variable.prototype.evaluate = function (x, y, z) {
    if (this.exp === "x") return x;
    if (this.exp === "y") return y;
    return z;
};

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

function Add(first, second) {
    Binary.call(this, first, second);
}

Add.prototype = Object.create(Binary.prototype);
Add.prototype.char = "+";
Add.prototype.calc = (a, b) => (a + b);

function Subtract(first, second) {
    Binary.call(this, first, second);
}

Subtract.prototype = Object.create(Binary.prototype);
Subtract.prototype.char = "-";
Subtract.prototype.calc = (a, b) => (a - b);

function Multiply(first, second) {
    Binary.call(this, first, second);
}

Multiply.prototype = Object.create(Binary.prototype);
Multiply.prototype.char = "*";
Multiply.prototype.calc = (a, b) => (a * b);

function Divide(first, second) {
    Binary.call(this, first, second);
}

Divide.prototype = Object.create(Binary.prototype);
Divide.prototype.char = "/";
Divide.prototype.calc = (a, b) => (a / b);


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
        if (args.length !== quantity.get(op)) throw new Error("Error in the number of arguments");
        return new (operations.get(op))(...args);
    };

    return parser(0);
};


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
