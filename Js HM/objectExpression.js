"use strict";

function Unary(exp) {
	this.exp = exp;
}
Unary.prototype.toString = function(x, y, z) {
	return this.exp.toString();
	// return this.exp.toString() + (this.char === undefined ? "" : " " + this.char.toString()); можно использовать char если в дальнейшем потребуется добавить множество фун-ий таких как log2 pow2 и тд... (тогда не надо переопределять прототип Negate а всего лишь опр его символ)
};

function Const(x) {
	Unary.call(this, x);
}
Const.prototype = Object.create(Unary.prototype);
Const.prototype.evaluate = function(x, y, z) {
	return this.exp;
};

function Negate(x) {
	Unary.call(this, x);
	// this.char = "negate";
}
Negate.prototype = Object.create(Unary.prototype);
Negate.prototype.toString = function(x, y, z) {
	return this.exp.toString() + " negate";
};
Negate.prototype.evaluate = function(x, y, z) {
	return this.exp.evaluate(x, y, z) * -1;
};

function Variable(name) {
	Unary.call(this, name);
}
Variable.prototype = Object.create(Unary.prototype);
Variable.prototype.evaluate = function(x, y, z) {
	if (this.exp === "x") return x;
	if (this.exp === "y") return y;
	return z;
};

function Binary(first, second) {
	this.first = first;
	this.second = second;
}
Binary.prototype.evaluate = function(x, y, z) {
	return this.calc(this.first.evaluate(x, y, z), this.second.evaluate(x, y, z));
};
Binary.prototype.toString = function() {
	return this.first.toString() + " " + this.second.toString() + " " + this.char;
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
