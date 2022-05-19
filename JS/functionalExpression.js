"use strict"


const cnst = value => () => value;
const pi = () => Math.PI;
const e = () => Math.E;

const variables = ["x", "y", "z"];
const variable = (name) => (...values) => values[variables.indexOf(name)];

const operation = (f) => (...args) => (x, y, z) => f(...args.map(func => func(x, y, z)));
const negate = operation((first) => -first);
const add = operation((first, second) => first + second);
const subtract = operation((first, second) => first - second);
const multiply = operation((first, second) => first * second);
const divide = operation((first, second) => first / second);
const cube = operation((first) => Math.pow(first, 3));
const cuberoot = operation((first) => first < 0 ? -Math.pow(-first, 1 / 3) : Math.pow(first, 1 / 3));

const cos = operation((first) => Math.cos(first));
const sin = operation((first) => Math.sin(first));
