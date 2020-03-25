"use strict"

let commonBinaryOperation = (first, second) => evalFnc => (...args) =>evalFnc(first(...args), second(...args));
let commonUnaryOperation = (arg) => evalFnc => (...args) => evalFnc(arg(...args));

let cnst = val => (...args) => val;
let variable = name => (...args) => {
    if(name === 'x') return args[0];
    else if (name === 'y') return args[1];
    else return args[2];
}
let add = (first, second) => commonBinaryOperation(first, second)((first, second) => first + second);
let subtract = (first, second) => commonBinaryOperation(first, second)((first, second) => first - second);
let multiply = (first, second) => commonBinaryOperation(first, second)((first, second) => first * second);
let divide = (first, second) => commonBinaryOperation(first, second)((first, second) => first / second);

let negate = (arg) => commonUnaryOperation(arg)((x) => -x);
let cube = (arg) => commonUnaryOperation(arg)((x) => Math.pow(x, 3));
let cuberoot = (arg) => commonUnaryOperation(arg)((x) => x < 0 ? -Math.pow(-x, 1/3) : Math.pow(x, 1/3));
let e = () => Math.E;
let pi = () => Math.PI;
let sin = (arg) => commonUnaryOperation(arg)(Math.sin);
let cos = (arg) => commonUnaryOperation(arg)(Math.cos);
