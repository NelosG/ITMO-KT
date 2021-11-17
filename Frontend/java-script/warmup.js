'use strict';

function isInteger(n) {
    if (typeof n !== 'number' && !Number.isInteger(n)) {
            throw TypeError('...');
    }
}

function isString(s) {
    if (typeof s !== 'string' && !(s instanceof String)) {
        throw TypeError('...');
    }
}

/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
    isInteger(a);
    isInteger(b);
    return a + b;
}


/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
    isInteger(year)
    if (year <= 0) {
        throw RangeError('...');
    }
    return Math.ceil(year / 100);
}

/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
    isString(hexColor);
    if (hexColor.length > 7 || hexColor[0] !== '#') {
        throw RangeError('...');
    }

    let regRes = /^#([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/gi.exec(hexColor.toUpperCase());
    if(regRes == null) {
        throw RangeError('...');
    }
    let r = parseInt(regRes[1], 16);
    let g = parseInt(regRes[2], 16);
    let b = parseInt(regRes[3], 16);

    return '(' + r + ', ' + g + ', ' + b + ')';
}

/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
    isInteger(n);
    if (n <= 0) {
        throw RangeError('...');
    }
    const result = [0, 1];
    for (let i = 2; i <= n; i++) {
        result.push(result[i - 2] + result[i - 1]);
    }
    return result[n];
}

/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {
    if (!Array.isArray(matrix) || matrix.length === 0 || !matrix.every(Array.isArray) ||
        !matrix.every(line => line.length === matrix[0].length)) {
        throw TypeError('...');
    }
    return matrix[0].map((_, i) => matrix.map(row => row[i]));
}

/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
    isInteger(targetNs);
    if (typeof n !== 'number') {
        throw TypeError('...');
    }
    return n.toString(targetNs);
}

/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
    isString(phoneNumber);
    return /^8-800-\d\d\d-\d\d-\d\d$/.test(phoneNumber);
}


/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
    isString(text);
    return (text.match(/\(-:|:-\)/g) || []).length;
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
    for (let i = 0; i < 3; i++) {
        if (field[0][i] === field[1][i] && field[1][i] === field[2][i]) {
            return field[0][i];
        }
        if (field[i][0] === field[i][1] && field[i][1] === field[i][2]) {
            return field[i][0];
        }
    }
    if (field[0][0] === field[1][1] && field[1][1] === field[2][2] ||
        field[0][2] === field[1][1] && field[1][1] === field[2][0]) {
        return field[1][1];
    }

    return 'draw';
}

module.exports = {
    abProblem,
    centuryByYearProblem,
    colorsProblem,
    fibonacciProblem,
    matrixProblem,
    numberSystemProblem,
    phoneProblem,
    smilesProblem,
    ticTacToeProblem
};
