'use strict';

/**
 * Телефонная книга
 */
const phoneBook = new Map()
let requestNumber = 0

/**
 * Вызывайте эту функцию, если есть синтаксическая ошибка в запросе
 * @param {number} lineNumber – номер строки с ошибкой
 * @param {number} charNumber – номер символа, с которого запрос стал ошибочным
 */
function syntaxError(lineNumber, charNumber) {
    throw new Error(`SyntaxError: Unexpected token at ${lineNumber}:${charNumber}`)
}

function createContact(request) {
    if (request[1] === 'контакт') {
        request.splice(0, 2)
        let name = request.join(' ')
        if (!phoneBook.has(name)) {
            phoneBook.set(name, [[], []])
        }
    } else {
        syntaxError(requestNumber, 8)
    }
}

function del(request) {
    switch (request[1]) {
        case 'контакт':
            deleteContact(request)
            break

        case 'почту':
        case 'телефон':
            deletePhoneAndMail(request)
            break

        case 'контакты,':
            deleteContacts(request)
            break

        default:
            syntaxError(requestNumber, 7)
    }
}

function deleteContact(request) {
    request.splice(0, 2)
    phoneBook.delete(request.join(' '))
}

function indexForError(i, request) {
    let index = 0
    for (let j = 0; j < i; j++) {
        index += request[j].length + 1
    }
    index++
    return index
}

function addOrDeletePhoneOrMail(request, isDelete) {
    let i = 1
    let phones = []
    let mails = []
    while (request[i] === 'телефон' || request[i] === 'почту') {
        if (request[i] === 'телефон') {
            if (/^[0-9]{10}$/.test(request[++i])) {
                phones.push(request[i])
            } else {
                syntaxError(requestNumber, indexForError(i, request))
            }
        } else if (request[i] === 'почту') {
            mails.push(request[++i])
        } else {
            syntaxError(requestNumber, indexForError(i, request))
        }
        if (request[++i] === 'и' || request[i] === 'для') {
            i++
        } else {
            syntaxError(requestNumber, indexForError(i, request))
        }
    }

    if (request[i - 1] === 'для') {
        if (request[i] === 'контакта') {
            request.splice(0, ++i)
            let name = request.join(' ')
            if (phoneBook.has(name)) {
                let f = (ind, phonesOrMails) => {
                    for (let j = 0; j < phonesOrMails.length; j++) {
                        if (phoneBook.get(name)[ind].indexOf(phonesOrMails[j]) === -1) {
                            if (!isDelete) {
                                phoneBook.get(name)[ind].push(phonesOrMails[j])
                            }
                        } else {
                            if (isDelete) {
                                phoneBook.get(name)[ind].splice(phoneBook.get(name)[ind].indexOf(phonesOrMails[j]), 1)
                            }
                        }
                    }
                }
                f(0, phones);
                f(1, mails);
            }
        } else {
            syntaxError(requestNumber, indexForError(i, request))
        }
    } else {
        syntaxError(requestNumber, indexForError(i, request))
    }
}

function addPhoneAndMail(request) {
    addOrDeletePhoneOrMail(request, false);
}

function deletePhoneAndMail(request) {
    addOrDeletePhoneOrMail(request, true);
}

function deleteContacts(request) {
    if (request[2] === 'где') {
        if (request[3] === 'есть') {
            request.splice(0, 4)
            let delRequest = request.join(' ')
            if (delRequest === '' || delRequest === ' ') {
                return
            }
            for (let entry of phoneBook) {
                let element = entry[0]
                let phones = entry[1][0]
                let mails = entry[1][1]
                if (element.includes(delRequest)) {
                    phoneBook.delete(element)
                } else {
                    let f = (phonesOrMails) => {
                        for (let j = 0; j < phonesOrMails.length; j++) {
                            if (phonesOrMails[j].includes(delRequest)) {
                                phoneBook.delete(element)
                                break
                            }
                        }
                    }
                    if (phones !== []) {
                        f(phones)
                    }
                    if (mails !== []) {
                        f(mails)
                    }
                }
            }

        } else {
            syntaxError(requestNumber, 21)
        }
    } else {
        syntaxError(requestNumber, 17)
    }
}


function show(request) {
    let requests = []
    let i = 1
    while (request[i] === 'имя' || request[i] === 'почты' || request[i] === 'телефоны') {

        switch (request[i]) {
            case 'телефоны':
            case 'почты':
            case 'имя':
                requests.push(request[i])
                if (request[++i] === 'и' || request[i] === 'для') {
                    i++
                } else {
                    syntaxError(requestNumber, indexForError(i, request))
                }
                break

        }
    }

    let answer = []
    if (request[i - 1] === 'для'){
        if(request[i++] === 'контактов,' && request[i++] === 'где' && request[i++] === 'есть') {
            let part = request[i]
            for (let entry of phoneBook) {
                let name = entry[0]
                if (name === '') {
                    return
                }
                let phones = entry[1][0]
                let mails = entry[1][1]
                let flag = false
                let f = (phonesOrMails) => {
                    for (let j = 0; j < phonesOrMails.length; j++) {
                        if (phonesOrMails[j].includes(part)) {
                            flag = true
                        }
                    }
                }
                f(mails)
                f(phones)
                if (name.includes(part)) {
                    flag = true
                }
                if (flag) {
                    let stringAnswer = ''
                    for (let j = 0; j < requests.length; j++) {
                        if (requests[j] === 'имя') {
                            stringAnswer += name + ';'
                        } else if (requests[j] === 'телефоны') {
                            if (phones.length === 0) {
                                stringAnswer += ';'
                            } else {
                                for (let q = 0; q < phones.length - 1; q++) {
                                    let phone = `+7 (${phones[q].slice(0, 3)}) ${phones[q].slice(3, 6)}-${phones[q].slice(6, 8)}-${phones[q].slice(8, 10)}`
                                    stringAnswer += phone + ','
                                }
                                let phone = `+7 (${phones[phones.length - 1].slice(0, 3)}) ${phones[phones.length - 1].slice(3, 6)}-${phones[phones.length - 1].slice(6, 8)}-${phones[phones.length - 1].slice(8, 10)}`
                                stringAnswer += phone + ';'
                            }
                        } else if (requests[j] === 'почты') {
                            if (mails.length === 0) {
                                stringAnswer += ';'
                            } else {
                                for (let q = 0; q < mails.length - 1; q++) {
                                    stringAnswer += mails[q] + ','
                                }
                                stringAnswer += mails[mails.length - 1] + ';'
                            }
                        } else {
                            syntaxError(requestNumber, indexForError(j, request))
                        }
                    }
                    if (stringAnswer[stringAnswer.length - 1] === ';') {
                        stringAnswer = stringAnswer.slice(0, -1)
                    }
                    answer.push(stringAnswer)
                }
            }
        } else {
            syntaxError(requestNumber, indexForError(i - 1, request))
        }
    } else {
        syntaxError(requestNumber, indexForError(i, request))
    }
    return answer
}

/**
 * Выполнение запроса на языке pbQL
 * @param {string} query
 * @returns {string[]} - строки с результатами запроса
 */
function run(query) {
    let ans = []
    let queryArr = query.split(';')
    if (queryArr[queryArr.length - 1] === '') {
        queryArr.splice(queryArr.length - 1, 1)
    } else {
        syntaxError(queryArr.length, queryArr[queryArr.length - 1].length + 1)
    }

    queryArr.forEach((request) => {
        requestNumber++
        request = request.split(' ')

        switch (request[0]) {
            case 'Создай':
                createContact(request)
                break

            case 'Удали' :
                del(request)
                break

            case 'Добавь':
                addPhoneAndMail(request)
                break

            case 'Покажи':
                let answer = show(request)
                if (answer !== undefined) {
                    answer.forEach((element) => {
                        ans.push(element)
                    })
                }
                break

            default:
                syntaxError(requestNumber, 1)
        }
    })
    return ans;
}

// console.log('Покажи имя и телефоны и почты для контактов, где есть ий'.length)
// console.log(
//     run(
//         'Создай контакт Григорий;' +
//         'Создай контакт Василий;' +
//         'Добавь телефон 5556667787 для контакта Василий;' +
//         'Добавь телефон 5556667787 для контакта Григорий;' +
//         'Добавь телефон 5556667788 и почту grisha@example.com для контакта Григорий;' +
//         'Покажи имя и телефоны и почты для контактов, где есть ий;' +
//         'Удали телефон 5556667788 и почту grisha@example.com для контакта Григорий;' +
//         'Покажи имя и телефоны и почты для контактов, где есть ий;' +
//         'Удали контак Григорий;' +
//         'Покажи имя и телефоны и почты для контактов, где есть ий'
//     )
// )

module.exports = {phoneBook, run};