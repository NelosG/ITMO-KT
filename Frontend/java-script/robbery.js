'use strict';

const timeREG = new RegExp('^(.{2})\\s(\\d{2}):(\\d{2})\\+(\\d)$');
const dayNames = ['ПН', 'ВТ', 'СР', 'ЧТ', 'ПТ', 'СБ', 'ВС'];

const hoursInDay = 24;
const minutesInHour = 60;
const open = 0;
const close = 1;

let beforeStart;
let afterEnd;

function getMinutes(timeString) {
    const [, dayString, hourString, minuteString, timeZone] = timeREG.exec(timeString);

    return (dayNames.indexOf(dayString) * hoursInDay + parseInt(hourString) -
        parseInt(timeZone)) * minutesInHour + parseInt(minuteString);
}

function getEvents(schedule, events, reverse = false) {
    events = events.slice();
    for (const {from, to} of schedule) {
        events.push({time: getMinutes(from), type: reverse ? close : open});
        events.push({time: getMinutes(to), type: reverse ? open : close});
    }
    return events;
}


function getCommonMomentRanges(events, enoughOpenedForRobbery) {
    // sort
    events = events.slice();

    events.sort((first, second) => {
        const result = first.time - second.time;
        if (result !== 0) {
            return result;
        }

        return first.type - second.type;
    });

    let count = 0;
    const notStarted = beforeStart - 1;
    let rangeStarted = notStarted;
    const ranges = [];

    for (const event of events) {
        count += event.type === open ? 1 : -1;

        if (count >= enoughOpenedForRobbery && rangeStarted === notStarted) {
            rangeStarted = event.time;
        }

        if (count < enoughOpenedForRobbery && rangeStarted !== notStarted) {
            ranges.push({from: rangeStarted, to: event.time});
            rangeStarted = notStarted;
        }
    }

    return ranges;
}


function getRanges(schedule, workingHours, duration) {
    let ranges = [];
    let bankEvents = getEvents(dayNames.slice(0, 3).map(day => ({
        from: `${day} ${workingHours.from}`,
        to: `${day} ${workingHours.to}`
    })), []);

    let f = (robberSchedule) => {
        const events = getEvents(robberSchedule, [{time: beforeStart, type: open}], true);
        events.push({time: afterEnd, type: close});

        return events;
    }

    for (const key of Object.keys(schedule)) {
        bankEvents = bankEvents.concat(f(schedule[key]));
    }

    for (const range of getCommonMomentRanges(bankEvents, Object.keys(schedule).length + 1)) {
        if (range.to - range.from >= duration) {
            ranges.push({from: range.from, to: range.to - duration});
        }
    }
    return ranges;
}

/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+5"
 * @param {string} workingHours.to Время закрытия, например, "18:00+5"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
    const timeZone = parseInt(workingHours.from[workingHours.from.length - 1]);
    beforeStart = getMinutes('ПН 00:00+' + timeZone);
    afterEnd = getMinutes('ВС 23:59+' + timeZone);
    let ranges = getRanges(schedule, workingHours, duration);

    return {
        /**
         * Найдено ли время
         * @returns {boolean}
         */
        exists: function () {
            return ranges.length > 0;
        },

        /**
         * Возвращает отформатированную строку с часами
         * для ограбления во временной зоне банка
         *
         * @param {string} template
         * @returns {string}
         *
         * @example
         * ```js
         * getAppropriateMoment(...).format('Начинаем в %HH:%MM (%DD)') // => Начинаем в 14:59 (СР)
         * ```
         */
        format(template) {
            if (ranges.length === 0) {
                return '';
            }
            const formatNumber = (number) => {
                return number < 10 ? '0' + number : number;
            }
            const timeInBankTimezone = ranges[0].from + timeZone * minutesInHour;
            return template.replace('%DD', dayNames[Math.floor(timeInBankTimezone / minutesInHour / hoursInDay)])
                .replace('%HH', formatNumber(Math.floor(timeInBankTimezone / 60) % 24))
                .replace('%MM', formatNumber(timeInBankTimezone % 60));
        },

        /**
         * Попробовать найти часы для ограбления позже [*]
         * @note Не забудь при реализации выставить флаг `isExtraTaskSolved`
         * @returns {boolean}
         */
        tryLater() {
            if (ranges.length === 0) {
                return false;
            }

            const min = ranges[0].from + 30;

            for (let i = 0; i < ranges.length; ++i) {
                if (ranges[i].to >= min) {
                    ranges = ranges.slice(i);
                    ranges[0].from = Math.max(ranges[0].from, min);

                    return true;
                }
            }

            return false;
        }
    };
}

const isExtraTaskSolved = true;

module.exports = {
    getAppropriateMoment,

    isExtraTaskSolved
};
