'use strict';
/**
 * Итератор по друзьям
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 */
function Iterator(friends, filter) {
    this.maxLevel = Infinity;
    this.index = 0;
    this.invited = this.init(friends, filter);
}

Iterator.prototype.init = function(friends, filter) {
    const friendsComparator = (first, second) => first.name.localeCompare(second.name);
    if (!(filter instanceof Filter)) {
        throw new TypeError();
    }
    let level = this.maxLevel;
    const used = new Set();
    const guests = [];
    let currentLevel = friends.filter(friend => friend.best).sort(friendsComparator);
    while (currentLevel.length > 0 && level > 0) {
        level--;
        currentLevel.forEach(friend => used.add(friend.name));
        guests.push(...currentLevel.filter(filter.test));
        currentLevel = currentLevel
            .reduce((a, b) => a.concat(b.friends), [])
            .filter(name => !used.has(name));
        currentLevel = friends
            .filter(friend => currentLevel.includes(friend.name))
            .sort(friendsComparator);
    }
     return guests;
};

Iterator.prototype.done = function() {
    return this.index >= this.invited.length;
};
Iterator.prototype.next = function() {
    return this.done() ? null : this.invited[this.index++];
};

/**
 * Итератор по друзям с ограничением по кругу
 * @extends Iterator
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 * @param {Number} maxLevel – максимальный круг друзей
 */
function LimitedIterator(friends, filter, maxLevel) {
    this.maxLevel = maxLevel;
    this.index = 0;
    this.invited = this.init(friends, filter);
}
LimitedIterator.prototype = Object.create(Iterator.prototype);
LimitedIterator.prototype.constructor = LimitedIterator;

/**
 * Фильтр друзей
 * @constructor
 */
function Filter() {}
Filter.prototype.constructor = Filter;
Filter.prototype.test = (_friend) => true;

/**
 * Фильтр друзей
 * @extends Filter
 * @constructor
 */
function MaleFilter() {}
MaleFilter.prototype = Object.create(Filter.prototype);
MaleFilter.prototype.constructor = MaleFilter;
MaleFilter.prototype.test = (friend) => friend.gender === 'male';


/**
 * Фильтр друзей-девушек
 * @extends Filter
 * @constructor
 */
function FemaleFilter() {}
FemaleFilter.prototype = Object.create(Filter.prototype);
FemaleFilter.prototype.constructor = FemaleFilter;
FemaleFilter.prototype.test = friend => friend.gender === 'female';


exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;

exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;