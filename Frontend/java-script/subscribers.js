/**
 * Возвращает новый emitter
 * @returns {Object}
 */
function getEmitter() {
    const nameSpace = [];

    const createAction = actionName => {
        let action = nameSpace.find(({name}) => name === actionName);
        if (action) {
            return action;
        }
        action = {name: actionName, actions: []};
        nameSpace.push(action);
        return action;
    };

    return {

        /**
         * Подписаться на событие
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         */
        on: function (event, context, handler) {
            createAction(event).actions.push({context, handler});
            return this;
        },

        /**
         * Отписаться от события
         * @param {String} event
         * @param {Object} context
         */
        off: function (event, context) {
            for (const emit of nameSpace.filter(
                someAction => someAction.name.startsWith(event + '.') || someAction.name === event)
                ) {
                emit.actions = emit.actions.filter(action => action.context !== context);
            }

            return this;
        },

        /**
         * Уведомить о событии
         * @param {String} event
         */
        emit: function (event) {
            const eventParts = event.split('.');

            eventParts.reduce((acc, value, ind) => {
                acc.push(createAction(eventParts.slice(0, ind + 1).join('.')));
                return acc;
            }, [])
                .sort((a, b) => a.name.length - b.name.length)
                .reverse()
                .reduce((acc, value) => acc.concat(value.actions), [])
                .forEach(func => func.handler.call(func.context));
            return this;
        },

        /**
         * Подписаться на событие с ограничением по количеству полученных уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} times – сколько раз получить уведомление
         */
        several: function (event, context, handler, times) {
            this.on(event, context,
                times > 0 ? () => {
                    if (times > 0) {
                        handler.call(context);
                    }
                    times--;
                } : handler
            );

            return this;
        },

        /**
         * Подписаться на событие с ограничением по частоте получения уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} frequency – как часто уведомлять
         */
        through: function (event, context, handler, frequency) {
            let count = 0;
            this.on(event, context,
                frequency > 0 ? () => {
                        if (count === 0) {
                            handler.call(context);
                        }
                        count = (count + 1) % frequency;
                    }
                    : handler
            );

            return this;
        }
    };
}

module.exports = {
    getEmitter
};