#!/bin/bash
#Написать пару скрипов генератор оработчик генератор должн уметь принимать 3 вида строк
#строка "TERM" "QUIT" "KILL" -> Передавать сигналы и определить какой сигнал

echo $$ > .pid

term()
{
    echo "Handler: Signal from Generator is 'TERM'"
}
kill()
{
    echo "Handler: Signal from Generator is 'KILL'"
}

quit()
{
    echo "Handler: Signal from Generator is 'QUIT'"
}

trap 'term' SIGTSTP
trap 'quit' USR1
trap 'kill' USR2

while true
do
    echo "I am live"
    sleep 5
done
