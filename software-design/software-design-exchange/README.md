# Software Design Exchange
- Основные зависимости, а так же работу с commons для работы с БД предоставляет модуль `ru.ifmo.pga.software.design::core`
- Пакеты:
    - Entity - сущности бд
    - Dao - апи обращений к бд
    - Service - сервисы для работы с бд(в отличие от dao имеют некую логику)
    - Web - котроллеры для веба
- Шаблоны страниц находятся в resources/templates

# Запуск
```
mvn package -f pom.xml
mvn docker:run -f pom.xml
```
