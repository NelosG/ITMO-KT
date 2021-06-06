package info.kgeorgiy.java.advanced.student;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-student">Student</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class StudentQueryTest extends BaseTest implements StudentQuery {
    protected static final Random RANDOM = new Random(5708430435870328541L);
    private static final List<String> FIRST_NAMES = List.of("Руслан", "Станислав", "Ксения", "Илья", "Даниил", "Мелисса", "Максим", "Виктор", "Никита", "Николай", "Дмитрий", "Александра", "Данил", "Павел", "Мария", "Александр", "Артем", "Айша", "Фарит", "Антон", "Элина", "Владислав", "Петр", "Михаил", "Сергей", "Егор", "Роман", "Екатерина", "Владимир", "Глеб", "Валерия", "Ирина", "Наталья", "Кирилл", "Карен", "Аркадий", "Тимур", "Андрей", "Матвей", "Милена", "Артём", "Данила", "Вероника", "Марсель", "Алексей", "Евгений", "Ярослав", "Даниэль", "Артемий", "Маргарита", "Алина", "Леонид", "Фёдор", "Эмиль", "Салават", "Федор", "Татьяна", "Илона", "Денис", "Ахмад", "Савва", "Иван", "Семен", "Анастасия");
    private static final List<String> LAST_NAMES = List.of("Алиев", "Баталенков", "Батурина", "Береговенко", "Вирцев", "Ермишина", "Жеромский", "Забровский", "Истратов", "Ковальский", "Коробейников", "Коробков", "Костенкова", "Лиханов", "Лыскин", "Пак", "Перцев", "Проценко", "Скрипченко", "Скроба", "Соколов", "Тананов", "Фадеев", "Щелочков", "Якупова", "Автахов", "Асмирко", "Барышников", "Батырова", "Богданов", "Вольнов", "Герасимов", "Гобузов", "Дзюба", "Зудин", "Зюзько", "Иванов", "Карасева", "Колчин", "Круду", "Крюков", "Мищенко", "Монахов", "Москвичев", "Попов", "Пушкарев", "Рындина", "Софрыгин", "Ткаченко", "Тукалло", "Юльцова", "Андреев", "Антонов", "Арменакян", "Артеменко", "Байрамуков", "Белицкий", "Будущев", "Булкина", "Гаранин", "Гордиенко", "Деминцев", "Дубровин", "Зырянова", "Калиничев", "Кожухаров", "Колосов", "Кулешов", "Курябов", "Мороз", "Осиков", "Павлов", "Полчинский", "Прокопенко", "Раков", "Шахов", "Яценко", "Аникина", "Ахметов", "Васильев", "Винников", "Гальченко", "Гранкин", "Гусаров", "Ефимов", "Загребина", "Илаев", "Кулагин", "Курдюков", "Михайлов", "Ползик", "Сабреков", "Самсикова", "Стрельников", "Сысоев", "Харёв", "Холодов", "Чмыхалов", "Шиманская", "Ярошевский", "Алехин", "Белодедова", "Бородачев", "Бородин", "Вихнин", "Гарипов", "Гусев", "Давыдов", "Ильин", "Кирсанов", "Клиначев", "Козлов", "Лабазов", "Малько", "Мозжевилов", "Мухамеджанов", "Надуткин", "Синяченко", "Ушков", "Холявин", "Черемхина", "Шашуловский", "Шик", "Андриянов", "Боже", "Воркожоков", "Дювенжи", "Ешкин", "Ибрахим", "Казаков", "Косогоров", "Криушенков", "Кузин", "Купчик", "Кучма", "Мартынов", "Морев", "Наумов", "Нестеренко", "Панов", "Сластин", "Степанов", "Тушканова", "Чернацкий");
    private static final List<GroupName> GROUPS = List.of(GroupName.values());

    protected static final List<Student> STUDENTS = RANDOM.ints(300)
            .mapToObj(id -> new Student(id, random(FIRST_NAMES), random(LAST_NAMES), random(GROUPS)))
            .collect(Collectors.toUnmodifiableList());

    private static final List<List<Student>> INPUTS = IntStream.range(0, STUDENTS.size())
            .mapToObj(size -> {
                final List<Student> students = new ArrayList<>(STUDENTS);
                Collections.shuffle(students, RANDOM);
                return List.copyOf(students.subList(0, size));
            })
            .collect(Collectors.toUnmodifiableList());

    private static <T> T random(final List<T> values) {
        return values.get(RANDOM.nextInt(values.size()));
    }

    private final StudentQuery db = createCUT();

    @Test
    public void test01_testGetFirstNames() {
        test(this::getFirstNames, db::getFirstNames);
    }

    @Test
    public void test02_testGetLastNames() {
        test(this::getLastNames, db::getLastNames);
    }

    @Test
    public void test03_testGetGroups() {
        test(this::getGroups, db::getGroups);
    }

    @Test
    public void test04_testGetFullNames() {
        test(this::getFullNames, db::getFullNames);
    }

    @Test
    public void test05_testGetDistinctFirstNames() {
        test(this::getDistinctFirstNames, db::getDistinctFirstNames);
    }

    @Test
    public void test06_testGetMaxStudentFirstName() {
        test(this::getMaxStudentFirstName, db::getMaxStudentFirstName);
    }

    @Test
    public void test07_testSortStudentsById() {
        test(this::sortStudentsById, db::sortStudentsById);
    }

    @Test
    public void test08_testSortStudentsByName() {
        test(this::sortStudentsByName, db::sortStudentsByName);
    }

    @Test
    public void test09_testFindStudentsByFirstName() {
        testBi(this::findStudentsByFirstName, db::findStudentsByFirstName, FIRST_NAMES);
    }

    @Test
    public void test10_testFindStudentsByLastName() {
        testBi(this::findStudentsByLastName, db::findStudentsByLastName, FIRST_NAMES);
    }

    @Test
    public void test11_testFindStudentsByGroup() {
        testBi(this::findStudentsByGroup, db::findStudentsByGroup, GROUPS);
    }

    @Test
    public void test12_findStudentNamesByGroup() {
        testBi(this::findStudentNamesByGroupList, db::findStudentNamesByGroupList, GROUPS);
    }

    public static <R> void test(final Function<List<Student>, R> reference, final Function<List<Student>, R> tested) {
        for (final List<Student> input : INPUTS) {
            Assert.assertEquals("For " + input, reference.apply(input), tested.apply(input));
        }
    }

    protected static <T, U> void testBi(
            final BiFunction<List<Student>, U, T> reference,
            final BiFunction<List<Student>, U, T> tested,
            final List<U> values
    ) {
        for (final U value : values.subList(0, Math.min(values.size(), 10))) {
            System.err.println("\tTesting " + value);
            try {
                test(input -> reference.apply(input, value), input -> tested.apply(input, value));
            } catch (final AssertionError e) {
                throw new AssertionError("Value " + value + ": " + e.getMessage(), e);
            }
        }
    }

    // Reference implementation follows
    // This implementation is intentionally poorly-written and contains a lot of copy-and-paste

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        final List<String> result = new ArrayList<>();
        for (final Student student : students) {
            result.add(student.getFirstName());
        }
        return result;
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        final List<String> result = new ArrayList<>();
        for (final Student student : students) {
            result.add(student.getLastName());
        }
        return result;
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        final List<GroupName> result = new ArrayList<>();
        for (final Student student : students) {
            result.add(student.getGroup());
        }
        return result;
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        final List<String> result = new ArrayList<>();
        for (final Student student : students) {
            result.add(student.getFirstName() + " " + student.getLastName());
        }
        return result;
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return new TreeSet<>(getFirstNames(students));
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        int maxId = Integer.MIN_VALUE;
        String maxName = "";
        for (final Student student : students) {
            if (maxId < student.getId()) {
                maxId = student.getId();
                maxName = student.getFirstName();
            }
        }
        return maxName;
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        final ArrayList<Student> sorted = new ArrayList<>(students);
        Collections.sort(sorted);
        return sorted;
    }

    private static final Comparator<Student> STUDENT_COMPARATOR = (a, b) -> {
        final int last = a.getLastName().compareTo(b.getLastName());
        if (last != 0) {
            return -last;
        }
        final int first = a.getFirstName().compareTo(b.getFirstName());
        if (first != 0) {
            return -first;
        }
        return Integer.compare(a.getId(), b.getId());
    };

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        final ArrayList<Student> sorted = new ArrayList<>(students);
        sorted.sort(STUDENT_COMPARATOR);
        return sorted;
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, final String firstName) {
        final ArrayList<Student> result = new ArrayList<>(students);
        result.removeIf(student -> !student.getFirstName().equals(firstName));
        result.sort(STUDENT_COMPARATOR);
        return result;
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, final String lastName) {
        final ArrayList<Student> result = new ArrayList<>(students);
        result.removeIf(student -> !student.getLastName().equals(lastName));
        result.sort(STUDENT_COMPARATOR);
        return result;
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
        final ArrayList<Student> result = new ArrayList<>(students);
        result.removeIf(student -> !student.getGroup().equals(group));
        result.sort(STUDENT_COMPARATOR);
        return result;
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
        final Map<String, String> result = new HashMap<>();
        for (final Student student : findStudentsByGroup(students, group)) {
            result.merge(student.getLastName(), student.getFirstName(), BinaryOperator.minBy(Comparable::compareTo));
        }
        return result;
    }
}
