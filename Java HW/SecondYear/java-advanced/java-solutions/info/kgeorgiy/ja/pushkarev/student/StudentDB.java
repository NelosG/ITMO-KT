package info.kgeorgiy.ja.pushkarev.student;

import info.kgeorgiy.java.advanced.student.*;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;


public class StudentDB implements AdvancedQuery {

    // Comparators and collectors to avoid having to create them on every function call
    private static final Collector<Student, ?, Map<GroupName, List<Student>>> COLLECT_BY_GROUP =
            Collectors.groupingBy(Student::getGroup);

    private static final Collector<Student, ?, Map<String, String>> COLLECTOR_TO_MAP =
            Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator.minBy(String::compareTo));

    private static final Collector<Student, ?, Map<String, Set<GroupName>>> NAME_AND_SET_OF_GROUP_COLLECTOR =
            Collectors.groupingBy(Student::getFirstName,
                    Collectors.mapping(Student::getGroup, Collectors.toSet()));

    private static final Collector<String, ?, Set<String>> STRING_TO_SET_COLLECTOR =
            Collectors.toCollection(TreeSet::new);

    private static final Collector<String, ?, List<String>> STRING_TO_LIST_COLLECTOR =
            Collectors.toCollection(ArrayList::new);

    private static final Collector<GroupName, ?, List<GroupName>> GROUP_NAME_TO_LIST_COLLECTOR =
            Collectors.toCollection(ArrayList::new);

    private static final Collector<Group, ?, List<Group>> GROUP_TO_UNMODIFIABLE_LIST_COLLECTOR =
            Collectors.toUnmodifiableList();

    private static final Collector<Student, ?, List<Student>> STUDENT_TO_UNMODIFIABLE_LIST_COLLECTOR =
            Collectors.toUnmodifiableList();

    // :NOTE: should be static. and in capitals // Fixed.
    private static final Comparator<Student> STUDENT_COMPARATOR = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparingInt(Student::getId);

    private static final Comparator<Map.Entry<GroupName, List<Student>>> LARGEST_GROUP_COMPARATOR = Comparator
            .comparingInt((Map.Entry<GroupName, List<Student>> entry) -> entry.getValue().size())
            .thenComparing(Map.Entry::getKey, GroupName::compareTo);

    private static final Comparator<Group> GROUP_COMPARATOR = Comparator.comparing(Group::getName);

    private static final Comparator<Map.Entry<String, Set<GroupName>>> MOST_POPULAR_NAME_COMPARATOR = Comparator
            .comparingInt((Map.Entry<String, Set<GroupName>> entry) -> entry.getValue().size())
            .thenComparing(Map.Entry::getKey);

    //Can't be static because of getDistinctFirstNames
    private final Comparator<Map.Entry<GroupName, List<Student>>> LARGEST_GROUP_FIRST_NAME_COMPARATOR = Comparator
            .comparingInt((Map.Entry<GroupName, List<Student>> entry) -> getDistinctFirstNames(entry.getValue()).size())
            .thenComparing(Map.Entry::getKey, Collections.reverseOrder(GroupName::compareTo));


    private static <T, V> T getLargestOrMostPopular(final Collection<Student> students,
                                                    final Collector<Student, ?, Map<T, V>> collector,
                                                    final Comparator<Map.Entry<T, V>> comparator,
                                                    final T defaultVal) {
        return students.stream()
                .collect(collector)
                .entrySet()
                .stream()
                .max(comparator)
                .map(Map.Entry::getKey).orElse(defaultVal);
    }

    private static <T> List<T> indexedGet(final Collection<Student> students,
                                          final int[] indices,
                                          final Function<Student, T> mapper,
                                          final Collector<T, ?, List<T>> collector) {
        return toList(Arrays.stream(indices).mapToObj(List.copyOf(students)::get), mapper, collector);
    }

    private static <T, C extends Collection<T>, V> C mapToCollection(final Stream<V> values,
                                                                     final Function<V, T> mapper,
                                                                     final Collector<T, ?, C> collector) {
        return values.map(mapper).collect(collector);
    }

    private static <T, V> List<T> toList(final Stream<V> values,
                                         final Function<V, T> mapper,
                                         final Collector<T, ?, List<T>> collector) {
        return mapToCollection(values, mapper, collector);
    }

    private static <T> List<T> toSortedList(final Stream<T> stream,
                                            final Comparator<T> comparator,
                                            final Collector<T, ?, List<T>> collector) {
        return stream.sorted(comparator).collect(collector);
    }

    private static List<Group> getSortedGroups(final Collection<Student> students,
                                               final Comparator<Student> studentComparator) {
        return toSortedList(
                students.stream()
                        .sorted(studentComparator)
                        .collect(COLLECT_BY_GROUP)
                        .entrySet()
                        .stream()
                        .map(e -> new Group(e.getKey(), e.getValue()))
                , GROUP_COMPARATOR, GROUP_TO_UNMODIFIABLE_LIST_COLLECTOR);
    }

    private static <T, K> Predicate<T> equalsPredicate(final Function<T, K> extractor, final K value) {
        return student -> Objects.equals(extractor.apply(student), value);
    }

    private static <K> List<Student> filterByKey(final Collection<Student> students,
                                                 final K key,
                                                 final Function<Student, K> extractor) {
        return toSortedList(students.stream().filter(equalsPredicate(extractor, key)),
                STUDENT_COMPARATOR, STUDENT_TO_UNMODIFIABLE_LIST_COLLECTOR);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return toList(students.stream(), this::fullName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return toList(students.stream(), Student::getFirstName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return toList(students.stream(), Student::getLastName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return toList(students.stream(), Student::getGroup, GROUP_NAME_TO_LIST_COLLECTOR);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return mapToCollection(students.stream(), Student::getFirstName, STRING_TO_SET_COLLECTOR);
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream()
                .max(Student::compareTo)
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return toSortedList(students.stream(), Student::compareTo, STUDENT_TO_UNMODIFIABLE_LIST_COLLECTOR);
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return toSortedList(students.stream(), STUDENT_COMPARATOR, STUDENT_TO_UNMODIFIABLE_LIST_COLLECTOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return filterByKey(students, name, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return filterByKey(students, name, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return filterByKey(students, group, Student::getGroup);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return students.stream()
                .filter(equalsPredicate(Student::getGroup, group))
                .collect(COLLECTOR_TO_MAP);
    }

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getSortedGroups(students, STUDENT_COMPARATOR);
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getSortedGroups(students, Student::compareTo);
    }

    @Override
    public List<String> getFirstNames(Collection<Student> students, int[] indices) {
        return indexedGet(students, indices, Student::getFirstName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public List<String> getLastNames(Collection<Student> students, int[] indices) {
        return indexedGet(students, indices, Student::getLastName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public List<GroupName> getGroups(Collection<Student> students, int[] indices) {
        return indexedGet(students, indices, Student::getGroup, GROUP_NAME_TO_LIST_COLLECTOR);
    }

    @Override
    public List<String> getFullNames(Collection<Student> students, int[] indices) {
        return indexedGet(students, indices, this::fullName, STRING_TO_LIST_COLLECTOR);
    }

    @Override
    public GroupName getLargestGroup(Collection<Student> students) {
        return getLargestOrMostPopular(students, COLLECT_BY_GROUP, LARGEST_GROUP_COMPARATOR, null);
    }

    // :NOTE: you create a new comparator object every time you call the function // Fixed
    @Override
    public GroupName getLargestGroupFirstName(Collection<Student> students) {
        return getLargestOrMostPopular(students, COLLECT_BY_GROUP, LARGEST_GROUP_FIRST_NAME_COMPARATOR, null);
    }

    @Override
    public String getMostPopularName(Collection<Student> students) {
        return getLargestOrMostPopular(students, NAME_AND_SET_OF_GROUP_COLLECTOR,
                MOST_POPULAR_NAME_COMPARATOR, "");
    }

    private String fullName(final Student student) {
        return student.getFirstName() + " " + student.getLastName();
    }
}
