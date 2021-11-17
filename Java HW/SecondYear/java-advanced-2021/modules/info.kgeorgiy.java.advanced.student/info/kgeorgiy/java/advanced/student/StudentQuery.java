package info.kgeorgiy.java.advanced.student;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Easy-version interface
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-student">Student</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 * <p>
 * <em>Students ordered by name</em>:
 * students ordered by {@link Student#getLastName() last name} in descending order
 * students with equal last names are ordered by {@link Student#getFirstName() first name} in descending order,
 * students having equal both last and first names are ordered by {@link Student#getId() id} in ascending order.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface StudentQuery {
    /** Returns student {@link Student#getFirstName() first names}. */
    List<String> getFirstNames(List<Student> students);

    /** Returns student {@link Student#getLastName() last names}. */
    List<String> getLastNames(List<Student> students);

    /** Returns student {@link Student#getGroup() groups}. */
    List<GroupName> getGroups(List<Student> students);

    /** Returns full student name. */
    List<String> getFullNames(List<Student> students);

    /** Returns distinct student {@link Student#getFirstName() first names} in lexicographic order. */
    Set<String> getDistinctFirstNames(List<Student> students);

    /** Returns a {@link Student#getFirstName() first name} of the student with maximal {@link Student#getId() id}. */
    String getMaxStudentFirstName(List<Student> students);

    /** Returns students ordered by {@link Student#getId() id}. */
    List<Student> sortStudentsById(Collection<Student> students);

    /** Returns students ordered by name. */
    List<Student> sortStudentsByName(Collection<Student> students);

    /** Returns students having specified first name. Students are ordered by name. */
    List<Student> findStudentsByFirstName(Collection<Student> students, String name);

    /** Returns students having specified last name. Students are ordered by name. */
    List<Student> findStudentsByLastName(Collection<Student> students, String name);

    /** Returns students having specified groups. Students are ordered by name. */
    List<Student> findStudentsByGroup(Collection<Student> students, GroupName group);

    /** Returns map of group's student last names mapped to minimal first name. */
    Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group);

    /** Stable version of {@link #findStudentNamesByGroup(Collection, GroupName)} */
    default List<Map.Entry<String, String>> findStudentNamesByGroupList(final List<Student> students, final GroupName group) {
        return findStudentNamesByGroup(students, group).entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .collect(Collectors.toUnmodifiableList());
    }
}
