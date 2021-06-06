package info.kgeorgiy.java.advanced.student;

import java.util.Collection;
import java.util.List;

/**
 * Hard-version interface
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-student">Student</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface AdvancedQuery extends GroupQuery {
    /**
     * Returns the first name of the student such that most number of groups has student with that name.
     * If there are more than one such name, the largest one is returned.
     */
    String getMostPopularName(Collection<Student> students);

    /** Returns student {@link Student#getFirstName() first names} by indices. */
    List<String> getFirstNames(Collection<Student> students, final int[] indices);

    /** Returns student {@link Student#getLastName() last names} by indices. */
    List<String> getLastNames(Collection<Student> students, final int[] indices);

    /** Returns student {@link Student#getGroup() groups} by indices. */
    List<GroupName> getGroups(Collection<Student> students, final int[] indices);

    /** Returns full student name by indices. */
    List<String> getFullNames(Collection<Student> students, final int[] indices);
}
