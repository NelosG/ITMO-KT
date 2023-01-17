package ru.ifmo.pga.software.design.core.entity;

import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Transient;

import java.util.Objects;

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@MappedSuperclass
public abstract class AbstractEntity {
    public static final String ID = "id";

    protected Long id;

    @Transient
    public abstract Long getId();

    public void setId(final Long id) {
        this.id = id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final AbstractEntity other = (AbstractEntity) o;
        return Objects.equals(id, other.id);
    }
}
