package ua.marketplace.entitys;

import jakarta.persistence.*;
import lombok.*;

import java.util.Objects;

/**
 * An entity class representing a user in the system.
 */
@Entity
@Table(name = "users")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "phone")
    private String phone;

    @Column(name = "password")
    private String password;

    /**
     * Compares this User with the specified object for equality.
     * Two User objects are considered equal if they have the same id, phone, and password.
     *
     * @param o the object to be compared for equality with this User
     * @return true if the specified object is equal to this User, otherwise false
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return Objects.equals(id, user.id) && Objects.equals(phone, user.phone) && Objects.equals(password, user.password);
    }

    /**
     * Returns a hash code value for this User.
     *
     * @return a hash code value for this User
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, phone, password);
    }
}
