package ua.marketplace.entities;

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

    @Column(name = "name")
    private String name;

    @Column(name = "phone_number")
    private String phoneNumber;

    @Column(name = "code")
    private String code;

    /**
     * Overrides equals method to compare User objects based on their attributes.
     *
     * @param o Object to compare with.
     * @return True if the objects are equal, false otherwise.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return Objects.equals(id, user.id) && Objects.equals(name, user.name) && Objects.equals(phoneNumber, user.phoneNumber) && Objects.equals(code, user.code);
    }

    /**
     * Generates hash code for User object based on its attributes.
     *
     * @return Hash code value.
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, name, phoneNumber, code);
    }
}