package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
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

    @Column(name = "code")
    private String code;

    @Column(name = "created_code")
    private LocalDateTime createdTimeCode;

    @Column(name = "role")
    private String role;

    @Column(name = "is_enabled")
    private Boolean isEnabled;

    /**
     * Compares this User with the specified object for equality.
     * Two User objects are considered equal if they have the same id, phone, and password.
     *
     * @param object the object to be compared for equality with this User
     * @return true if the specified object is equal to this User, otherwise false
     */
    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        User user = (User) object;
        return Objects.equals(id, user.id) && Objects.equals(phone, user.phone);
    }

    /**
     * Returns a hash code value for this User.
     *
     * @return a hash code value for this User
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, phone);
    }
}