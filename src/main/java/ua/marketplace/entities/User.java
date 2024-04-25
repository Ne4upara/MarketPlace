package ua.marketplace.entities;

import jakarta.persistence.*; // For handling JPA annotations
import lombok.*; // For generating boilerplate code like getters, setters, etc.
import org.hibernate.proxy.HibernateProxy; // For handling Hibernate proxies

import java.time.LocalDateTime; // For handling date and time
import java.util.Objects; // For handling null values and objects

/**
 * An entity class representing a user in the system. This class is mapped to the 'users' table in the database.
 */
@Entity
@Table(name = "users")
@Getter
@Setter
@ToString // Generates toString() method
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id; // The unique identifier for a user

    @Column(name = "phone_number", unique = true)
    private String phoneNumber; // The user's phone number, unique for each user

    @Column(name = "first_name")
    private String firstName; // The user's first name

    @OneToOne(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @ToString.Exclude // Exclude VerificationCode from toString() to avoid infinite recursion
    private VerificationCode verificationCode; // The user's verification code

    @Column(name = "role", insertable = false)
    private String role; // The user's role, populated from the Role entity

    @Column(name = "is_enabled", insertable = false)
    private Boolean isEnabled; // The user's enabled status, populated from the UserStatus entity

    @Column(name = "creation_date")
    private LocalDateTime creationDate; // The date and time when the user was created

    /**
     * A method called before the entity is persisted to the database. Sets the creation date to the current date and time.
     */
    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();
    }

    /**
     * Overrides the default equals() method to compare User instances based on their IDs.
     * This is necessary for Hibernate to correctly identify and manage entities.
     */
    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        Class<?> oEffectiveClass = o instanceof HibernateProxy ? ((HibernateProxy) o).getHibernateLazyInitializer().getPersistentClass() : o.getClass();
        Class<?> thisEffectiveClass = this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass() : this.getClass();
        if (thisEffectiveClass != oEffectiveClass) return false;
        User user = (User) o;
        return getId() != null && Objects.equals(getId(), user.getId());
    }

    /**
     * Overrides the default hashCode() method to generate a hash code based on the User's class.
     * This is necessary for Hibernate to correctly identify and manage entities.
     */
    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }
}

