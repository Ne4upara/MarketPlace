package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

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
@EqualsAndHashCode
public class User {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "phone_number")
    private String phoneNumber;

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
}
