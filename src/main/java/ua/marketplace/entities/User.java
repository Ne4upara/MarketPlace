package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

/**
 * An entity class representing a user in the system.
 */
@Entity
@Table(name = "users")
@Getter
@Setter
@ToString
@Builder
@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor
public class User {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "phone_number")
    private String phoneNumber;

    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "sms_code_id", referencedColumnName = "id")
    private SmsCode smsCode;
}
