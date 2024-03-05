package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * An entity class representing a verification code in the user.
 */
@Entity
@Table(name = "verification_codes")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class VerificationCode {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "code")
    private String code;

    @Column(name = "created_code", insertable = false)
    private LocalDateTime createdTimeCode;

    @Column(name = "entry_by_code", insertable = false)
    private Boolean isEntryByCode;

    @Column(name = "login_attempt", columnDefinition = "0", insertable = false)
    private int loginAttempt;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "phone_number", referencedColumnName = "phone_number")
    User user;
}