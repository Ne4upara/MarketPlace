package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * An entity class representing a sms code in the system.
 */
@Entity
@Table(name = "sms_codes")
@Getter
@Setter
@ToString
@Builder
@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor
public class SmsCode {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "code")
    private String code;

    @Column(name = "create_at")
    private LocalDateTime createAt;

    @Column(name = "is_enable")
    private boolean isEnable;

    @Column(name = "verification_attempts")
    private int verificationAttempts;
}
