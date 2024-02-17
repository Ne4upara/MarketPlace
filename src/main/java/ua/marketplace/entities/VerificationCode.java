package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "registration_code")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
public class VerificationCode {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "code")
    private String code;

    @Column(name = "created_code")
    private LocalDateTime createdTimeCode;

    @Column(name = "entry_by_code")
    private Boolean isEntryByCode;

    @Column(name = "login_attempt")
    private int loginAttempt;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "phone_number", referencedColumnName = "phone_number")
    private User user;


}
