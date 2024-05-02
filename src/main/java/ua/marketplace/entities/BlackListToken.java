package ua.marketplace.entities;

import java.time.LocalDateTime;
import java.util.Date;
import jakarta.persistence.*;
import lombok.*;

/**
 * Entity class representing a blacklisted token.
 * Contains information about the blacklisted token, including its ID,
 * token value, expiration date, and creation date.
 */
@Entity
@Table(name = "black_list")
@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class BlackListToken {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "token")
    private String token;

    @Column(name = "expired_token")
    private Date expiredTokens;

    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    /**
     * Callback method executed before the entity is persisted.
     * Sets the creation date to the current date and time.
     */
    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();
    }
}
