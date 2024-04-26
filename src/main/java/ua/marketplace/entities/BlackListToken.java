package ua.marketplace.entities;

import java.time.LocalDateTime;
import java.util.Date;
import jakarta.persistence.*;
import lombok.*;

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

    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();
    }

}
