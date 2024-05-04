package ua.marketplace.entities;

import java.time.LocalDateTime;

import jakarta.persistence.*;
import lombok.*;

/**
 * Entity class representing a favorite association between a user and a product.
 * Contains information about the association, including its ID, associated user,
 * associated product, and creation date.
 */
@Entity
@Table(name = "favorites")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Favorite {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne
    @JoinColumn(name = "product_id")
    private Product product;

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
