package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * An entity class representing a product photo in the system.
 */
@Entity
@Table(name = "product_photos")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductPhoto {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @ToString.Exclude
    @JoinColumn(name = "product_id")
    private Product product;

    @Column(name = "photo_link")
    private String photoLink;

    @Column(name = "number_photo")
    private Integer numberPhoto;

    @Column(name = "original_name")
    private String originalName;

    @Column(name = "main_page")
    private boolean mainPage;

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
