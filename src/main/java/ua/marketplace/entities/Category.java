package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

/**
 * An entity class representing a category in the system.
 * This class maps to the `product_categories` table in the database.
 */
@Entity
@Table(name = "product_categories")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Category {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name_url")
    private String categoryName;

    @Column(name = "name_ukr")
    private String nameUkr;

}
