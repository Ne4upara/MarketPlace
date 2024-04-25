package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

/**
 * An entity class representing a category in the system.
 * This class maps to the `product_categories` table in the database.
 *
 * @author Your Name
 * @since 1.0.0
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

    /**
     * The unique identifier of the category.
     * This field is mapped to the `id` column in the `product_categories` table.
     */
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * The name of the category, in the form of a URL.
     * This field is mapped to the `name_url` column in the `product_categories` table.
     */
    @Column(name = "name_url")
    private String categoryName;

    /**
     * The name of the category in Ukrainian.
     * This field is mapped to the `name_ukr` column in the `product_categories` table.
     */
    @Column(name = "name_ukr")
    private String nameUkr;
}
