package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.proxy.HibernateProxy;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

/**
 * An entity class representing a product in the system.
 * A product has a unique identifier (ID), name, list of photos, price, description,
 * category, product type, owner, list of reviews, seller information, view count,
 * relevancy, enable/disable status, and creation date.
 */
@Entity
@Table(name = "products")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Product {

    // The unique identifier (ID) for the product
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // The name of the product
    @Column(name = "name")
    private String productName;

    // The list of photos associated with the product
    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<ProductPhoto> photos;

    // The price of the product
    @Column(name = "price")
    private BigDecimal productPrice;

    // The description of the product
    @Column(name = "description")
    private String productDescription;

    // The category to which the product belongs
    @ManyToOne
    @JoinColumn(name = "category_id")
    private Category category;

    // The type of the product
    @Column(name = "product_type")
    private String productType;

    // The user who owns the product
    @ManyToOne
    @JoinColumn(name = "owner_id")
    private User owner;

    // The list of reviews for the product
    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private List<ProductRating> reviews;

    // The name of the seller
    @Column(name = "seller_name")
    private String sellerName;

    // The phone number of the seller
    @Column(name = "seller_phone_number")
    private String sellerPhoneNumber;

    // The email of the seller
    @Column(name = "seller_email")
    private String sellerEmail;

    // The location of the seller
    @Column(name = "location")
    private String location;

    // The number of times the product has been viewed
    @Column(name = "count_view", insertable = false)
    private int countView;

    // The relevancy of the product
    @Column(name = "relevancy")
    private int relevancy;

    // The enable/disable status of the product
    @Column(name = "is_enabled", insertable = false)
    private Boolean isEnabled;

    // The creation date of the product
    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    /**
     * A method that sets the creation date to the current date and time when a new product is created.
     */
    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();
    }

    // Overriding the equals() method to compare the IDs of two products
    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        Class<?> oEffectiveClass = o instanceof HibernateProxy ? ((HibernateProxy) o).getHibernateLazyInitializer().getPersistentClass() : o.getClass();
        Class<?> thisEffectiveClass = this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass() : this.getClass();
        if (thisEffectiveClass != oEffectiveClass) return false;
        Product product = (Product) o;
        return getId() != null && Objects.equals(getId(), product.getId());
    }

    // Overriding the hashCode() method to generate a hash code based on the product's class
    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }

}
