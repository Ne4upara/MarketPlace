package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.proxy.HibernateProxy;
import ua.marketplace.data.ProductCategory;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * An entity class representing a product in the system.
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

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name")
    private String productName;

    @Column(name = "photo")
    private String productPhotoLink;

    @Column(name = "price")
    private BigDecimal productPrice;

    @Column(name = "description")
    private String productDescription;

    @Column(name = "category")
    private ProductCategory productCategory;

    @Column(name = "product_type")
    private String productType;

    @ManyToOne
    @JoinColumn(name = "owner_id")
    private User owner;

    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    @Column(name = "rating")
    private int productRating;

    @Column(name = "rating_count")
    private int productRatingCount;

    @Column(name = "quantity")
    private int productQuantity;

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

    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }
}
