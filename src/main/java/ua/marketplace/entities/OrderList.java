package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;

/**
 * Entity class representing an order list.
 * Contains information about the order list, including its ID, associated user,
 * list of products, and total price.
 */
@Entity
@Table(name = "order_list")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OrderList {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            name = "product_bucket",
            joinColumns = @JoinColumn(name = "product_bucket_id"),
            inverseJoinColumns = @JoinColumn(name = "product_id")
    )
    private List<Product> products;

    @Column(name = "total_price")
    private BigDecimal totalPrice;
}
