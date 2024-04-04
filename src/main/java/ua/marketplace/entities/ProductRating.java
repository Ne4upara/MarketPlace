package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;
@Entity
@Table(name = "product_ratings")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductRating {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "product_id")
    private Product product;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;

    @Column(name = "rating")
    private int rating;

    @Column(name = "review")
    private String review; // Нове поле для відгуку

    // Додайте інші необхідні поля, якщо потрібно
}
