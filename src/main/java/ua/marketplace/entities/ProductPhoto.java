package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;


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
    @JoinColumn(name = "product_id")
    private Product product;

    @Column(name = "photo_link")
    private String photoLink;

    @Column(name = "main_page")
    private String mainPage;

}
