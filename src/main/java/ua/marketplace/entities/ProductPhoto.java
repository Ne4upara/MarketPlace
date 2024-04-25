/**
 * An entity class representing a product photo in the system.
 * This class is mapped to the "product_photos" table in the database.
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

    /**
     * The unique identifier for this product photo.
     * Generated automatically by the database.
     */
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /**
     * The product that this photo belongs to.
     * This is a many-to-one relationship, as multiple photos can belong to the same product.
     */
    @ManyToOne
    @ToString.Exclude
    @JoinColumn(name = "product_id")
    private Product product;

    /**
     * The URL of the photo.
     * This is a simple string field that stores the link to the image file.
     */
    @Column(name = "photo_link")
    private String photoLink;

    /**
     * Whether or not this photo should be displayed on the main page.
     * This is a boolean field that indicates whether the photo should be featured prominently.
     */
    @Column(name = "main_page")
    private boolean mainPage;

    /**
     * The date and time that this photo was created.
     * This is automatically set by the system when the photo is first saved.
     */
    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    /**
     * A method that is called automatically before this photo is persisted to the database.
     * Sets the creation date to the current date and time.
     */
    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();
    }
}
