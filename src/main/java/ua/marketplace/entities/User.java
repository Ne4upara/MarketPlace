package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;
import org.apache.commons.lang3.builder.ToStringExclude;
import org.hibernate.proxy.HibernateProxy;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * An entity class representing a user in the system. This class is mapped to the 'users' table in the database.
 */
@Entity
@Table(name = "users")
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "phone_number", unique = true)
    private String phoneNumber;

    @Column(name = "first_name")
    private String firstName;

    @OneToOne(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @ToString.Exclude
    private VerificationCode verificationCode;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @ToStringExclude
    private List<Favorite> favorites;

    @Column(name = "role", insertable = false)
    private String role;

    @Column(name = "is_enabled", insertable = false)
    private Boolean isEnabled;

    @Column(name = "creation_date")
    private LocalDateTime creationDate;

    @OneToOne(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @ToString.Exclude
    private OrderList orderList;

    /**
     * A method called before the entity is persisted to the database. Sets the creation date to the current date and time.
     */
    @PrePersist
    protected void onCreate() {
        creationDate = LocalDateTime.now();

        if(this.orderList == null){
            this.orderList = new OrderList();
            this.orderList.setUser(this);
            this.orderList.setProducts(new ArrayList<>());
            this.orderList.setTotalPrice(BigDecimal.ZERO);
        }
    }

    /**
     * Overrides the default equals() method to compare User instances based on their IDs.
     * This is necessary for Hibernate to correctly identify and manage entities.
     */
    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        Class<?> oEffectiveClass = o instanceof HibernateProxy ? ((HibernateProxy) o).getHibernateLazyInitializer().getPersistentClass() : o.getClass();
        Class<?> thisEffectiveClass = this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass() : this.getClass();
        if (thisEffectiveClass != oEffectiveClass) return false;
        User user = (User) o;
        return getId() != null && Objects.equals(getId(), user.getId());
    }

    /**
     * Overrides the default hashCode() method to generate a hash code based on the User's class.
     * This is necessary for Hibernate to correctly identify and manage entities.
     */
    @Override
    public final int hashCode() {
        return this instanceof HibernateProxy ? ((HibernateProxy) this).getHibernateLazyInitializer().getPersistentClass().hashCode() : getClass().hashCode();
    }

}
