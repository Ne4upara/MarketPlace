package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.OrderList;
import ua.marketplace.entities.User;

/**
 * Repository interface for accessing and managing OrderList entities in the database.
 * Extends JpaRepository for basic CRUD operations.
 */
@Repository
public interface OrderListRepository extends JpaRepository<OrderList,Long> {

    /**
     * Retrieves the order list associated with the given user.
     *
     * @param user The user associated with the order list.
     * @return The order list associated with the user, if found, otherwise null.
     */
    OrderList findByUser(User user);
}
