package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
}
