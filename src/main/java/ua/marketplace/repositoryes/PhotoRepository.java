package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.ProductPhoto;

@Repository
public interface PhotoRepository extends JpaRepository<ProductPhoto, Long> {
}
