package ua.marketplace.repositoryes;

import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.ProductPhoto;

@Repository
public interface PhotoRepository extends JpaRepository<ProductPhoto, Long> {

    @Transactional
    @Modifying
    @Query("DELETE FROM ProductPhoto p WHERE  p.id = :photoId")
    void deleteByPhotoId(@Param("photoId") Long photoId);
}
