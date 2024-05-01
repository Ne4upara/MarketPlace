package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Bucket;
import ua.marketplace.entities.User;

@Repository
public interface BucketRepository extends JpaRepository<Bucket,Long> {

    Bucket findByUser(User user);
}
