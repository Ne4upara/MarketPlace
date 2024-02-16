package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.SmsCode;

/**
 * A repository interface for accessing sms code data in the database.
 */
@Repository
public interface SmsCodeRepository extends JpaRepository<SmsCode, Long> {
}
