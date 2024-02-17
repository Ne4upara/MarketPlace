package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.VerificationCode;

public interface VerificationCodeRepository extends JpaRepository<VerificationCode, Long> {
}
