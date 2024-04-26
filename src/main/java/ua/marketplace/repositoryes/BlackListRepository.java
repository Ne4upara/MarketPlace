package ua.marketplace.repositoryes;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import ua.marketplace.entities.BlackListToken;

public interface BlackListRepository extends JpaRepository <BlackListToken, Long> {

    Boolean existsByToken(String token);

    @Query("SELECT b FROM BlackListToken b WHERE b.expiredTokens < CURRENT_TIMESTAMP")
    List<BlackListToken> findAllExpiredTokens();

}