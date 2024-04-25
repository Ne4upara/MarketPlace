package ua.marketplace.repositoryes;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import ua.marketplace.entities.BlackListToken;

/**
 * Interface for BlackListToken repository, extending JpaRepository to provide basic CRUD operations.
 */
public interface BlackListRepository extends JpaRepository <BlackListToken, Long> {

    /**

