package ua.marketplace.entities;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * An entity class representing a verification code in the user.
 * This class uses Lombok annotations for generating boilerplate code such as getters, setters,
 * toString, builder, and constructors.
 */
@Entity // Marks this class as an entity, which maps to a database table
@Table(name = "verification_codes") // Specifies the name of the database table
@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class VerificationCode {

    /**

