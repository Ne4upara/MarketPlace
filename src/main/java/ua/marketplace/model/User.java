package ua.marketplace.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "users")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotNull(message = "the phone can`t be null")
//    @Size(min = 9, max = 10, message = "need 9 or 10 numbers")
    @Pattern(regexp = "\\d{9,10}", message = "Phone number must be 9 or 10 digits")
    @Column()
    private String phone;

    @NotNull(message = "the password can`t be null")
    @Size(min = 3, max = 10, message = "minimum password must contain at least 3 characters")
    @Column()
    private String password;

    @NotNull
    @Column(nullable = false)
    private boolean enabled;

    @NotNull
    @Column(nullable = false)
    private String role;
}