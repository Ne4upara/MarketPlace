package ua.marketplace.dto;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Builder
@Getter
@Setter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public class ProductDto {

    private String productName;
    private String productPhotoLink;
    private BigDecimal productPrice;
    private String productDescription;
    private String productCategory;
    private String productType;
    private LocalDateTime creationDate;
    private int productRating;
    private int productQuantity;
}
