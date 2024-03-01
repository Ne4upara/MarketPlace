package ua.marketplace.dto;

import lombok.*;
import ua.marketplace.data.ProductCategory;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ProductDto {

    private String productName;
    private String productPhotoLink;
    private BigDecimal productPrice;
    private String productDescription;
    private ProductCategory productCategory;
    private String productType;
    private LocalDateTime creationDate;
    private int productRating;
    private int productQuantity;
}
