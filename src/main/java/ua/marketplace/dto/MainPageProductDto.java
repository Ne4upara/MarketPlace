package ua.marketplace.dto;

import lombok.*;

import java.math.BigDecimal;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class MainPageProductDto {

    private String productPhotoLink;
    private String productName;
    private String productType;
    private BigDecimal productPrice;
    private int productRating;
}
