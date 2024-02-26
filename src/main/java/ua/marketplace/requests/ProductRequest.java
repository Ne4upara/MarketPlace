package ua.marketplace.requests;

import lombok.*;

import java.math.BigDecimal;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductRequest {

    private String productName;
    private String productPhotoLink;
    private BigDecimal productPrice;
    private String productDescription;
    private String productCategory;
    private String productType;
}
