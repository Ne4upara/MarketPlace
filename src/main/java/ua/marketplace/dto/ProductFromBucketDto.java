package ua.marketplace.dto;

import java.math.BigDecimal;

public record ProductFromBucketDto(
        Long id,
        String productPhotoLink,
        String productName,
        BigDecimal productPrice
) {
}
