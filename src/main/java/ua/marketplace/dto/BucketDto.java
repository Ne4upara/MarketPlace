package ua.marketplace.dto;

import java.math.BigDecimal;
import java.util.List;

public record BucketDto(
        Long id,
        List<ProductFromBucketDto> products,
        BigDecimal totalPrice
) {
}
