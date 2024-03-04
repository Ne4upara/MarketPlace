package ua.marketplace.dto;

import java.math.BigDecimal;

public record MainPageProductDto(
        String productPhotoLink,
        String productName,
        String productType,
        BigDecimal productPrice,
        int productRating
) {}
