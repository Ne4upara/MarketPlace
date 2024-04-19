package ua.marketplace.dto;

import java.io.Serializable;
import java.util.List;

public record Pagination(
        int pageNumber,
        Long totalElements,
        int totalPages,
        List<MainPageProductDto> body
) implements Serializable {
}
