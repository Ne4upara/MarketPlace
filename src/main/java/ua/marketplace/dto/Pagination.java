package ua.marketplace.dto;

import java.util.List;

public record Pagination (
        int pageNumber,
        Long totalElements,
        int totalPages,
        List<MainPageProductDto> body
){
}
