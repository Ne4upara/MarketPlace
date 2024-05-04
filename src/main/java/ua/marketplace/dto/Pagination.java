package ua.marketplace.dto;

import java.io.Serializable;
import java.util.List;

/**
 * Represents a pagination object containing information about a paginated list of items.
 * Includes details such as the page number, total number of elements, total number of pages,
 * and the list of items in the current page.
 */
public record Pagination(
        int pageNumber,
        Long totalElements,
        int totalPages,
        List<MainPageProductDto> body
) implements Serializable {
}
