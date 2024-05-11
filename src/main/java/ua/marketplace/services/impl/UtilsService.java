package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.utils.ErrorMessageHandler;

import java.security.Principal;
import java.util.List;
import java.util.UUID;

/**
 * Service class for utility operations.
 */
@Service
@RequiredArgsConstructor
public class UtilsService {

    private final UserRepository userRepository;
    private final ProductRepository productRepository;

    /**
     * Constructs a Pageable object for pagination.
     *
     * @param num      The page number.
     * @param size     The number of items per page.
     * @param sortBy   The field to sort by.
     * @param orderBy  The sorting order ("ASC" for ascending, "DESC" for descending).
     * @return Pageable object for pagination.
     */
    public Pageable getPageRequest(int num, int size, String sortBy, String orderBy) {
        return PageRequest.of(num, size, isSort(sortBy, orderBy));
    }

    /**
     * Constructs a Sort object based on sorting parameters.
     *
     * @param sortBy   The field to sort by.
     * @param orderBy  The sorting order ("ASC" for ascending, "DESC" for descending).
     * @return Sort object for sorting.
     */
    private Sort isSort(String sortBy, String orderBy) {
        if ("ASC".equals(orderBy)) return Sort.by(sortBy).ascending();
        return Sort.by(sortBy).descending();
    }

    /**
     * Retrieves the user associated with the provided principal.
     *
     * @param principal The principal representing the user.
     * @return The User object associated with the principal.
     * @throws ResponseStatusException if the user is not found.
     */
    public User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.UNAUTHORIZED, ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }

    /**
     * Converts a Page of products to a list of MainPageProductDto.
     *
     * @param products The Page of products.
     * @return List of MainPageProductDto objects.
     */
    public List<MainPageProductDto> convertProductListToDto(Page<Product> products) {
        return products.stream().map(ProductMapper.PRODUCT_INSTANCE::productToMainPageDto)
                .toList();
    }

    /**
     * Retrieves a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return The Product object with the specified ID.
     * @throws ResponseStatusException if the product is not found.
     */
    public Product getProductById(Long id) {
        return productRepository.findById(id)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.PRODUCT_NOT_FOUND, id)));
    }

    public String getRandomName(){
        UUID uuid = UUID.randomUUID();
        return uuid.toString();
    }
}
