package ua.marketplace.services;

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
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.utils.ErrorMessageHandler;

import java.security.Principal;
import java.util.List;

@Service
@RequiredArgsConstructor
public class UtilsService {

    private final UserRepository userRepository;

    public Pageable getPageRequest(int num, int size, String sortBy, String orderBy) {
        return PageRequest.of(num, size, isSort(sortBy, orderBy));
    }

    private Sort isSort(String sortBy, String orderBy) {
        if ("ASC".equals(orderBy)) return Sort.by(sortBy).ascending();
        return Sort.by(sortBy).descending();
    }

    public User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.UNAUTHORIZED, ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }

    public List<MainPageProductDto> convertProductListToDto(Page<Product> products) {
        return products.stream().map(ProductMapper.PRODUCT_INSTANCE::productToMainPageDto)
                .toList();
    }
}
