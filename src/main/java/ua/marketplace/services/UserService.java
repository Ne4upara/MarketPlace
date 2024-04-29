package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.Pagination;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;

import java.security.Principal;
import java.util.List;

@Service
@RequiredArgsConstructor
public class UserService implements IUserService {

    private final UtilsService utilsService;
    private final ProductRepository productRepository;
    private final FavoriteRepository favoriteRepository;

    @Override
    public Pagination getViewMyProduct(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal) {
        User user = utilsService.getUserByPrincipal(principal);
        Pageable pageable = utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Product> pageAll = productRepository.findAllByOwner(user, pageable);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(pageAll);

        return new Pagination(pageAll.getNumber(),
                pageAll.getTotalElements(),
                pageAll.getTotalPages(),
                pageAllContent);
    }

    @Override
    public Pagination getAllFavorite(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal) {
        User user = utilsService.getUserByPrincipal(principal);
        Pageable pageable = utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Favorite> favoritesPage = favoriteRepository.findAllByUser(user, pageable);
        Page<Product> productFavorite = favoritesPage.map(Favorite::getProduct);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(productFavorite);
        return new Pagination(favoritesPage.getNumber(),
                favoritesPage.getTotalElements(),
                favoritesPage.getTotalPages(),
                pageAllContent);
    }
}
