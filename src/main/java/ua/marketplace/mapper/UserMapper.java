package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.User;

import java.util.List;
import java.util.Set;

/**
 * Mapper interface for converting between User entities and DTOs.
 * Uses MapStruct for mapping configurations.
 */
@Mapper(componentModel = "spring")
public interface UserMapper {

    UserMapper USER_MAPPER = Mappers.getMapper(UserMapper.class);

    /**
     * Converts a User entity to a UserDto.
     *
     * @param user The User entity to convert.
     * @param favorites The set of favorite associations for the user.
     * @return The corresponding UserDto.
     */
    @Mapping(target = "favorite_id", expression = "java(getAllFavoriteProducts(favorites))")
    @Mapping(target = "order_list",
            expression = "java(OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderUserInfoDto(user.getOrderList()))")
    UserDto userToUserDTO(User user, Set<Favorite> favorites );

    /**
     * Helper method to retrieve all favorite product IDs associated with a user.
     *
     * @param favorites The set of favorite associations for the user.
     * @return The list of all favorite product IDs.
     */
    default List<Long> getAllFavoriteProducts(Set<Favorite> favorites) {
        return favorites.stream()
                .map(favorite -> favorite.getProduct().getId())
                .toList();
    }
}
