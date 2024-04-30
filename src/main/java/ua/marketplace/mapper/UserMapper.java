package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.User;

import java.util.List;
import java.util.Set;

@Mapper(componentModel = "spring")
public interface UserMapper {

    UserMapper USER_MAPPER = Mappers.getMapper(UserMapper.class);

    @Mapping(target = "favorite_id", expression = "java(getAllFavorite(favorites))")
    UserDto userToUserDTO(User user, Set<Favorite> favorites );

    default List<Long> getAllFavorite(Set<Favorite> favorites) {
        return favorites.stream()
                .map(favorite -> favorite.getProduct().getId())
                .toList();
    }
}
