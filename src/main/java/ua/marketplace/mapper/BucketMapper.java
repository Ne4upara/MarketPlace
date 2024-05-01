package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.BucketDto;
import ua.marketplace.dto.ProductFromBucketDto;
import ua.marketplace.entities.Bucket;
import ua.marketplace.entities.Product;

@Mapper(componentModel = "spring")
public interface BucketMapper {

    BucketMapper BUCKET_INSTANCE = Mappers.getMapper(BucketMapper.class);

    BucketDto bucketToBucketDto(Bucket bucket);

    @Mapping(target = "productPhotoLink", expression = "java(ProductMapper.PRODUCT_INSTANCE.getMainPagePhotoLink(product))")
    ProductFromBucketDto productToProductBucketDto(Product product);
}
