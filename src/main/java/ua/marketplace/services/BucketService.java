package ua.marketplace.services;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.BucketDto;
import ua.marketplace.entities.Bucket;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.BucketMapper;
import ua.marketplace.repositoryes.BucketRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.utils.ErrorMessageHandler;

import java.math.BigDecimal;
import java.security.Principal;

@Service
@RequiredArgsConstructor
public class BucketService implements IBucketService {

    private final BucketRepository bucketRepository;
    private final UtilsService utilsService;
    private final ProductRepository productRepository;

    @Override
    public BucketDto viewBucket(Principal principal) {
        return BucketMapper.BUCKET_INSTANCE.bucketToBucketDto(getBucketByPrincipal(principal));
    }

    @Override
    @Transactional
    public BucketDto addToBucket(Long productId, Principal principal) {

        Product productById = getProductById(productId);

        Bucket bucket = getBucketByPrincipal(principal);
        bucket.getProducts().add(productById);
        bucket.setTotalPrice(bucket.getTotalPrice().add(productById.getProductPrice()));
        Bucket savedBucket = bucketRepository.save(bucket);
        return BucketMapper.BUCKET_INSTANCE.bucketToBucketDto(savedBucket);
    }

    @Override
    @Transactional
    public BucketDto deleteFromBucked(Long productId, Principal principal) {

        Product productById = getProductById(productId);
        Bucket bucket = getBucketByPrincipal(principal);

        productExistInBucket(bucket, productById);

        bucket.getProducts().remove(productById);
        bucket.setTotalPrice(decrementTotalPrice(bucket,productById));
        Bucket savedBucket = bucketRepository.save(bucket);
        return BucketMapper.BUCKET_INSTANCE.bucketToBucketDto(savedBucket);
    }

    private Bucket getBucketByPrincipal(Principal principal) {
        User userByPrincipal = utilsService.getUserByPrincipal(principal);
        Bucket bucket = userByPrincipal.getBucket();
        if(bucket == null) {
            Bucket createdBucket = new Bucket();
            createdBucket.setUser(userByPrincipal);
            return createdBucket;
        }

            return bucket;
    }

    private Product getProductById(Long id) {
        return productRepository.findById(id)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.PRODUCT_NOT_FOUND, id)));
    }

    private void productExistInBucket(Bucket bucket, Product product) {
        if (!bucket.getProducts().contains(product)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT,
                    String.format(ErrorMessageHandler.PRODUCT_NOT_FOUND, product.getId()));
        }
    }

    private BigDecimal decrementTotalPrice(Bucket bucket, Product product){
        bucket.setTotalPrice(bucket.getTotalPrice().subtract(product.getProductPrice()));
        if(bucket.getTotalPrice().doubleValue() <= 0){
            return BigDecimal.ZERO;
        } else {
            return bucket.getTotalPrice();
        }
    }
}
