package ua.marketplace.services;

import ua.marketplace.dto.BucketDto;

import java.security.Principal;

public interface IBucketService {

    BucketDto viewBucket(Principal principal);
    BucketDto addToBucket(Long productId, Principal principal);
    BucketDto deleteFromBucked(Long productId, Principal principal);
}
