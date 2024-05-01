package ua.marketplace.controllers;

import ua.marketplace.dto.BucketDto;

import java.security.Principal;

public interface IBucketController {

    BucketDto getBucket(Principal principal);

    BucketDto addProductToBucket(Principal principal, Long id);

    BucketDto deleteProductFromBucket(Principal principal, Long id);
}
