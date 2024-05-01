package ua.marketplace.controllers;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.BucketDto;
import ua.marketplace.services.BucketService;

import java.security.Principal;

@RestController
@RequestMapping("/v1/buckets")
@RequiredArgsConstructor
public class BucketControllerImp implements IBucketController {

    private final BucketService bucketService;

    @GetMapping("/view")
    public BucketDto getBucket(Principal principal){
       return bucketService.viewBucket(principal);
    }

    @PostMapping("/add/{id}")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public BucketDto addProductToBucket(Principal principal, @PathVariable Long id){
        return bucketService.addToBucket(id,principal);
    }

    @DeleteMapping("/delete/{id}")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public BucketDto deleteProductFromBucket(Principal principal, @PathVariable Long id){
        return bucketService.deleteFromBucked(id,principal);
    }
}
