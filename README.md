# fluffy-unicorns

This is a package to write with data in R. Each character is computed separately, allowing for multilayering. Functionality of the bundler function (Latest Update):

```
lapply(1:max(df$index), function(x) chars2data(char=df$chars[x],
                                                    anker=df$ankers[x],
                                                    ylims = ylims,
                                                    index=df$index[x],
                                                    jitterfactor=2)) %>%
              do.call(rbind, .)
```


## tasks
- annotate script
- __formulate bundler function__
- variable random noise factor
- elliptical function in addition to circular one
- __add additional letters__
    - available:
        - A
        - B
        - C
        - D
        - E
        - G
        - I
        - L
        - M
- replace vertical lines by function
- __make jitter factor a variable__
- possible variables:
    - alpha
    - __jitter factor__
        - differentiate between x and y jitter
    - method (raster, polygon) [consolidate]
    - conditional additional layers
    - background
    - grid

  ![](../display/banner-hex.png)
