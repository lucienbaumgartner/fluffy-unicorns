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
- ~~formulate bundler function~~
- variable random noise factor
- elliptical function in addition to circular one
- ~~add additional letters~~
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
- ~~create jitter factor a variable~~
- possible variables:
    - alpha
    - ~~jitter factor~~
        - differentiate between x and y jitter
    - method (raster, polygon) [consolidate]
    - conditional additional layers
    - background
    - grid
- combine multi-liner and one-liner into one function

## examples
Small multiline 'logos' work pretty well already:
![hex mode banner](/display/banner-hex.png)
![hex mode](/display/logo-triple-hex.png)
![raster mode](/display/logo-triple-raster.png)
![multilayering](/display/logo-triple-hex6-clear.png)

## still in development
The ideal distribution parameter for large word numbers has still to be figured out, as you can see:

![](/display/gencon.png)
