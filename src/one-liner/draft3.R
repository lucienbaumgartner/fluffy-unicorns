library(dplyr)
library(ggplot2)
library(viridis)
rm(list=ls())

setwd('~/fluffy-unicorns/display/')
# circle function
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt) * 1.5
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

ellipseFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt) * 3
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# set limits
limits <- c(0, 100)
ylims <- c(40, 60)

# set y baseline for one-liners
y.baseline <- median(limits)

# fracture string
fracture <- function(x){
  strsplit(gsub("([[:alnum:]]{1})", "\\1 ", tolower(x)), " ")[[1]]
}

# get anker points
get_ankers <- function( index){
  buffer <- (limits[2]-limits[1])/max(index)
  nwords <- 
    range <- (limits[2]-limits[1])-(buffer*2)
  ankers <- seq(limits[1]+buffer, limits[2]-buffer, by=range/max(container$index))
  return(ankers)
}


# set up container
binning <- function(x){
  tibble(chars=fracture(x), 
         index=1:length(chars),
         data=NA)
}

df <- binning('Digital Democracy Lab')

vec <- NA
for(i in 1:nrow(df)){
  if(i==1){vec[i] <- 10}else{
    if(identical(df$chars[i], "")){vec[i] <- vec[i-1]+60}else{
      if(df$chars[i]%in%c('c')){vec[i] <- vec[i-1]+60}else{
        if(identical(df$chars[i], 'a')&identical(df$chars[i-1], 'l')){vec[i] <- vec[i-1]+50}else{
          if(df$chars[i-1]%in%c('c')&!identical(df$chars[i], 'y')){vec[i] <- vec[i-1]+20}else{
            if(df$chars[i-1]%in%c('g')&!identical(df$chars[i], 'y')){vec[i] <- vec[i-1]+30}else{
              if(identical(df$chars[i], 'r')){vec[i] <- vec[i-1]+30}else{
                if(df$chars[i-1]%in%c('e', 'm', 'o')){vec[i] <- vec[i-1]+60}else{
                  if(identical(df$chars[i], 'a')){vec[i] <- vec[i-1]+60}else{
                    vec[i] <- vec[i-1]+50
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

df <- df %>% mutate(ankers=vec)

chars2data <- function(char, anker, ylims, index, jitterfactor){
  
  if(identical(char, 'd')){
    dstring <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                      x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/index)))*jitterfactor),
                      lB= 'char_dstring')
    dcirc <- sapply(circleFun(c(anker, median(ylims)),20, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
      as_tibble %>% 
      mutate(lB='char_dcirc') %>% 
      filter(x>anker)
    return(rbind(dstring, dcirc))
  }
  if(identical(char, 'i')){
    return(tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                  x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/index)))*jitterfactor),
                  lB= 'char_i'))
  }
  if(identical(char, 'g')){
    gcirc <- sapply(circleFun(c(anker, median(ylims)),20, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
      as_tibble %>% 
      mutate(lB='char_dcirc') %>% 
      filter(x<anker)
    gstring <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                      x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/index)))*jitterfactor),
                      lB= 'char_i') %>% 
      filter(y<=median(ylims))
    return(rbind(gcirc, gstring))
  }
  if(identical(char, 't')){
    tbig <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                   x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/index)))*jitterfactor),
                   lB= 'char_tbig')
    tsmall <- tibble(y=jitter(rep(max(ylims), length(rep(seq(anker-15, anker+15, 0.2), 5))), factor = (1-(1-(1/index)))*jitterfactor),
                     x=jitter(rep(seq(anker-15, anker+15, 0.2), 5), factor = 15*jitterfactor),
                     lB= 'char_tsmall')
    return(rbind(tbig, tsmall))
  }
  if(identical(char, 'a')){
    aleft <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.3), 35), factor = 10*jitterfactor), 
                    x=jitter(rep(seq(anker-10, anker, 0.15), 35), factor = 10*jitterfactor),
                    lB='char_aleft') 
    aright <- tibble(y=jitter(rep(seq(ylims[2], ylims[1], -0.3), 35), factor = 10*jitterfactor), 
                     x=jitter(rep(seq(anker, anker+10, 0.15), 35), factor = 10*jitterfactor),
                     lB='char_aright')
    return(rbind(aleft, aright))
  }
  if(identical(char, 'l')){
    lbig <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                   x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/index)))*jitterfactor),
                   lB= 'char_lbig')
    lsmall <- tibble(y=jitter(rep(min(ylims), length(rep(seq(anker, anker+20, 0.2), 5))), factor = (1-(1-(1/index)))*jitterfactor), 
                     x=jitter(rep(seq(anker, anker+20, 0.2), 5), factor = 1*jitterfactor),
                     lB= 'char_lsmall')
    return(rbind(lbig, lsmall))
  }
  if(identical(char, 'c')){
    return(sapply(ellipseFun(c(anker,median(ylims)),20, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
             as_tibble %>% 
             mutate(lB='char_dcirc') %>% 
             filter(x<anker))
  }
  
  if(identical(char, 'b')){
    bsmall <- sapply(circleFun(c(anker,median(ylims)),8, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
      as_tibble %>% 
      mutate(lB='char_bsmall') %>% 
      filter(x>anker) %>% 
      mutate(x=x+5,y=y+6)
    bbig <- sapply(circleFun(c(anker,median(ylims)),12, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
      as_tibble %>% 
      mutate(lB='char_bbig') %>% 
      filter(x>anker) %>% 
      mutate(x=x+5,y=y-5)
    bstring <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                      x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor =(1-(1-(1/index)))*jitterfactor),
                      lB= 'char_bstring')
    return(rbind(bsmall, bbig, bstring))
  }
  if(identical(char, 'm')){
    mstringleft <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                          x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/10)))*jitterfactor),
                          lB= 'char_mstringleft') %>% 
      mutate(x=x-10)
    mstringright <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 20), factor = 15*jitterfactor), 
                           x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 20))), factor = (1-(1-(1/10)))*jitterfactor),
                           lB= 'char_mstringleft') %>% 
      mutate(x=x+10)
    msmallleft <- tibble(y=jitter(rep(seq(ylims[2], median(ylims), -0.3), 35), factor = 10*jitterfactor), 
                         x=jitter(rep(seq(anker-10, anker, 0.3), 35), factor = (40-index*2)*jitterfactor),
                         lB='char_aright')
    
    msmallright <- tibble(y=jitter(rep(seq(median(ylims), ylims[2], 0.3), 35), factor = 10*jitterfactor), 
                          x=jitter(rep(seq(anker, anker+10, 0.3), 35), factor = (40-index*2)*jitterfactor),
                          lB='char_aleft') 
    return(rbind(mstringleft, mstringright, msmallleft, msmallright))
  }
  if(identical(char, 'o')){
    return(sapply(circleFun(c(anker,median(ylims)),20, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
             as_tibble %>% 
             mutate(lB='char_o'))
  }
  if(identical(char, 'e')){
    estring <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 15), factor = 15*jitterfactor), 
                      x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 15))), factor = (1-(1-(1/index)))*jitterfactor),
                      lB= 'char_estring')
    ebiglower <- tibble(y=jitter(rep(min(ylims), length(rep(seq(anker, anker+20, 0.2), 5))), factor = (1-(1-(1/index)))*jitterfactor), 
                        x=jitter(rep(seq(anker, anker+20, 0.2), 5), factor = 1*jitterfactor),
                        lB= 'char_ebiglower')
    ebigupper <- tibble(y=jitter(rep(min(ylims), length(rep(seq(anker, anker+20, 0.2), 5))), factor = (1-(1-(1/index)))*jitterfactor), 
                        x=jitter(rep(seq(anker, anker+20, 0.2), 5), factor = 1*jitterfactor),
                        lB= 'char_ebigupepr') %>% 
      mutate(y=y+(ylims[2]-ylims[1]))
    esmall <- tibble(y=jitter(rep(min(ylims), length(rep(seq(anker, anker+10, 0.2), 5))), factor = (1-(1-(1/index)))*jitterfactor), 
                     x=jitter(rep(seq(anker, anker+10, 0.2), 5), factor = 1*jitterfactor),
                     lB= 'char_esmall') %>% 
      mutate(y=y+(ylims[2]-ylims[1])/2)
    return(rbind(estring, ebiglower, ebigupper, esmall) %>% mutate(x=x-5))
  }
  if(identical(char, 'y')){
    ybig <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.1), 15), factor = 10*jitterfactor), 
                   x=jitter(rep(seq(anker-20, anker+10, 0.15), 15), factor = 10*jitterfactor),
                   lB='char_ybig')
    ysmall <- tibble(y=jitter(rep(seq(ylims[2], mean(ylims), -0.15), 10), factor = 10*jitterfactor), 
                     x=jitter(rep(seq(anker-20, anker, 0.3), 10), factor = 10*jitterfactor),
                     lB='char_ysmall') %>% 
      mutate(x=x-2)
    
    return(rbind(ybig, ysmall))
  }
  if(identical(char, 'r')){
    rstring <- tibble(y=jitter(rep(seq(ylims[1], ylims[2], 0.2), 15), factor = 15*jitterfactor), 
                      x=jitter(rep(anker, length(rep(seq(ylims[1], ylims[2], 0.2), 15))), factor = (1-(1-(1/index)))*jitterfactor),
                      lB= 'char_rstring')
    rsmall <- tibble(y=jitter(rep(seq(mean(ylims), ylims[1], -0.15), 10), factor = 10*jitterfactor), 
                     x=jitter(rep(seq(anker, anker+20, 0.3), 10), factor = 10*jitterfactor),
                     lB='char_rsmall') %>% 
      mutate(x=x-2)
    rcirc <- sapply(circleFun(c(anker,median(ylims)),12, npoints = 101), function(x) jitter(rep(x, 50),factor=50*jitterfactor)) %>% 
      as_tibble %>% 
      mutate(lB='char_rcirc') %>% 
      filter(x>anker) %>% 
      mutate(x=x+5, y=y+4)
    return(rbind(rstring, rsmall, rcirc))
  }
  
}

l <- lapply(1:max(df$index), function(x) chars2data(char=df$chars[x], 
                                                    anker=df$ankers[x], 
                                                    ylims = ylims,
                                                    index=df$index[x],
                                                    jitterfactor=2)) %>% 
              do.call(rbind, .)

rndm <- rndm <- sapply(tibble(y=rnorm(1000, mean = mean(ylims), sd = 10), 
                              x=rnorm(1000, mean = mean(c(min(l$x), max(l$x))), sd = 100)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_rndm')
rndm2 <- rbind(l[sample(nrow(l), 10000),],
               l[sample(nrow(l), 10000),],
               l[sample(nrow(l), 10000),])

rndm3 <- rbind(l[sample(nrow(l), 10000),],
               l[sample(nrow(l), 10000),],
               l[sample(nrow(l), 10000),])

rndm4 <- sapply(tibble(y=rnorm(10000, mean = mean(ylims), sd = 20), 
                              x=rnorm(10000, mean = 200, sd = 100)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_rndm')

rndm5 <- sapply(tibble(y=rnorm(10000, mean = mean(ylims), sd = 20), 
                               x=rnorm(10000, mean = 600, sd = 100)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_rndm')

l <- rbind(l, rndm, rndm2, rndm3)

f <- ggplot(l) +
  # stat_density_2d(aes(x=x, y=y, fill = ..density..), geom = "raster", contour=F) +
  # geom_point(aes(x=x, y=y), alpha=0.008) +
  stat_bin_hex(aes(x=x, y=y), bins =70) +
  scale_x_continuous(expand = c(0.2,0.2)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_fill_viridis(option='inferno', guide=F, 
                     rescaler = function(x, to = c(0, 1), from = NULL){
                       ifelse(x<600, 
                              scales::rescale(x,
                                              to = to,
                                              from = c(min(x, na.rm = TRUE), 600)),
                              1)}) +
  theme(
    panel.background = element_rect(fill='black'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) 
f
ggsave(filename='gencon3.png', f, width = 15, height=5)
