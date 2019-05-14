setwd('~/fluffy-unicorns/')
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

center_around <- function(center=0) {
  function(x, to=NA, from=NA) {
    r <- max(abs(from-center))
    (x - (center-r)) / 2/r
  }
}

rndm <- sapply(tibble(y=rnorm(20000, mean = 50, sd = 50), 
                      x=rnorm(20000, mean = 50, sd = 50)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  rbind(., sapply(tibble(y=rnorm(10000, mean = 110, sd = 30), 
                      x=rnorm(10000, mean =110, sd = 20)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(10000, mean = 40, sd = 40), 
                         x=rnorm(10000, mean = 40, sd = 100)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(10000, mean = 60, sd = 20), 
                       x=rnorm(10000, mean = 200, sd = 15)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(10000, mean = 200, sd = 100), 
                         x=rnorm(10000, mean = 10, sd = 40)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 40, sd = 7), 
                         x=rnorm(1000, mean = 220, sd = 5)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 10, sd = 5), 
                         x=rnorm(1000, mean = 60, sd = 5)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(2000, mean = 50, sd = 5), 
                         x=rnorm(2000, mean = 10, sd = 10)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 30, sd = 3), 
                         x=rnorm(1000, mean = 20, sd = 4)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 100, sd = 50), 
                         x=rnorm(1000, mean = 200, sd = 30)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 10, sd = 20), 
                         x=rnorm(1000, mean = 300, sd = 30)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 250, sd = 30), 
                         x=rnorm(1000, mean = 50, sd = 30)), function(x) jitter(rep(x, 5), factor=50)))



g <- ggplot(rndm) +
  stat_bin_hex(aes(x=x, y=y), bins =55) +
  # stat_density_2d(aes(x=x, y=y, fill=..level..), geom = 'polygon', alpha=0.5) +
  # geom_density_2d(aes(x=x, y=y, fill=..level..), alpha=0.5, colour='white') +
  # geom_point(aes(x=x, y=y), alpha=0.02, color='white') +
  scale_x_continuous(limits = c(0, 600), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 600), expand = c(0,0)) +
  # scale_fill_gradient(low = "#3E0200", high = "#EB0802", space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  scale_fill_gradientn(colors = c('#011A27', '#F0810F', '#E6DF44'), 
                       rescaler = center_around(2000)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  coord_equal()+
  guides(fill=FALSE)

g

ggsave('cornerstones-poster-left-bottomv2.png', g, height=15, width=15, bg='transparent')

######### right upper corner

rndm <- sapply(tibble(y=rnorm(20000, mean = 50, sd = 50), 
                      x=rnorm(20000, mean = 50, sd = 50)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 110, sd = 30), 
                         x=rnorm(1000, mean =110, sd = 20)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 40, sd = 40), 
                         x=rnorm(1000, mean = 40, sd = 100)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 60, sd = 20), 
                         x=rnorm(1000, mean = 200, sd = 15)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 200, sd = 100), 
                         x=rnorm(1000, mean = 10, sd = 40)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 40, sd = 7), 
                         x=rnorm(1000, mean = 220, sd = 5)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 10, sd = 5), 
                         x=rnorm(1000, mean = 60, sd = 5)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 50, sd = 5), 
                         x=rnorm(1000, mean = 10, sd = 10)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 30, sd = 3), 
                         x=rnorm(1000, mean = 20, sd = 4)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 100, sd = 50), 
                         x=rnorm(1000, mean = 200, sd = 30)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 10, sd = 20), 
                         x=rnorm(1000, mean = 300, sd = 30)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 250, sd = 30), 
                         x=rnorm(1000, mean = 50, sd = 30)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 300, sd = 30), 
                         x=rnorm(1000, mean = 50, sd = 50)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 400, sd = 50), 
                         x=rnorm(1000, mean = 50, sd = 20)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 500, sd = 10), 
                         x=rnorm(1000, mean = 50, sd = 5)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 500, sd = 2), 
                         x=rnorm(1000, mean = 50, sd = 10)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 490, sd = 2), 
                         x=rnorm(1000, mean = 30, sd = 20)), function(x) jitter(rep(x, 5), factor=50)))%>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 150, sd = 30), 
                         x=rnorm(1000, mean = 100, sd = 20)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(50000, mean = 150, sd = 50), 
                         x=rnorm(50000, mean = 10, sd = 40)), function(x) jitter(rep(x, 5), factor=50))) %>% 
  rbind(., sapply(tibble(y=rnorm(1000, mean = 200, sd = 50), 
                         x=rnorm(1000, mean = 100, sd = 40)), function(x) jitter(rep(x, 5), factor=50)))






g <- ggplot(rndm) +
  stat_bin_hex(aes(x=x, y=y), bins =56) +
  # stat_density_2d(aes(x=x, y=y, fill=..level..), geom = 'polygon', alpha=0.5) +
  # geom_density_2d(aes(x=x, y=y, fill=..level..), alpha=0.5, colour='white') +
  # geom_point(aes(x=x, y=y), alpha=0.02, color='white') +
  scale_x_continuous(limits = c(0, 600), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 600), expand = c(0,0)) +
  # scale_fill_gradient(low = "#3E0200", high = "#EB0802", space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  scale_fill_gradientn(colors = c('#011A27', '#F0810F', '#E6DF44'), 
                       rescaler = center_around(2000)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  coord_equal()+
  guides(fill=FALSE)

g

ggsave('cornerstones-poster-right-bottomv2.png', g, height=15, width=15, bg='transparent')
