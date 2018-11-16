rm(list=ls())

setwd('~/ddl-git/designs')
set.seed(123)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

rndm <- sapply(tibble(y=rnorm(20000, mean = 50, sd = 20), 
                      x=rnorm(20000, mean = 50, sd = 20)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_rndm')

char_i <- tibble(y=jitter(rep(seq(40, 60, 0.2), 20), factor = 10), 
                 x= jitter(rep(50, length(rep(seq(40, 60, 0.2), 20))), factor = 1),
                 lB= 'char_i')

char_lsmall <- tibble(y=jitter(rep(10, length(rep(seq(20, 30, 0.2), 20))), factor = 5), 
                      x=jitter(rep(seq(20, 30, 0.2), 20), factor = 1),
                      lB= 'char_l')

char_estr <- char_i %>% mutate(x=x-5)

char_lstr <- char_i %>% mutate(x=x-30, y=y-30)

char_d1str <- char_i %>% mutate(x=x-30, y=y+30)

char_d2str <- char_i %>% mutate(x=x-30)

char_ii <- char_i %>% mutate(y=y+30)

char_dg <- sapply(circleFun(c(50,50),20, npoints = 101), function(x) jitter(rep(x, 50),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg') 

char_d1circ <- char_dg %>% filter(x>50) %>% mutate(x=x-30, y=y+30)

char_d2circ <- char_dg %>% filter(x>50) %>% mutate(x=x-30)

char_gcirc <- char_dg %>% filter(x<50) %>% mutate(x=x+30, y=y+30)

char_gstr <- char_i %>% filter(y<50) %>% mutate(x=x+30, y=y+30)

char_esmall <- char_lsmall %>% filter(x<25) %>% mutate(x=x+25, y=y+40)

char_ebig1 <- char_lsmall %>% mutate(x=x+25, y=y+30)

char_ebig2 <- char_lsmall %>% mutate(x=x+25, y=y+50)

char_mstr1 <- char_i %>% mutate(x=x+20)
char_mstr2 <- char_i %>% mutate(x=x+30)

char_bstr <- char_i %>% mutate(x=x+23, y=y-30)
char_bcurvesmall <- sapply(circleFun(c(50,50), 8, npoints = 33), function(x) jitter(rep(x, 50),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg') %>% 
  filter(x>50) %>% 
  mutate(x=x+25, y=y-23)
char_bcurvebig <- sapply(circleFun(c(50,50),12, npoints = 51), function(x) jitter(rep(x, 50),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg') %>% 
  filter(x>50)%>% 
  mutate(x=x+25, y=y-34)

char_mleft <- tibble(y=jitter(rep(seq(60, 50, -0.3), 20), factor = 10), 
                     x=jitter(rep(seq(70, 75, 0.15), 20), factor = 40),
                     lB='char_a')

char_mright <- tibble(y=jitter(rep(seq(50, 60, 0.3), 20), factor = 10), 
                      x=jitter(rep(seq(75, 80, 0.15), 20), factor = 40),
                      lB='char_a')

char_aleft <- tibble(y=jitter(rep(seq(10, 30, 0.3), 35), factor = 10), 
                     x=jitter(rep(seq(40, 50, 0.15), 35), factor = 40),
                     lB='char_a') 
char_aright <- tibble(y=jitter(rep(seq(30, 10, -0.3), 35), factor = 10), 
                      x=jitter(rep(seq(50, 60, 0.15), 35), factor = 40),
                      lB='char_a')

core <- rbind(char_estr, 
              char_d2str, 
              char_d1str, 
              char_lstr, 
              char_lsmall, 
              char_d1circ,
              char_d2circ,
              char_gcirc,
              char_gstr,
              char_ii, 
              char_esmall,
              char_ebig1,
              char_ebig2,
              char_mstr1,
              char_mstr2,
              char_bstr,
              char_bcurvesmall,
              char_bcurvebig,
              char_aleft,
              char_aright,
              char_mleft,
              char_mright,
              rndm)
g <- core %>%  
  ggplot(.) +
  stat_bin_hex(aes(x=x, y=y), bins =70) +
  # stat_density_2d(aes(x=x, y=y, fill=..level..), geom = 'polygon', alpha=0.5) +
  # geom_density_2d(aes(x=x, y=y, fill=..level..), alpha=0.5, colour='white') +
  # geom_point(aes(x=x, y=y), alpha=0.02, color='white') +
  scale_x_continuous(limits = c(-20, 120), expand = c(0,0)) +
  scale_y_continuous(limits = c(-20, 120), expand = c(0,0)) +
  scale_fill_gradient(low = "#3E0200", high = "#EB0802", space = "Lab",
                      na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  theme(
    panel.background = element_rect(fill='white'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  coord_equal()+
  guides(fill=FALSE)

g
