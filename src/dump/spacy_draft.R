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

rndm <- sapply(tibble(y=rnorm(3000, mean = 50, sd = 20), 
                      x=rnorm(3000, mean = 50, sd = 20)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_rndm')

char_i <- tibble(y=jitter(rep(seq(40, 60, 0.2), 5), factor = 10), 
                 x= jitter(rep(50, length(rep(seq(40, 60, 0.2), 5))), factor = 1),
                 lB= 'char_i')

char_l_big <- tibble(y=jitter(rep(seq(10, 30, 0.1), 5), factor = 10), 
                     x= jitter(rep(30, length(rep(seq(10, 30, 0.1), 5))), factor = 1),
                     lB= 'char_lbig') 

char_l_small <- tibble(y=jitter(rep(10, length(rep(seq(30, 40, 0.1), 5))), factor = 2), 
                 x=jitter(rep(seq(30, 40, 0.1), 5), factor = 10),
                 lB= 'char_l')

char_dg <- sapply(circleFun(c(50,50),20, npoints = 51), function(x) jitter(rep(x, 30),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg') 


core <- rbind(rndm, char_i, char_dg, char_l_small, char_l_big) 

step1 <- core %>% 
  filter(lB=='char_dg') %>% 
  mutate(lB=ifelse(x>50, 'char_d_curve', 'char_g_curve')) %>% 
  rbind(core, .)

step2 <- step1 %>% 
  filter(lB=='char_i') %>% 
  mutate(char_i1X=x, # first i
         char_i_eX=x, # e
         char_i_d1X=x-20, # first d
         char_i_d2X=x-20, # second d
         char_i_lX=x-20, # l
         ) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  mutate(char_i1Y=y+30, # first i
         char_i_eY=y, # e
         char_i_d1Y=y+30, # first d
         char_i_d2Y=y # second d, # l
         ) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(y=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  rbind(step1, .) %>% 
  filter(!lB=='char_i')  



step3 <- step2 %>% 
  filter(lB=='char_d_curve') %>% 
  mutate(char_d1X=x-20,
         char_d2X=x-20) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  mutate(char_d1Y=y+30,
         char_d2Y=y) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(y=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  rbind(.,.) %>% 
  rbind(step2, .) %>% 
  filter(!lB%in%c('char_dg', 'char_d_curve')) 

step4 <- step3 %>% 
  filter(lB=='char_l') %>% 
  mutate(char_l_elowX=x+20,
         char_l_emidX=x+20,
         char_l_eupX=x+20) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  mutate(char_l_elowY=y+30,
         char_l_emidY=y+40,
         char_l_eupY=y+50) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>%
  mutate(y=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  rbind(step3, .) 

step5 <- step4 %>% 
  filter(lB=='char_g_curve') %>% 
  mutate(x=x+20,
         y=y+30) %>% 
  rbind(step4 %>% filter(!lB=='char_g_curve'), .)
  
g <- step5 %>%  
  ggplot(.) +
  stat_density_2d(aes(x=x, y=y, fill=..level..), geom = 'polygon', alpha=0.5) +
  # geom_density_2d(aes(x=x, y=y, fill=..level..), alpha=0.5, colour='white') +
  geom_point(aes(x=x, y=y), alpha=0.05, color='white') +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_fill_viridis() +
  theme(
    panel.background = element_rect(fill='black'),
    panel.grid = element_line(colour = 'gray40'),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  coord_equal()+
  guides(fill=FALSE)

g

#ggsave('ddl-glowy3.png', g, width=8, height=8)