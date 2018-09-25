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

char_l <- tibble(y=jitter(rep(41, length(rep(seq(70, 80, 0.2), 5))), factor = 2), 
                 x= jitter(rep(seq(70, 80, 0.2), 5), factor = 10),
                 lB= 'char_l')

char_dg <- sapply(circleFun(c(50,50),20, npoints = 51), function(x) jitter(rep(x, 30),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg') 


core <- rbind(rndm, char_i, char_dg, char_l) 

step1 <- core %>% 
  filter(lB=='char_dg') %>% 
  mutate(lB=ifelse(x>50, 'char_d_curve', 'char_g_curve')) %>% 
  rbind(core, .)

step2 <- step1 %>% 
  filter(lB=='char_i') %>% 
  mutate(char_i_d1X=x-20,
         char_i_d2X=x,
         char_i_lX=x+20) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  rbind(step1, .) %>% 
  filter(!lB=='char_i')  
  


step3 <- step2 %>% 
  filter(lB=='char_d_curve') %>% 
  mutate(char_d1=x-20,
         char_d2=x) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>%
  rbind(step2, .) %>% 
  filter(!lB%in%c('char_dg', 'char_g_curve', 'char_d_curve')) 
  

g <- step3 %>%  
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
ggsave('ddl-glowy3.png', g, width=8, height=8)
