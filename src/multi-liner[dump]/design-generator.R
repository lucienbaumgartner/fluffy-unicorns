library(extrafont)
library(ggplot2)
library(dplyr)
library(reshape2)
library(viridis)

font_import(pattern = 'Codystar-Regular')

fonts()

setwd('~/ddl-git/designs')

# banner
df <- tibble(coordx=c(50), coordy=c(25), txt=c('Digital Democracy Lab'))
g <- ggplot(df) +
  geom_text(aes(x=coordx, y=coordy, label=txt), family='Codystar-Light', size=30, color = 'white') +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='black'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggsave('twitter-banner-bw.png', g, width=25, height = 8)

# logo
df <- tibble(coordx=c(50), coordy=c(50), txt=c('DDL'))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(50,50),50,npoints = 101)
g <- ggplot(df) +
  geom_text(aes(x=coordx, y=coordy, label=txt), family='Codystar-Light', size=40, color = 'black') +
  geom_point(data=dat[seq(1, nrow(dat), 2),], aes(x=x, y=y), size=1, color='black') +
  geom_point(data=dat[seq(2, nrow(dat), 2),], aes(x=x, y=y), size=2, color='black') +
  coord_equal() + 
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='white'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
g
ggsave('twitter-logo-bw.png', width = 10, height=10)

set.seed(123)

rndm <- sapply(tibble(y=rnorm(5000, mean = 50, sd = 20), 
              x=rnorm(5000, mean = 50, sd = 20)), function(x) jitter(rep(x, 5), factor=50)) %>% 
  as_tibble %>% 
   mutate(lB='char_rndm')

char_i <- tibble(y=jitter(rep(seq(45, 55, 0.2), 5), factor = 10), 
       x= jitter(rep(25, length(rep(seq(45, 55, 0.2), 5))), factor = 1),
       lB= 'char_i')

char_dg <- sapply(circleFun(c(10,50),10, npoints = 51), function(x) jitter(rep(x, 30),factor=50)) %>% 
  as_tibble %>% 
  mutate(lB='char_dg')

char_t <- tibble(y=jitter(rep(55, length(rep(seq(29, 35, 0.1), 5))), factor = 1),
                 x=jitter(rep(seq(29, 35, 0.1), 5), factor = 10),
                 lB='char_t')

core <- rbind(char_i,char_dg, char_t, rndm)
step1 <- core %>% 
  filter(lB=='char_i') %>% 
  mutate(char_i_dX=x-15,
         char_i1X=x-8,
         char_i_tX=x+7,
         char_i2X=x+2) %>% 
  melt(id.var=c('y', 'x', 'lB')) %>% 
  mutate(x=value, lB=variable) %>% 
  select(-c(value, variable)) %>% 
  rbind(core, .)

step2 <- step1 %>% 
  filter(lB=='char_dg') %>% 
  mutate(lB=ifelse(x>10, 'char_d_curve', 'char_g_curve'),
         x=ifelse(x<=10, x+14, x)) %>% 
  rbind(step1, .)

step3 <- step2 %>% 
  filter(lB=='char_i', y<50) %>% 
  mutate(lB='char_gstring',
         x=x-1) %>% 
  rbind(step2, .)
  

step3 %>% 
  filter(!lB %in% c('char_i', 'char_dg')) %>% 
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
  guides(fill=FALSE)
  
step3 %>% 
  filter(!lB %in% c('char_i', 'char_dg')) %>% 
  ggplot(.) +
  stat_density_2d(aes(x=x, y=y, fill=..level..), geom = 'polygon', alpha=0.2, color='white') +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
 
  theme(
    panel.background = element_rect(fill='black')
  )

  
  
  

step3 <- step2 %>% 
  filter(lB=='char_i', y<50) %>% 
  mutate(lB='char_gstring',
         x=x-1) %>% 
  rbind(step2, .)
  vec3 <- rep(seq(45, 55, 0.1), 5)
a_y <- jitter(vec3, factor = 10)
a_x <- jitter(rep(seq(35, 39, ((39-35)/(length(vec3)-1)))), factor = 1)
a <- tibble(y=a_y, x=a_x)
length(dat$x)
length(i_y)==length(i_x)
ggplot() +
  geom_point(data=random_stuff, aes(x=i_x, y=i_y), alpha = 0.2, color='white')+
  geom_point(data=dr, aes(x=i_x-8, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x-15, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x+7, y=i_y), alpha = 0.2, color='white') +
  # geom_point(data=a, aes(x=x, y=y), alpha = 0.2, color='blue') +
  geom_point(data=t_string, aes(x=t_x, y=t_y), alpha = 0.2, color='white') +
  geom_point(data=dr[dr$i_y<50,], aes(x=i_x-1, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x+2, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dat[dat$x>10,], aes(x=x, y=y), alpha = 0.1, color='white') +
  geom_point(data=dat[dat$x<10,], aes(x=x+14, y=y), alpha = 0.1, color='white')+
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='black')
  )

ggplot() +
  geom_point(data=random_stuff, aes(x=i_x, y=i_y), alpha = 0.2, color='white')+
  geom_point(data=dr, aes(x=i_x-8, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x-15, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x+7, y=i_y), alpha = 0.2, color='white') +
  # geom_point(data=a, aes(x=x, y=y), alpha = 0.2, color='blue') +
  geom_point(data=t_string, aes(x=t_x, y=t_y), alpha = 0.2, color='white') +
  geom_point(data=dr[dr$i_y<50,], aes(x=i_x-1, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dr, aes(x=i_x+2, y=i_y), alpha = 0.2, color='white') +
  geom_point(data=dat[dat$x>10,], aes(x=x, y=y), alpha = 0.1, color='white') +
  geom_point(data=dat[dat$x<10,], aes(x=x+14, y=y), alpha = 0.1, color='white')+
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='black')
  )

