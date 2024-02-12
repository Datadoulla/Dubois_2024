# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge02/data.csv')

# Generate a random walk for the ripped effect
RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)}

dt <- tibble(Year=seq(1790,1870.1,length.out=3000),
             high=3-RW(3000,0,0,.00001))

saveRDS(dt,'challenge02/zigzag.RDS')


# Color palette and labels
desert <- '#E1D6C8'
black <- '#282422'
red <- '#CF2C49'
yy <- c('','1%','2%','3%')
xx <- c('1.3%',c(data %>% slice(2:8) %>% 
                   select(Free) %>% deframe()),'100%')

# Plot ---------------------------------------------------
data %>% 
  add_row(Year=1863, Slave=3, Free=3.1, .before = 9) %>% 
  mutate(Free = ifelse(Year==1870,3.1,Free)) %>% 
  ggplot(aes(Year,Free))+
  geom_area(data=dt,aes(Year,high),fill=black)+
  geom_line(col=desert)+
  geom_area(fill=red)+
  geom_ribbon(data=dt,aes(Year,.5*high,ymin=high),ymax=-Inf,fill=desert)+
  geom_line(data=dt,aes(Year,high),col=black)+
  geom_segment(data= data %>% slice(2:8),
               aes(x=Year,xend=Year,y=0,yend=3), col=desert,size=.4)+
  coord_flip(ylim = c(3,0))+
  scale_x_reverse(n.breaks = 9,labels = data$Year,
                     sec.axis = sec_axis(~., breaks = data$Year, labels = xx),
                     expand = expansion(add = 0)
                  )+
  scale_y_reverse(n.breaks = 4,position = 'right', labels=yy)+
  labs(title = 'SLAVES AND FREE NEGROES.', 
       subtitle  = 'PERCENT OF \nFREE NEGROES')+
  theme(panel.background = element_rect(fill = desert),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = desert),
        text = element_text(family = 'mono'),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = .5, face = 'bold', vjust = 3, size = 20),
        plot.subtitle = element_text(size= 9,hjust = 1.65, vjust = -1),
        plot.margin = margin(1.5, 4, 2, 4, "cm"), 
        axis.ticks.length.y.left = unit(.7, "cm"),
        axis.ticks.length.y.right = unit(.7, "cm"),
        axis.ticks.y= element_blank(),
        ) -> p

# Saving  ---------------------------------------------------
ggsave('challenge02/plate_w2.png', p ,width = 6.8, height = 8, limitsize = F)
