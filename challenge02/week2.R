pacman::p_load(tidyverse)

data <- read_csv('challenge02/data.csv')

dt <- tibble(Year=seq(1790,1870.1,length.out=300),
             high=3-rnorm(300)/40)

desert <- '#E1D6C8'
black <- '#282422'
red <- '#CF2C49'#'#C2505C'
yy <- c('','1%','2%','3%')
xx <- c('1.3%',c(data %>% slice(2:8) %>% select(Free) %>% deframe()),'100%')


data %>% 
  add_row(Year=1863, Slave=3, Free=3.1, .before = 9) %>% 
  mutate(Free = ifelse(Year==1870,3.1,Free)) %>% 
  #mutate(high=3-rnorm(10,sd = .01)) %>% 
  #head(8) %>% 
  ggplot(aes(Year,Free))+
  #geom_line(aes(y = Slave))+
  geom_area(data=dt,aes(Year,high),fill=black)+
  geom_line(col=desert)+
  geom_area(fill=red)+
  #geom_vline(col='white',xintercept = 1870, col='#EBD5A6')+
  geom_ribbon(data=dt,aes(Year,high,ymin=high),ymax=-Inf,fill=desert)+
  geom_line(data=dt,aes(Year,high),col=black)+
  geom_segment(data= data %>% slice(2:8),
               aes(x=Year,xend=Year,y=0,yend=3), col=desert,size=.4)+
  coord_flip(ylim = c(3,0))+
  #theme_void()+
  #scale_y_discrete(labels=yy)+
  scale_x_reverse(n.breaks = 9,labels = data$Year,
                     sec.axis = sec_axis(~.,breaks = data$Year,labels = xx),
                     expand = c(0,0))+
  scale_y_reverse(position = 'right',labels=yy)+
  labs(title = 'SLAVES AND FREE NEGROES .', 
       subtitle  = 'PERCENT OF  \nFREE NEGROES')+
  #coord_cartesian(ylim = c(0,3), xlim = c(1790,1870))+
  theme(panel.background = element_rect(fill = desert),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = desert),
        axis.text.y = element_text(size = 12), 
        axis.text.y.left = element_text(hjust = -10),
        plot.title = element_text(hjust = .5,face = 'bold',vjust = 5,size = 18),
        plot.subtitle = element_text(size=7,hjust = 1.3, vjust = -7),
        #plot.margin = margin(2,2,2,2, unit = "pt"),
        plot.margin = margin(1.5, 5, 1.5, 5, "cm"), 
        axis.ticks.y= element_blank())

        