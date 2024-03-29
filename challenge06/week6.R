# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge06/data.csv')

# Generate a random walk for the ripped effect
RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)}

dt <- tibble(Year=seq(3542147*.303, -3e6, length.out=150),
             pt = seq(4.02, 4.1, length.out=150),
             high=pt+rnorm(150,0,.01))


# Color palette and labels
desert <- '#E1D6C8'
rd <- '#9A0C27'
bk <- '#0D0E0E'
yl <- '#F7B500'
y <- '#fbdb7a'
br <- '#9D714A'#
d <- '#221710'


data |> add_row(
  Year = c(1840,1800,1790),
  Mulattoes = c(1113063/4,0,0),
  Negroes = c(3542147*.7, 3542147*.3, 3542147*.3)
) |> mutate(
  Mulattoes = if_else(Year==1860, 1113063/2, Mulattoes)/2,#*1.5,
  mul_ = Mulattoes,
  Limit = - Mulattoes,
  Lim_ = -mul_,
  Year = c(1:4,4.1),
  ab = mul_+Mulattoes,
) |> 
  pivot_longer(cols = Negroes:Lim_) |> 
  mutate(
    #Year = factor(Year, levels = c('1890','1860','1840','1800')),
    name = factor(name, levels = c('Negroes', 'mul_', 'Mulattoes', 'Lim_','Limit'))
    ) -> data_plot

data_plot |> 
  ggplot(aes(Year, value))+
  geom_area(aes(fill = name))+
  coord_flip()+
  scale_x_continuous(expand = expansion(add = c(.1,.3)),)+ 
                      #labels = c('1890','1860','1840','1800'))+
  scale_y_reverse(expand = expansion(add = c(0,0)))+
  scale_fill_manual(values = c(bk,d,br,y,yl))+
  geom_ribbon(data=dt,aes(high,Year,xmin=high),xmax=Inf,fill=desert)+
  geom_line(data=dt,aes(x=high, y=Year), col= bk, orientation = "y", alpha = .5)+
  geom_segment(data=data_plot |> slice(1:20),size=.2,
               aes(x = Year, xend = Year, y = -Inf,yend = value+ab), col = 'white')+
  geom_segment(data=data_plot |> slice(1:20),size=.2,
               aes(x = Year, xend = Year, y = -Inf,yend = -ab), col = bk)+
  labs(title = 'The Amalgamation of the White and Black elements of the population\nin the United States.',
       subtitle = 'Amalgamation des elements blancs et noirs parmi la population Americaine.\n
       Done by Atlanta University, reproduice by Datadoulla.')+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #title = element_markdown(),
    panel.background = element_rect(fill = desert),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = desert),
    #text = element_text(family = 'mono'),
    plot.title = element_text(hjust = .5#, vjust = -4, face = 'bold', #size = 10, 
                              ),
    plot.subtitle = element_text(hjust = .5, vjust = -3), 
    legend.position = 'None', 
    plot.margin = margin(.5,0,0,0,'cm')
  )


  
  #geom_vline(xintercept = 1113063/2)+
  #geom_line(aes(x = Mulattoes, group=1))+
  #geom_line(aes(x = Negroes, group=1))+
  #scale_x_reverse(expand = expansion(mult = c(0,.5)))


