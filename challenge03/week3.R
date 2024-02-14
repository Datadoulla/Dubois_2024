# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext, magrittr)


# Data wrangling  ---------------------------------------------------
data <- read.csv('./challenge03/data.csv') %>% add_row(X1874=1874,X338769.000=338769, .before = 1)

data %<>% mutate(n=1:25, X1874 = if_else(n==12,1886,X1874),
                 txt = if_else(n==25,1062223,X338769.000)) %>% select(-n)

# Color palette and labels
desert <- '#E1D6C8'
black <- '#282422'
red <- c('#CF2C49', '#E62F4D', '#E2334F')


# Plot ---------------------------------------------------
data %>% ggplot(aes(X338769.000,factor(X1874), 
                    label=format(txt, big.mark='.', decimal.mark = ',')))+
  geom_col(fill = '#E2334F', width = .5,orientation = 'y')+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = expansion(mult = c(0,0)))+
  labs(title = 'ACRES OF LAND OWENED BY NEGROES \nIN GEORGIA.')+
  geom_text(data = data %>% slice(1,25),
    aes(X338769.000/2),size=4.5, family = 'mono', fontface = "bold")+
  theme(
    text = element_text(family = 'mono'),
    plot.title = element_text(hjust = .5,size = 18, face = 'bold'),
    plot.margin = margin(.5,.9,.5,.9,unit = 'cm'),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = desert),
    panel.grid = element_blank()
    ) -> p


# Saving  ---------------------------------------------------
ggsave('challenge03/plate_w3.png', p ,width = 6.8, height = 8, limitsize = F)
