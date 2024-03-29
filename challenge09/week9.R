# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext, patchwork)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge09/data.csv')

# Color palette and labels
desert <- '#E1D6C8'

data |> 
  mutate(
    pt = if_else(Year == 1870, 11, Free)
  ) |> 
  ggplot(aes(Year, Free))+
  geom_line()+
  #ylim(0,100)+
  scale_y_reverse(expand = expansion(add = c(0,0)))+
  geom_area(fill = '#015528')+
  geom_ribbon(aes(ymin = Free, ymax = Inf), fill = 'black')+
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = Inf), 
               col = 'black', size = .2)+
  geom_text(aes(y = (pt - 1.8),label = str_c(Free,'%')), 
            size = 5, fontface = 'bold', family = 'mono')+
  scale_x_continuous(n.breaks = 9, position = 'top')+
  labs(title = 'PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NOIRS LIBRES ET DES ESCLAVES EN AMÉRIQUE.',
       subtitle = 'Done by Atlanta University, reproduice by Datadoulla.')+
  annotate("text", x = 1830, y = 4, label = "FREE - LIBRE", 
           size = 7, fontface = 'bold', family = 'mono')+
  annotate("text", x = 1830, y = 45, label = "SLAVES\nESCLAVES", 
           size = 8, fontface = 'bold', family = 'mono', color = desert)+
  theme(
  axis.title = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_text(size = 17, color = 'black'),
  axis.ticks = element_blank(),
  panel.background = element_rect(fill = desert),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = desert),
  text = element_text(family = 'mono',face = 'bold'),
  plot.title = element_text(hjust = .5, face = 'bold', vjust = 20, size = 15, ),
  plot.subtitle = element_text(hjust = .5, vjust = 16, face = 'bold'), 
  plot.margin = margin(3, 0, .6, 0,'cm')) -> p


# Saving  ---------------------------------------------------
ggsave('challenge09/plate_w9.png', p ,width = 6.8, height = 8, limitsize = F)
