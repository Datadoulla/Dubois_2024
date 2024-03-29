# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext)

# Data wrangling  ---------------------------------------------------
data <- read.csv('challenge07/data.csv',header = F, col.names = c('country', 'count')) #|> view()

# Color palette and labels
desert <- '#D6C9BC'
pal <- c('#2F8558' ,'#BD032A')

# Plot ---------------------------------------------------
data |> 
  mutate(dum = if_else(country == 'Negroes, U.S.A.',1,0) |> 
           factor()) |> 
  ggplot(aes(count, reorder(country, count, identity), fill = dum))+
  geom_col(col='#5b5b5b', width = .5)+
  scale_y_discrete(expand = expansion(add = c(1,2.5)))+
  scale_fill_manual(values = pal)+
  labs(title = 'Illiteracy of the American Negroes compared with that of other nations.',
       subtitle = 'Proportion d\'illettrées parmi les Negres Americains comprée à celle des autres nations.\n
       Done by Atlanta University, reproduice by Datadoulla.')+
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(hjust = 0, face = 'bold', size = 11),
    plot.caption = element_markdown(),
    # #title = element_markdown(),
    panel.background = element_rect(fill = desert),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = desert),
    text = element_text(family = 'mono'),
    plot.title = element_text(hjust = .5, vjust = -9, face = 'bold', size = 13,
    ),
    plot.subtitle = element_text(hjust = .5, vjust = -20, size = 10),
    legend.position = 'None',
    plot.margin = margin(.5,0,0,1,'cm'),
    plot.title.position = 'plot'
  ) -> p


# Saving  ---------------------------------------------------
ggsave('challenge07/plate_w7.png', p ,width = 8.5, height = 10, limitsize = F)

