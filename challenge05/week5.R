# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge05/data.csv')


# Color palette and labels
desert <- '#E1D6C8'
rd <- '#9A0C27'
bk <- '#0D0E0E'
yl <- '#F7B500'
br <- '#432B26'

data |> 
  mutate(prt = str_c(Percentage,'%'),
         cum = c((16+40+44/2),(16+40/2),16/2),
         label = str_c("<span style = 'font-family:mono'>","**",
                       str_to_upper(Category),".","**","</span>"),
         sublabel = c(
           "<span style = 'font-family:mono'>IE. FULL-BLOODED
           <br>NEGROES.</span>",
           "<span style = 'font-family:mono'>IE. PERSONS WITH
           <br>SOME WHITE BLOOD<br>OR DESCENDANTS
           <br>OF LIGHT COLORED<br>AFRICANS</span>.",
           "<span style = 'font-family:mono'>IE. PERSONS WITH
           <br>MORE WHITE THAN<br>NEGRO BLOOD.</span>"
         ),
         orientation = rep("upright",3),
         hjust = rep(0,3),
         vjust = rep(1,3)) |> 
  ggplot(aes(1,Percentage, #label= prt,
             orientation = orientation, hjust = hjust, vjust = vjust))+
  geom_col(aes(fill=Category), width = .55)+
  geom_text(aes(1, y=(16+40+44/2), label = '44%'), color = 'white', size=5,
            hjust = .5, vjust = .5, fontface = 'bold', family = 'mono')+
  geom_text(aes(1, y=(16+40/2), label = '40%'), color = rd, size=5,
            hjust = .5, vjust = .5, fontface = 'bold', family = 'mono')+
  geom_text(aes(1, y=(16/2), label = '16%'), color = bk, size=5.5,
            hjust = .5, vjust = .5, fontface = 'bold', family = 'mono')+
  geom_richtext(aes(0.3, cum+.37*Percentage, label = label), size=5, 
                fill = NA, label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt"))+
  geom_richtext(aes(0.35, cum+.37*Percentage-4, label = sublabel), 
                size=3.5, fill = NA, label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt"))+
  scale_fill_manual(values = c(bk, br, yl))+
  coord_cartesian(xlim = c(.1, 1.5))+
  labs(title = 'RACE AMALGAMATION IN GEORGIA.',
       subtitle = 'BASED ON A STUDY OF 40.000 INDIVIDUALS OF NEGRO DESCENT.')+
  theme(
        legend.position = "none",
        panel.background = element_rect(fill = desert),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = desert),
        text = element_text(family = 'mono'),
        plot.title = element_text(hjust = .5, face = 'bold', size = 20, 
                                  vjust = -2),
        plot.subtitle = element_text(hjust = .5, size= 12, vjust = -4), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()
        ) -> p 

# Saving  ---------------------------------------------------
ggsave('challenge05/plate_w5.png', p ,width = 6.8, height = 8, limitsize = F)
