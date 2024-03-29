# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext, patchwork)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge08/data.csv')

dt_sf <- data |> slice(1)

dt_ot <- data |> 
  slice(-1) |> 
  rename(
    Owners = Slave,
    Tenants = Free
  ) |> slice(2)

# Color palette and labels
desert <- '#E1D6C8'

textin <- "IN 1890 NEARLY ONE FIFTH OF THEM OWNED THEIR OWN HOMES AND FARMS.\nTHIS ADVANCE WAS ACCOMPLISHED ENTIRELY WITHOUT STATE AID, AND IN\nTHE FACE OF PROSCRIPTIVE LAWS\n\nEN 1880 ENVIRON UN CINQUEME ÉTAIENT PROPRIETAIRES DE LEURS HABITA-\nTIONS ET DE LEURS FERMES, CE PROGRES S'EST ACCOMPLI SANS SECOURS\nAUCUN DE L'ETAT ET EN PRÉSENCE DE LOIS DE FAVORABLES."

textin2 <- "IN 1883 NEARLY 90% OF THE BLACKS WERE SLAVES.\n\nEN 1880 ENVIRON 90% DES NEGRES ÉTAIENT ESCLAVES."

texttitle = 'THE RISE OF THE NEGROES FROM SLAVERY TO FREEDOM IN ONE GENERATION.\n
\nPROGRES GRADUEL DES NEGRES DE L\'ESCLAVAGE À LA LIBERTÉ EN UNE GÉNÉRATION.\n'

dt_sf |> 
   pivot_longer(2:3) |> 
   mutate(value = parse_double(value),
          cum = cumsum(value) - value/2,
          lb = str_c(value,'%'),
          lbs = c("SLAVES\nESCLAVES",
                  "FREE LABORERS\nOUVRIERS LIBRES"),
          #label = textin,
          #sublabel = textin2,
          hjust = 0,
   ) |> 
   ggplot(aes(Year, value, fill = name,
              hjust = hjust, 
   ))+
   geom_col(width = .3)+
   scale_fill_manual(values = c('#015528','#0C0A05'))+
   scale_x_discrete(expand = expansion(add = c(.2, .4)))+
   scale_y_discrete(expand = expansion(mul = c(0, .3)))+
   geom_text(aes(y = 105, label = Year, hjust = .5), size = 7,fontface = 'bold', 
             family = 'mono', col = 'black')+
   geom_text(aes(y = cum, label = lbs, hjust = .5), size = 3.5,fontface = 'bold', 
             family = 'mono', col = 'red')+
   geom_text(aes(y = cum, label = lb, hjust = 2.5), size = 6,fontface = 'bold', 
             family = 'mono', col = 'red')+
   geom_text(aes(.8, 190, label = textin, family = 'mono', fontface = 'italic'), 
             size=3)+
   geom_text(aes(.85, 140, label = textin2,family = 'mono', fontface = 'italic'), 
             size=3)+
   theme(
     legend.position = "none",
     axis.title = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     panel.background = element_rect(fill = desert),
     panel.grid = element_blank(),
     plot.background = element_rect(fill = desert, colour = desert)
   ) -> pleft

dt_ot |> 
  pivot_longer(2:3) |> 
  mutate(value = parse_double(value),
         cum = 100 - (cumsum(value) - value/2),
         lb = str_c(value,'%'),
         lbs = c("PEASANT PROPRIETORS\nPAYSAMS PROPRIETAIRES",
                 "TENANTS\nMETAYERS")) |> 
  ggplot(aes(Year, value, fill = name))+
  geom_col(width = .3)+
  scale_fill_manual(values = c('#B9042B', '#015528'))+
  #geom_text(aes(y = cum, label = name))+
  geom_text(aes(y = 103, label = Year, hjust = .5), size = 7,fontface = 'bold', 
            family = 'mono', col = 'black')+
  geom_text(aes(y = cum - 4, label = lbs, hjust = .5), size = 4,fontface = 'bold', 
            family = 'mono', col = 'black')+
  geom_text(aes(y = cum + 3, label = lb, hjust = .5), size = 6,fontface = 'bold', 
            family = 'mono', col = 'black')+
  scale_x_discrete(expand = expansion(add = c(0, 0)))+
  scale_y_discrete(expand = expansion(mul = c(.3, .2)))+
  #scale_y_reverse()+
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = desert),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = desert, colour = desert), 
  ) -> pright

pleft + pright+
  plot_layout(guides = 'collect') +
  plot_layout(widths = c(2, 1))&
  plot_annotation(title = texttitle,
                  subtitle = 'Done by Atlanta University, reproduice by Datadoulla.') &
  theme(
    panel.background = element_rect(fill = desert),
    plot.background = element_rect(fill = desert),
    text = element_text(family = 'mono'),
    plot.title = element_text(hjust = .5, face = 'bold', size = 12,#, vjust = -4, face = 'bold', #size = 10, 
    ),
    plot.subtitle = element_text(hjust = .5, vjust = -3, face = 'bold'), 
    #plot.margin = margin(.5,0,0,0,'cm')
  ) -> p

# Saving  ---------------------------------------------------
#ggsave('challenge08/pright.png', pright,width = 7.4, height = 8.5, limitsize = F)
#ggsave('challenge08/pleft.png', pleft,width = 7.4, height = 8.5, limitsize = F)
ggsave('challenge08/plate_w8.svg', p, width = 7.4, height = 8.5, limitsize = F)


