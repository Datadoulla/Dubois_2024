# Packages loading ---------------------------------------------------
#require('pacman')
pacman::p_load(tidyverse, ggtext, patchwork)

# Data wrangling  ---------------------------------------------------
data <- read_csv('challenge10/data.csv')

data |> arrange(desc(Percentage)) |> 
  mutate(cum = cumsum(Percentage) - Percentage*.45,
         label_e = if_else(Occupation %in% c('Teachers', 'House Wives'),
                           str_c(Percentage, '%'), ''),
         label_c = if_else(!(Occupation %in% c('Teachers', 'House Wives')),
                           str_c(Percentage, '%'), ''),
         cum2 = cumsum(Percentage) - (Percentage / 2),
         x = 50,  
         y = seq(55, 60, 1),
         yt = seq(60,55,-1),
         t = x + c(.75, .4, 0, 0, .4, .75),
         lb = factor(Occupation, 
                     levels = c("Teachers", "Ministers", "Government Service", 
                                "Business", "Other Professions", "House Wives")),
         lbf = factor(c("PROFESSEURS ET INSTITUTEURS", "MERES DE FAMILE", 
                        "MINISTRES DE L'EVANGLE", "MEDECINS, AVOCATS ET ETUDIENTS",
                        "EMPLOYES DU GOUVERNMENT", "MARCHANDS"),
                      levels = c("PROFESSEURS ET INSTITUTEURS", "MINISTRES DE L'EVANGLE",
                                 "EMPLOYES DU GOUVERNMENT", "MARCHANDS", 
                                 "MEDECINS, AVOCATS ET ETUDIENTS", "MERES DE FAMILE"))) |> 
  identity() -> data_plot


# Color palette and labels
desert <- '#E1D6C8'

sub <- "THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT. 
IT HAS FIVE BUILDINGS, 60 ACRES OF CAMPUS, AND A LIBRARY OF 11,000 VOLUMES. IT AIMS TO RAISE
AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEIR MORE CAPABLE MEMBERS IN THE LIBERAL 
ARTS ACCORDING TO THE BEST STANDARDS OF THE DAY.
THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500,000.
L'UNIVERSITÉ A ACTUELLEMENT 20 PROFESSEURS ET INSTRUCTEURS ET 250 ÉTUDIANTS.
ELLE EST COMPOSÉE DE CINQ BATIMENTS, 60 ACRES (ENVIRON 26 HECTARES) DE TERRAIN SERVANT 
DE COUR ET DE CHAMP DE RECREATION ET D'UNE BIBLIOTHEQUE CONTENANT 11.000 VOLUMES.
SON BUT EST D'ÉLEVER ET DE CIVILISER LES FILS DES ESCLAVES NOIRS AFFRANCHIS EN DONNANT
AUX MIEUX DOUÉS UNE EDUCATION DANS LES ARTS LIBERAUX EN ACCORD AVEC LES IDÉES 
LES PLUS PROGRESSSTES DE L'ÉPOQUE. 
L'ACCOMPLISSEMENT DE CETTE OŒUVRE DEMANDE UNE DOTATION DE $500.000 (2.500.000 FRANCS)."

eng <- "THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 NEGRO STUDENTS. 

IT HAS GRADUATED 330 NEGROES AMONG WHOM ARE:"

fr <- "UNIVERSTE A ÉTÉ FONDÉE EN 1867. ELLE A DONNE L'INSTRUCTION A 6000 ÉTUDIANTS NOIRS. 

ELLE A DÉLIVRÉ DES DIPLOMES A 330 NOIRS DONT:"

entg <- "PREPARED AND EXECUTED BY
NEGRO STUDENTS UNDER THE
DIRECTION OF
ATLANTA UNIVERSITY.
ATLANTA, GA. 
UNITED STATES OF AMERICA."

entf <- "PRÉPARÉES ET EXECUTERS PAR
LES ÉTUDIANTS NOIRS SOUS
LA DRECTION DE L'UNVERSITÉ
D'ATLANTA.
ÉTAT DE GEORGIE.
ÉTATS LINES D'AMERIQUE."

title <- "A SERIES OF STATISTICAL CHARTS, ILLUSTRATING THE CONDITION OF 
THE DESCENDANTS OF FORMER AFRICAN SLAVES NOW RESIDENT IN 
THE UNITED STATES OF AMERICA."
subtitle <- "UNE SÉRIE DE CARTES ET DIAGRAMNES STATISTIQUES MOITRANT LA CONDITION
PRESENTE DES DESCENDANTS DES ANCIENS ESCLAVER AFRICAINS ACTUELLEMENT
ETABILIS DANS LES ETATS LINIS D'AMÉRIQUE."


data_plot |> 
  ggplot(aes(fill = reorder(Occupation, Percentage, identity), 
             x = 1, y = Percentage, label = label_c))+
  geom_col(col = 'black', size = .1, width = 20)+
  geom_text(aes(x = 8.5, y = cum), angle = -35,
            size = 3, fontface = 'bold', family = 'mono')+
  geom_text(aes(y = cum, label = label_e),
            size = 3.5, fontface = 'bold', family = 'mono')+
  geom_point(aes(t, y, col = lb), size = 4.5, col = 'black')+
  geom_point(aes(t, y, col = lb), size = 4)+
  geom_point(aes(t, yt - 49, col = lb), size = 4.5, col = 'black')+
  geom_point(aes(t, yt - 49, col = lb), size = 4)+
  geom_text(aes(x = t - 2, y, label = lb, hjust = 0), 
            family = 'mono', size = 2.7, fontface = 'italic')+
  geom_text(aes(x = t - 2, yt - 49, label = lbf, hjust = 1), 
            col = 'red', family = 'mono', size = 2.7, fontface = 'italic')+
  annotate('text', x = 35, y = 83.5, label = sub, 
            family = 'mono', size = 3.5, fontface = 'italic')+
  annotate('text', x = 20, y = 34.5, label = eng, hjust = .5,
           family = 'mono', size = 3.5, fontface = 'italic')+
  annotate('text', x = 17, y = 34.5, label = fr, hjust = .5,col = 'red',
           family = 'mono', size = 3.5, fontface = 'italic')+
  annotate('text', x = 50, y = 43.4, label = entg, hjust = .65,
           family = 'mono', size = 3.5, fontface = 'italic')+
  annotate('text', x = 50, y = 23, label = entf, hjust = .35, col = 'red',
           family = 'mono', size = 3.5, fontface = 'italic')+
  labs(title = title, subtitle = subtitle)+
  scale_x_continuous(expand = expansion(add = c(0,0)))+
  scale_y_reverse()+
  scale_fill_manual(values = c('#BD9780', '#E0BDB0', '#968E7B', '#757CA8',
                               '#F9C152', '#E53C58'))+
  scale_color_manual(values = c( '#E53C58', '#757CA8', '#E0BDB0', '#BD9780', 
                                 '#968E7B', '#F9C152'))+
  coord_polar("y", start = 90.05)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = desert),
    plot.background = element_rect(fill = desert), 
    text = element_text(family = 'mono',face = 'bold'),
    plot.title = element_text(hjust = .5, vjust = -14, size = 15),
    plot.subtitle = element_text(hjust = .5, vjust = -20, size = 13, 
                                 color = 'red'), 
    plot.title.position = "plot",
    legend.position = 'None') -> p

# Saving  ---------------------------------------------------

ggsave('challenge10/plate_w10.svg', p, width = 9, height = 10, limitsize = F)

