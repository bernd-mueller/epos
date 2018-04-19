library(tidyr)
library(dplyr)

createDiceFrame <- function () {
  ddice <- data.frame (Elements = 1:length(ddrugbankmesh), DrugBank = ddrugbankmesh, EpSO = depsomesh, ESSO = dessomesh, EPILONT = depimesh)
}

createCosineFrame <- function () {
  dcosine <- data.frame (Elements = 1:length(cdrugbankmesh), DrugBank = cdrugbankmesh, EpSO = cepsomesh, ESSO = cessomesh, EPILONT = cepimesh)
}

createJaccardPlot <- function (djaccard) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  jaccardplot <- ggplot(data = djaccard, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    theme_classic ()+
    theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    labs (y="Jaccard", title = "Jaccard Similarity Coefficient against MeSH") +
    geom_step(size=1) + 
    geom_step(data = djaccard, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    geom_step(data = djaccard, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    geom_step(data = djaccard, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    scale_colour_manual(name="Dictionary",values=cols)+
    scale_size_manual()   
}

createDicePlot <- function (ddice) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  diceplot <- ggplot(data = ddice, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    theme_classic ()+
    theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    labs (y="Dice", title = "Dice Similarity Coefficient against MeSH") +
    geom_step(size=1) + 
    geom_step(data = ddice, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    geom_step(data = ddice, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    geom_step(data = ddice, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    scale_colour_manual(name="Dictionary",values=cols)+
    scale_size_manual()   
  diceplot
}

createCosinePlot <- function (dcosine) {
  cols <- c("DrugBank" = "#f04546", "EpSO"="#3591d1","ESSO"="#62c76b","EPILONT"="#666666")
  
  #drugbanklabel <- djaccard %>% filter(DrugBank == max(DrugBank)) %>% mutate(point_label = "DrugBank")
  #geom_text(data = drugbanklabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1) +
  
  cosineplot <- ggplot(data = dcosine, aes(x=Elements, y=DrugBank, colour = "DrugBank"), log10="x") + 
    theme_classic ()+
    theme(panel.grid.major = element_line(colour = "gray"), panel.grid.minor.y = element_line(colour = "gray")) +
    labs (y="Cosine", title = "Cosine Similarity Coefficient against MeSH") +
    geom_step(size=1) + 
    geom_step(data = dcosine, aes(x=Elements, y=EpSO, colour = "EpSO"), size=1) + 
    geom_step(data = dcosine, aes(x=Elements, y=ESSO, colour = "ESSO"), size=1) + 
    geom_step(data = dcosine, aes(x=Elements, y=EPILONT, colour = "EPILONT"), size=1) + 
    coord_trans(x = "log10", limx = c(10, 10000), limy = c(-0.001,0.125)) +
    scale_x_continuous(breaks = c(1, 10, 100, 1000,10000)) +
    scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    scale_colour_manual(name="Dictionary",values=cols)+
    scale_size_manual()   
  cosineplot
}

  #coord_cartesian(xlim = c(0, 10000)) 
  #scale_colour_manual(name="Dictionary",values=cols) +
  

#geom_text(data = epsolabel, aes(label=point_label), show.legend = FALSE, hjust=+0.3, vjust=-1)

# drugbanklabel <- djaccard %>% filter(Elements == max(Elements)) %>% mutate(point_label = "DrugBank")