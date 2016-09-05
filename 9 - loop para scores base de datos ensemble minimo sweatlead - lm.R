
setwd("C:/Users/Francisco/Dropbox/R/descriptores sweatlead")

lista.sweatlead <- c("SW1.xlsx", "SW2.xlsx" , "SW3.xlsx" , "SW4.xlsx", "SW5.xlsx" , "SW6.xlsx" , "SW7.xlsx" , "SW8.xlsx", "SW9.xlsx")

lista.base.datos <- list()

for ( i in 1:length(lista.sweatlead)) {
        
        lista.base.datos[[i]] <- clasificaciones.base.datos.ensemble.minimo.lm(base.datos  = lista.sweatlead[i],cant.modelos = 5, x = tabla.AUC.ordenadas.dude, remover.NA = TRUE)  
}

library(plyr)

tabla.scores.sweatlead <- ldply(lista.base.datos, data.frame)

write.xlsx(x= tabla.scores.sweatlead, file= "tabla scores sweatlead.xlsx", colNames= TRUE, rowNames=TRUE, keepNA=TRUE) # funcion para g

