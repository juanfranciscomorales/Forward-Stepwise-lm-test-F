
data_set <- "S-M test set lm.csv"

tabla <- tabla.AUC.ordenadas

###### ENSEMBLE PROMEDIO ########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

p.valores <- vector() ## creo un vector vacio donde voy a poner los valores de p-valor para las comparaciones de las AUC entre el ensemble y el mejor modelo individual

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.promedio.lm(test.set = data_set ,cant.modelos = i, x = tabla, remover.NA = FALSE , graficar = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]

        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
        p.valores[i] <- resultado[[6]]$p.value
        
        }  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

library(plotly)

grafica.promedio <- plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles,type = "scatter" ,mode="markers", 
        
                            error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2, array = high_conf_int2)) %>%

                        layout(title = "Ensemble Promedio - Test set" , xaxis = list(title="Number of individual models included in the ensemble", range = c(0,101) , dtick =5), yaxis=list(title="AUROC Test set" , range = c(0.5 , 1.01) , dtick = 0.05 ))

grafica.promedio

htmlwidgets::saveWidget(as.widget(grafica.promedio), file = "AUROC vs cant modelos Ensemble Promedio - Test set o Dude.html")

tabla.p.valores.AUC <- data.frame(cant.modelos, p.valores) ## genero una tabla donde guardo los p-valores de las comparaciones de las AUC ROC

library(openxlsx)

write.xlsx(x= tabla.p.valores.AUC, file= "tabla p-valores comparaciones AUC ROC ensemble promedio vs AUC ROC mejor individual - test.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

####### ENSEMBLE MINIMO #########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

p.valores <- vector() ## creo un vector vacio donde voy a poner los valores de p-valor para las comparaciones de las AUC entre el ensemble y el mejor modelo individual

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.minimo.lm(test.set = data_set,cant.modelos = i, x = tabla, remover.NA = FALSE , graficar = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
        p.valores[i] <- resultado[[6]]$p.value
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

grafica.minimo <- plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
                        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
                        layout(title = "Ensemble Minimo - Test set",  xaxis = list(title="Number of individual models included in the ensemble", range = c(0,101) , dtick =5), yaxis=list(title="AUROC Test set" , range = c(0.5 , 1.01) , dtick = 0.05 ))

grafica.minimo

htmlwidgets::saveWidget(as.widget(grafica.minimo), file = "AUROC vs cant modelos Ensemble Minimo - Test set o Dude.html")

tabla.p.valores.AUC <- data.frame(cant.modelos, p.valores) ## genero una tabla donde guardo los p-valores de las comparaciones de las AUC ROC

library(openxlsx)

write.xlsx(x= tabla.p.valores.AUC, file= "tabla p-valores comparaciones AUC ROC ensemble minimo vs AUC ROC mejor individual - test.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

######### ENSEMBLE VOTO ##########

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

p.valores <- vector() ## creo un vector vacio donde voy a poner los valores de p-valor para las comparaciones de las AUC entre el ensemble y el mejor modelo individual

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.voto.lm(test.set = data_set,cant.modelos = i, x = tabla, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
        p.valores[i] <- resultado[[6]]$p.value
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

grafica.voto <- plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
                        error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
                        layout( xaxis = list(title="Number of individual models included in the ensemble", range = c(0,101) , dtick =5), yaxis=list(title="AUROC" , range = c(0.5 , 1.01) , dtick = 0.05 ))

htmlwidgets::saveWidget(as.widget(grafica.voto), file = "AUROC vs cant modelos Ensemble Voto - Test set o Dude.html")

tabla.p.valores.AUC <- data.frame(cant.modelos, p.valores) ## genero una tabla donde guardo los p-valores de las comparaciones de las AUC ROC

library(openxlsx)

write.xlsx(x= tabla.p.valores.AUC, file= "tabla p-valores comparaciones AUC ROC ensemble voto vs AUC ROC mejor individual - test.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

##### ENSEMBLE RANKING #######

vector.AUC.ensembles <- vector()  ## creo vector vacio donde voy a poner las AUC ROC

low_conf_int <- vector() ## creo vector vacio donde voy a poner los valores bajos del intervalo de confianza de la AUC ROC

high_conf_int <- vector() ## creo vector vacio donde voy a poner los valores altos del intervalo de confianza de la AUC ROC

p.valores <- vector() ## creo un vector vacio donde voy a poner los valores de p-valor para las comparaciones de las AUC entre el ensemble y el mejor modelo individual

cant.modelos <- 1:100 ## le doy la secuencia de la cantidad de modelos a combinar

for( i in cant.modelos){
        
        resultado <- clasificaciones.test.set.ensemble.ranking.lm(test.set = data_set ,cant.modelos = i, x = tabla, remover.NA = FALSE)
        
        vector.AUC.ensembles[i] <-resultado[[2]]
        
        low_conf_int[i] <- resultado[[4]][[1]]
        
        high_conf_int[i] <- resultado[[4]][[3]]
        
        p.valores[i] <- resultado[[6]]$p.value
}  

low_conf_int2 <- vector.AUC.ensembles - low_conf_int  ## creo un vector con la diferencia entre el AUC y el valor bajo del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar

high_conf_int2 <- high_conf_int - vector.AUC.ensembles ## creo un vector con la diferencia entre el AUC y el valor alto del intervalo. Este vector lo voy a usar para graficar. Tiene que ser esta diferencia si o si para graficar


library(plotly)

grafica.ranking <- plot_ly(x = ~cant.modelos, y = ~vector.AUC.ensembles, mode="markers", 
        
                                error_y = ~list(type = "data", symmetric = FALSE, arrayminus = low_conf_int2,array = high_conf_int2)) %>%
        
                                layout(title = "Ensemble Ranking - Test set o Dude" , xaxis = list(title="Number of individual models included in the ensemble", range = c(0,101) , dtick =5), yaxis=list(title="AUROC" , range = c(0.5 , 1.01) , dtick = 0.05 ))

htmlwidgets::saveWidget(as.widget(grafica.ranking), file = "AUROC vs cant modelos Ensemble Ranking - Test set o Dude.html")

tabla.p.valores.AUC <- data.frame(cant.modelos, p.valores) ## genero una tabla donde guardo los p-valores de las comparaciones de las AUC ROC

library(openxlsx)

write.xlsx(x= tabla.p.valores.AUC, file= "tabla p-valores comparaciones AUC ROC ensemble ranking vs AUC ROC mejor individual - test.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar la tabla de los puntos de corte de la curva ROC con los valores de sensibilidad y especificidad despues del filtrado

