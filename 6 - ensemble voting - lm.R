combinacion.modelos.voting <- function (x = tabla.sensibilidad.ordenadas, cant.modelos = 10,fraccion.votos = 0.5) { #funcion donde x es el resultado de la funcion del script de curvas ROC.R y cant.modelos es la cant de modelos que quiero combinar
        
        mejores.segun.sensibilidad <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.sensibilidad]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.mejores.modelos <- lapply( lista.mejores.modelos , predict, type = "response")#armo una lista donde guardo las predicciones de cada uno de los mejroes modelos
        
        secuencia.puntos.corte<-puntos.corte.ROC.lm[ , "cutoff"] # puntos de corte
        
        secuencia.puntos.corte2 <- secuencia.puntos.corte[mejores.segun.sensibilidad] # solo los puntos de corte de los mejores modelos
        
        predicciones.redondeadas<-list()
        
        for( i in 1: length(lista.predicciones.mejores.modelos)) {
                predicciones.redondeadas[[i]] <- ifelse( lista.predicciones.mejores.modelos[[i]] > secuencia.puntos.corte2[i], yes = 1,no = 0)
                predicciones.redondeadas
        }
        
        matriz.predicciones.redondeadas <- matrix(unlist(predicciones.redondeadas), nrow= length(predicciones.redondeadas[[1]]), byrow=FALSE) #armo una matriz don
        
        votos <- apply(X = matriz.predicciones.redondeadas, MARGIN = 1, sum, na.rm =TRUE)
        
        predicciones.ensemble.voto.mayoria <- ifelse(votos > cant.modelos*fraccion.votos, yes=1,no=0 )
        
        clase <-lista.conjuntos2[[1]][,"clase" ] #extraigo los valores de la columna clase
        
        bien.clasificados <- predicciones.ensemble.voto.mayoria == clase ## veo si el voto me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
        
        tabla.bien.mal.clasificados <- table(predicciones.ensemble.voto.mayoria,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") #tabla para mostrar los bien y mal clasificados
        
        resultado.final <- list("% bien clasificados training set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados)
        
        resultado.final
        
}

############## ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

resultados.ensemble.voting <- combinacion.modelos.voting(x = tabla.sensibilidad.ordenadas, cant.modelos = 10, fraccion.votos = 0.5) # funcion para lm

resultados.ensemble.voting <- combinacion.modelos.voting(x = tabla.AUC.ordenadas, cant.modelos = 10,fraccion.votos = 0.5) # funcion para lm