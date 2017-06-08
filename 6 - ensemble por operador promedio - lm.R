##### ENSEMBLE OPERADOR promedio CON LOS MEJORES MODELOS

combinacion.modelos.promedio <- function (x = tabla.AUC.ordenadas, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE) { #funcion donde x es el resultado de la funcion del script de curvas ROC.R y cant.modelos es la cant de modelos que quiero combinar
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("OptimalCutpoints") == FALSE) {install.packages("OptimalCutpoints")} #si OptimalCutpoints no está instalado hago que me lo instale automaticamente
        
        if( is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(OptimalCutpoints) #cargo el paquete OptimalCutpoints que tiene las funciones para esta funcion
        
        library(data.table) #cargo el paquete data.table que tiene las funciones para esta funcion
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores 20 modelos
        
        lista.predicciones.mejores.modelos <- lapply( lista.mejores.modelos , predict, type = "response")#armo una lista donde guardo las predicciones de cada uno de los mejroes 20 modelos
        
        tabla.valores.prediccion <- data.frame(matrix(unlist(lista.predicciones.mejores.modelos), nrow= length(lista.predicciones.mejores.modelos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        score.mejor.modelo.individual <- tabla.valores.prediccion[,1] ## extraigo la primer columna  que es la que tiene los scores del mejor modelo individual
        
        promedio<-apply(tabla.valores.prediccion,1,mean, na.rm= remover.NA)#aplico operador promedio en los valores predichos de los mejores modelos para cada compuesto
        
        p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
                
        clase <-p[,"clase"]  ## extraigo la columna clase
                
        ROC.ensemble.promedio <- roc(predictor = promedio, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE, plot = graficar , print.auc = TRUE, main = "Training set - lm - Ensemble Promedio")#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio <- ROC.ensemble.promedio$auc[[1]] ### extraigo el AUC de la curva ROC
        
        int.conf.95.AUC.ROC <- ROC.ensemble.promedio$ci ## extraigo el intervalo de confianza del AUC ROC
        
        ROC.mejor.individual<- roc(predictor = score.mejor.modelo.individual, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        test.AUC <- roc.test(roc1 = ROC.ensemble.promedio , roc2 = ROC.mejor.individual , method = "delong" , alternative = "two.sided")
        
        df <- data.frame(cbind(clase,promedio)) ## creo un data frame donde tengo la clase y el valor del operador promedio para cada compuesto
        
       punto.corte <- OptimalCutpoints::optimal.cutpoints (X = "promedio",status="clase",tag.healthy=0, data=df,methods="MaxSp", direction="<") ## optimizo el punto de corte usando la funcion de maxima especificidad
        
       punto.corte2 <- data.frame(OptimalCutpoints::summary.optimal.cutpoints(punto.corte)$p.table$Global$MaxSp)["cutoff",] ## extraigo el valor de punto de corte
       
       predicciones <- ifelse( promedio > punto.corte2, yes = 1,no = 0) ## veo cual es la prediccion usando el punto de corte optimizado
        
       tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") ## hago la tabla para ver las clasificaciones      
       
       bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
       
       porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
       
      
       ## EN ESTA SECCION DE LA FUNCION ES TODO PARA ARMAR LOS GRAFICOS QUE VAN A APARECER PARA ANALIZAR LOS RESULTADOS
       
       if (graficar == TRUE) {
               
               library(ROCR) ## abro el paquete ROCR
               
               predicciones <- prediction(predictions = promedio, labels = clase) ## genero los valores para armar los diferentes graficos
               
               library(ggplot2)
               
               library(plotly)
               
               performance.mat.cutoff <- performance(predicciones , measure = "mat" , x.measure = "cutoff")
               
               df.mat.cutoff <- data.frame(performance.mat.cutoff@x.values , performance.mat.cutoff@y.values)
               
               colnames(df.mat.cutoff) <- c( "cutoff" , "mat")
               
               
               
               performance.ppv.cutoff <- performance(predicciones , measure = "ppv" , x.measure = "cutoff")
               
               df.ppv.cutoff <- data.frame(performance.ppv.cutoff@x.values , performance.ppv.cutoff@y.values)
               
               colnames(df.ppv.cutoff) <- c( "cutoff" , "ppv")
               
               
               
               performance.acc.cutoff <- performance(predicciones , measure = "acc" , x.measure = "cutoff")
               
               df.acc.cutoff <- data.frame(performance.acc.cutoff@x.values , performance.acc.cutoff@y.values)
               
               colnames(df.acc.cutoff) <- c( "cutoff" , "acc")
               
               
               
               performance.sens.cutoff <- performance(predicciones , measure = "sens" , x.measure = "cutoff")
               
               df.sens.cutoff <- data.frame(performance.sens.cutoff@x.values , performance.sens.cutoff@y.values)
               
               colnames(df.sens.cutoff) <- c( "cutoff" , "sens")
               
               
               
               performance.spec.cutoff <- performance(predicciones , measure = "spec" , x.measure = "cutoff")
               
               df.spec.cutoff <- data.frame(performance.spec.cutoff@x.values , performance.spec.cutoff@y.values)
               
               colnames(df.spec.cutoff) <- c( "cutoff" , "spec")
               
               
               
               performance.npv.cutoff <- performance(predicciones , measure = "npv" , x.measure = "cutoff")
               
               df.npv.cutoff <- data.frame(performance.npv.cutoff@x.values , performance.npv.cutoff@y.values)
               
               colnames(df.npv.cutoff) <- c( "cutoff" , "npv")
               
               
               
               grafico <- ggplot () + 
                       
                       geom_line(data = df.mat.cutoff , aes(x = cutoff, y = mat , color = "mat" ) , size = 1) + 
                       
                       geom_line(data = df.ppv.cutoff , aes(x = cutoff, y = ppv , color = "ppv" ) , size = 1) + 
                       
                       geom_line(data = df.acc.cutoff , aes(x = cutoff, y = acc , color = "acc" ) , size = 1) +
                       
                       geom_line(data = df.sens.cutoff , aes(x = cutoff, y = sens , color = "sens" ) , size = 1) +
                       
                       geom_line(data = df.spec.cutoff , aes(x = cutoff, y = spec , color = "spec" ) , size = 1) +
                       
                       geom_line(data = df.npv.cutoff , aes(x = cutoff, y = npv , color = "npv" ) , size = 1) +
                       
                       scale_colour_manual(values = c("red", "blue", "green" , "violet" , "yellow" , "orange")) +
                       
                       ggtitle("Training set - lm - Ensemble Promedio") +
                       
                       labs(colour = "Variable", y = NULL) 
               
               
               print(ggplotly(grafico))
               
       }
       
       ## RESUMEN DE LOS RESULTADOS
       
       resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio, "Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparación AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC , "punto de corte", punto.corte2, "% bien clasificados training set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
       
       resultado.final ## pongo el resultado final
       
       }

############# ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.AUC.ordenadas, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.AUC.ordenadas.test.set, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.AUC.ordenadas.dude, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.sensibilidad.ordenadas, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)

resultados.ensemble.promedio <- combinacion.modelos.promedio(x = tabla.AUROC.k.fold.CV.lm, cant.modelos = 10, remover.NA = FALSE, graficar = TRUE)