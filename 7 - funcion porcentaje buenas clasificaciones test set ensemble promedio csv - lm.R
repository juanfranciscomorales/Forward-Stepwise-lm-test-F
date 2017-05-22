#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE promedio

clasificaciones.test.set.ensemble.promedio.lm <- function (test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion fread
        
        df.test.set <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        score.mejor.modelo.individual <- tabla.valores.prediccion.test[,1] ## extraigo la primer columna  que es la que tiene los scores del mejor modelo individual
        
        promedio<-apply(tabla.valores.prediccion.test,1,mean, na.rm= remover.NA)#aplico operador promedio en los valores predichos de los mejores modelos para cada compuesto
        
        predicciones <- ifelse( promedio > resultados.ensemble.promedio[[8]], yes = 1,no = 0) ## predicciones aplicando el ensemble de operador promedio y usando el punto de corte que obtuve con el training
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
       
        tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany")  ##armo la tabla clasificatoria 
        
        bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el test set
        
        ROC.ensemble.promedio <- roc(predictor = promedio, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio <- ROC.ensemble.promedio$auc[[1]] ### extraigo el AUC de la curva ROC
        
        int.conf.95.AUC.ROC <- ROC.ensemble.promedio$ci ## extraigo el intervalo de confianza del AUC ROC
        
        ROC.mejor.individual<- roc(predictor = score.mejor.modelo.individual, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        test.AUC <- roc.test(roc1 = ROC.ensemble.promedio , roc2 = ROC.mejor.individual , method = "delong" , alternative = "two.sided")
        
        ## EN ESTA SECCION DE LA FUNCION ES TODO PARA ARMAR LOS GRAFICOS QUE VAN A APARECER PARA ANALIZAR LOS RESULTADOS
        
        library(ROCR) ## abro el paquete ROCR
        
        predicciones <- prediction(predictions = promedio, labels = clase) ## genero los valores para armar los diferentes graficos
        
        plot(performance(predicciones , measure = "tpr" , x.measure = "fpr"), main ="ROC Curve Test o Dude") ## grafico de PPV versus punto de corte
        
        abline(0,1) ### agrego la linea que muestra como seria la clasificacion si fuese aleatoria
        
        plot(performance(predicciones , measure = "mat" , x.measure = "cutoff"), main = "MCC vs cutoff") ## grafico el valor del MCC(Matthews Correlation Coefficient) , mientas mas cercano a 1 mejor, mayor a 0.7 seria optimo. Es una medida de calidad de clasificacion binaria
        
        plot(performance(predicciones , measure = "ppv" , x.measure = "cutoff"), main ="PPV vs cutoff") ## grafico de PPV versus punto de corte
        
        plot(performance(predicciones , measure = "acc" , x.measure = "cutoff"), main ="Accuracy vs cutoff") ## grafico de PPV versus punto de corte
        
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
                
                labs(colour = "Variable", y = NULL) 
        
        print(ggplotly(grafico))
        
        ## RESUMEN DE LOS RESULTADOS
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio,"Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparación AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC, "punto de corte", resultados.ensemble.promedio[[8]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.porcentajes.buenas.clasificaciones.k.fold.CV.lm, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUROC.k.fold.CV.lm, remover.NA = FALSE)
