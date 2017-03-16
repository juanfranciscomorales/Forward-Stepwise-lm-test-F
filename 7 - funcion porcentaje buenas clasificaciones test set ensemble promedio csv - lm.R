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
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio,"Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparación AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC, "punto de corte", resultados.ensemble.promedio[[8]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE)
