#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE ranking

clasificaciones.test.set.ensemble.ranking.lm <- function (test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est� instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no est� instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        ranking<-apply(-tabla.valores.prediccion.test,2,rank, na.last= "keep", ties.method = "first")#aplico operador ranking en los valores predichos de los mejores modelos para cada compuesto. Me da una matriz donde tengo el lugar del ranking que ocupa mi compuesto para cada modelo. filas son los compuestos y las columnas son los modelos
        
        score.mejor.modelo.individual <- ranking[,1] ## extraigo la primer columna  que es la que tiene los scores del mejor modelo individual
        
        promedio.ranking <- apply(ranking, 1, mean, na.rm = remover.NA) ## calculo para cada compuesto cual es el promedio de ranking
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        ROC.ensemble.promedio.ranking <- roc(predictor = promedio.ranking, response = clase, direction = ">" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio.ranking <- ROC.ensemble.promedio.ranking$auc[[1]] ### extraigo el AUC de la curva ROC
        
        int.conf.95.AUC.ROC <- ROC.ensemble.promedio.ranking$ci ## extraigo el intervalo de confianza del AUC ROC
        
        ROC.mejor.individual<- roc(predictor = score.mejor.modelo.individual, response = clase, direction = ">" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)# curva ROC mejor modelo individual
        
        test.AUC <- roc.test(roc1 = ROC.ensemble.promedio.ranking , roc2 = ROC.mejor.individual , method = "delong" , alternative = "two.sided") ## test de comparaci�n de AUC de las curvas ROC
        
        df <- data.frame(cbind(clase,promedio.ranking)) ## creo un data frame donde tengo la clase y el valor del operador promedio.ranking para cada compuesto
        
        punto.corte <- OptimalCutpoints::optimal.cutpoints (X = "promedio.ranking",status="clase",tag.healthy=0, data=df,methods="MaxSp", direction=">") ## optimizo el punto de corte usando la funcion de maxima especificidad, cambi� la direccion porque en este caso los valores activos son los que tienen menor ranking
        
        punto.corte2 <- data.frame(OptimalCutpoints::summary.optimal.cutpoints(punto.corte)$p.table$Global$MaxSp)["cutoff",] ## extraigo el valor de punto de corte
        
        predicciones <- ifelse( promedio.ranking < punto.corte2, yes = 1,no = 0) ## veo cual es la prediccion usando el punto de corte optimizado. Los que est�n por debajo del punto de corte son activos en este caso, porque los mejores rankeados son los uqe est�n cerca de 1
        
        tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") ## hago la tabla para ver las clasificaciones      
        
        bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio.ranking, "Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparaci�n AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC ,"punto de corte", punto.corte2, "% bien clasificados test set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.ensemble.ranking.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.ranking.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)

clasificaciones.test.set.ensemble.ranking.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.ranking.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE)
