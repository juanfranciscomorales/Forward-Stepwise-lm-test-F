#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE voto

clasificaciones.test.set.ensemble.voto.lm <- function (test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si openxlsx no está instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- as.data.frame(fread(input = test.set, check.names = TRUE)) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        ranking <- apply(-tabla.valores.prediccion.test,2,rank, na.last= "keep", ties.method = "first")#aplico operador ranking en los valores predichos de los mejores modelos para cada compuesto. 
        
        entero <- as.data.frame(apply(ranking, 2, function (i)  round (11- i/(0.02*nrow(tabla.valores.prediccion.test))))) ## calculo el entero que pide el ensemble por voting. A mayor valor ( o sea 11), mejor rankeado. Cero peor rankeado
        
        entero[entero<0]<-0 ## hago lo que pide el ensemble de hacer que los valores negativos se vuelvan 0. Los valores que estan en este data frame son el score final del ensemble voting
        
        score.mejor.modelo.individual <- entero[,1] ## extraigo la primer columna  que es la que tiene los scores del mejor modelo individual
        
        promedio.voto <- apply(entero, 1, mean, na.rm = remover.NA) ## calculo para cada compuesto cual es el promedio del score voting
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
        
        ROC.ensemble.promedio.voto <- roc(predictor = promedio.voto, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio.voto <- ROC.ensemble.promedio.voto$auc[[1]] ### extraigo el AUC de la curva ROC
        
        int.conf.95.AUC.ROC <- ROC.ensemble.promedio.voto$ci ## extraigo el intervalo de confianza del AUC ROC
        
        ROC.mejor.individual<- roc(predictor = score.mejor.modelo.individual, response = clase, direction = "<" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)# curva ROC mejor modelo individual
        
        test.AUC <- roc.test(roc1 = ROC.ensemble.promedio.voto , roc2 = ROC.mejor.individual , method = "delong" , alternative = "two.sided") ## test de comparación de AUC de las curvas ROC
        
        df <- data.frame(cbind(clase,promedio.voto)) ## creo un data frame donde tengo la clase y el valor del operador promedio.voto para cada compuesto
        
        punto.corte <- OptimalCutpoints::optimal.cutpoints (X = "promedio.voto",status="clase",tag.healthy=0, data=df,methods="MaxSp", direction="<") ## optimizo el punto de corte usando la funcion de maxima especificidad, cambié la direccion porque en este caso los valores activos son los que tienen menor voto
        
        punto.corte2 <- data.frame(OptimalCutpoints::summary.optimal.cutpoints(punto.corte)$p.table$Global$MaxSp)["cutoff",] ## extraigo el valor de punto de corte
        
        predicciones <- ifelse( promedio.voto > punto.corte2, yes = 1,no = 0) ## veo cual es la prediccion usando el punto de corte optimizado. Los que están por debajo del punto de corte son activos en este caso, porque los mejores rankeados son los uqe están cerca de 1
        
        tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") ## hago la tabla para ver las clasificaciones      
        
        bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio.voto, "Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparación AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC ,"punto de corte", punto.corte2, "% bien clasificados test set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.ensemble.voto.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.voto.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)

clasificaciones.test.set.ensemble.voto.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.voto.lm(test.set = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE)
