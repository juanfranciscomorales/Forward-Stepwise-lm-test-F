##### ENSEMBLE OPERADOR ranking CON LOS MEJORES MODELOS

combinacion.modelos.ranking <- function (x = tabla.AUC.ordenadas, cant.modelos = 10, remover.NA = FALSE) { #funcion donde x es el resultado de la funcion del script de curvas ROC.R y cant.modelos es la cant de modelos que quiero combinar
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est� instalado o no
        
        if ( is.installed("OptimalCutpoints") == FALSE) {install.packages("OptimalCutpoints")} #si OptimalCutpoints no est� instalado hago que me lo instale automaticamente
        
        if( is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no est� instalado hago que me lo instale automaticamente
        
        library(OptimalCutpoints) #cargo el paquete OptimalCutpoints que tiene las funciones para esta funcion
        
        library(data.table) #cargo el paquete data.table que tiene las funciones para esta funcion
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con el numero de los mejores 20 modelos
        
        lista.predicciones.mejores.modelos <- lapply( lista.mejores.modelos , predict, type = "response")#armo una lista donde guardo las predicciones de cada uno de los mejroes 20 modelos
        
        tabla.valores.prediccion <- data.frame(matrix(unlist(lista.predicciones.mejores.modelos), nrow= length(lista.predicciones.mejores.modelos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        ranking <- apply(-tabla.valores.prediccion,2,rank, na.last= "keep", ties.method = "first")#aplico operador ranking en los valores predichos de los mejores modelos para cada compuesto. Me da una matriz donde tengo el lugar del ranking que ocupa mi compuesto para cada modelo. filas son los compuestos y las columnas son los modelos
        
        score.mejor.modelo.individual <- ranking[,1] ## extraigo la primer columna  que es la que tiene los scores del mejor modelo individual
        
        promedio.ranking <- apply(ranking, 1, mean, na.rm = remover.NA) ## calculo para cada compuesto cual es el promedio de ranking
        
        p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
        
        clase <-p[,"clase"]  ## extraigo la columna clase
        
        q <- factor(clase) ##hago que la columna clase se exprese como factor
        
        ROC.ensemble.promedio.ranking <- roc(predictor = promedio.ranking, response = q, direction = ">" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio.ranking <- ROC.ensemble.promedio.ranking$auc[[1]] ### extraigo el AUC de la curva ROC
        
        int.conf.95.AUC.ROC <- ROC.ensemble.promedio.ranking$ci ## extraigo el intervalo de confianza del AUC ROC
        
        ROC.mejor.individual<- roc(predictor = score.mejor.modelo.individual, response = q, direction = ">" , ci = TRUE , auc = TRUE , conf.level=0.95 , ci.method = "delong", boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE)# curva ROC mejor modelo individual
        
        test.AUC <- roc.test(roc1 = ROC.ensemble.promedio.ranking , roc2 = ROC.mejor.individual , method = "delong" , alternative = "two.sided") ## test de comparaci�n de AUC de las curvas ROC
        
        df <- data.frame(cbind(clase,promedio.ranking)) ## creo un data frame donde tengo la clase y el valor del operador promedio.ranking para cada compuesto
        
        punto.corte <- OptimalCutpoints::optimal.cutpoints (X = "promedio.ranking",status="clase",tag.healthy=0, data=df,methods="MaxSp", direction=">") ## optimizo el punto de corte usando la funcion de maxima especificidad, cambi� la direccion porque en este caso los valores activos son los que tienen menor ranking
        
        punto.corte2 <- data.frame(OptimalCutpoints::summary.optimal.cutpoints(punto.corte)$p.table$Global$MaxSp)["cutoff",] ## extraigo el valor de punto de corte
        
        predicciones <- ifelse( promedio.ranking < punto.corte2, yes = 1,no = 0) ## veo cual es la prediccion usando el punto de corte optimizado. Los que est�n por debajo del punto de corte son activos en este caso, porque los mejores rankeados son los uqe est�n cerca de 1
        
        tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany") ## hago la tabla para ver las clasificaciones      
        
        bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el training set
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio.ranking,"Int Confianza AUC ROC" ,int.conf.95.AUC.ROC ,"Comparaci�n AUC ROC ensemble vs AUC ROC mejor modelo individual" , test.AUC , "punto de corte", punto.corte2, "% bien clasificados training set",porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
}

############# ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

resultados.ensemble.ranking <- combinacion.modelos.ranking(x = tabla.AUC.ordenadas, cant.modelos = 10, remover.NA = FALSE)

resultados.ensemble.ranking <- combinacion.modelos.ranking(x = tabla.AUC.ordenadas.test.set, cant.modelos = 10, remover.NA = FALSE)

resultados.ensemble.ranking <- combinacion.modelos.ranking(x = tabla.AUC.ordenadas.dude, cant.modelos = 10, remover.NA = FALSE)

resultados.ensemble.ranking <- combinacion.modelos.ranking(x = tabla.sensibilidad.ordenadas, cant.modelos = 10, remover.NA = FALSE)