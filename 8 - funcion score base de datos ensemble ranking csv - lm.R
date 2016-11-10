clasificaciones.base.datos.ensemble.ranking.lm <- function (base.datos = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA =FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion fread
        
        df.base.datos <- as.data.frame(fread(input=base.datos, check.names = TRUE)) #leo el archivo con la base de datos
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con los mejores modelos
        
        lista.predicciones.base.datos <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
        
        tabla.valores.prediccion.base.datos <- data.frame(matrix(unlist(lista.predicciones.base.datos), nrow= length(lista.predicciones.base.datos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        colnames(tabla.valores.prediccion.base.datos) <-  paste(rep("modelo" , length(mejores.segun.AUC)),paste(mejores.segun.AUC) ) ## hago que el nombre de cada columna sea el que le corresponda a cada modelo
       
        ranking<-as.data.frame(apply(-tabla.valores.prediccion.base.datos,2,rank, na.last= "keep", ties.method = "first"))#aplico operador ranking en los valores predichos de los mejores modelos para cada compuesto. Me da una matriz donde tengo el lugar del ranking que ocupa mi compuesto para cada modelo. filas son los compuestos y las columnas son los modelos
        
        promedio.ranking <- apply(ranking, 1, mean, na.rm = remover.NA) ## calculo para cada compuesto cual es el promedio de ranking
        
        ranking$promedio.ranking <- promedio.ranking ## lo transformo en data frame porque es una matrix
        
        ranking$NOMBRE <- df.base.datos[, "NAME"] #agrego la columna de los nombres de cada compuesto
        
        nombres <- colnames(ranking) ## armo un vector con los nombres de las columnas
        
        ranking <- ranking[ ,c(nombres[length(nombres)], nombres[1:length(nombres)-1]) ] ### reordeno las columnas de manera tal que el nombre de los compuestos sea la primera y la columna final sea el valor del ranking
        
        ranking ##  este es el resultado final
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.ranking.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE) ## si quiero que sea por AUC

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.ranking.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE) ## si quiero que sea por modelos con mayor sensibilidad

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.ranking.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE) ## si quiero que sea por modelos con mayor AUC ROC en la base dude

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.ranking.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE) ## si quiero que sea por AUC

