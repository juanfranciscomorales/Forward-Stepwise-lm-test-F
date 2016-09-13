clasificaciones.base.datos.ensemble.promedio.lm <- function (base.datos = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA =FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} #si data.table no está instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion read.csv
        
        df.base.datos <- as.data.frame(fread(input = base.datos, check.names = TRUE)) #leo el archivo con la base de datos
        
        mejores.segun.AUC <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.segun.AUC]#armo una lista con los mejores modelos
        
        lista.predicciones.base.datos <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.base.datos, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en la base de datos
        
        tabla.valores.prediccion.base.datos <- data.frame(matrix(unlist(lista.predicciones.base.datos), nrow= length(lista.predicciones.base.datos[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        colnames(tabla.valores.prediccion.base.datos) <-  paste(rep("modelo" , length(mejores.segun.AUC)),paste(mejores.segun.AUC) ) ## hago que el nombre de cada columna sea el que le corresponda a cada modelo
        
        promedio <- apply(tabla.valores.prediccion.base.datos,1,mean , na.rm = remover.NA)#aplico operador promedio en los valores predichos de los mejores modelos para cada compuesto
        
        tabla.valores.prediccion.base.datos$promedio <- promedio ## lo transformo en data frame porque es una matrix
        
        tabla.valores.prediccion.base.datos$NOMBRE <- df.base.datos[, "NAME"] #agrego la columna de los nombres de cada compuesto
        
        nombres <- colnames(tabla.valores.prediccion.base.datos) ## armo un vector con los nombres de las columnas
        
        tabla.valores.prediccion.base.datos <- tabla.valores.prediccion.base.datos[ ,c(nombres[length(nombres)], nombres[1:length(nombres)-1]) ] ### reordeno las columnas de manera tal que el nombre de los compuestos sea la primera y la columna final sea el valor del promedio
        
        tabla.valores.prediccion.base.datos ##  este es el resultado final
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.promedio.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE) ## si quiero que sea por AUC

tabla.predicciones.base.datos <- clasificaciones.base.datos.ensemble.promedio.lm(base.datos  = "Dtest.csv",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE) ## si quiero que sea por modelos con mayor sensibilidad
