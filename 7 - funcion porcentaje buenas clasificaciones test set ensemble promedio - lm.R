#####VOY A INTENTAR ARMAR UNA FUNCION PARA VER EL % DE BUENAS CLASIFICACIONES EN EL TEST SET POR EL ENSEMBLE promedio

clasificaciones.test.set.ensemble.promedio.lm <- function (test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE){
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est� instalado o no
        
        if (is.installed("openxlsx") == FALSE) {install.packages("openxlsx")} #si openxlsx no est� instalado hago que me lo instale automaticamente
        
        library(openxlsx) # cargo el paquete que tiene la funcion read.xlsx
        
        df.test.set <- read.xlsx(xlsxFile=test.set, check.names = TRUE) #leo el archivo con el test set
        
        mejores.modelos <- as.numeric(x[c(1:cant.modelos),"modelo"])#extraigo los mejores "cant.modelos" modelos
        
        lista.mejores.modelos <-lista.modelos[mejores.modelos]#armo una lista con el numero de los mejores modelos
        
        lista.predicciones.test <- lapply(X = lista.mejores.modelos, FUN = predict, newdata = df.test.set, type ="response" ) #genero una lista que contenga todas las predicciones de todos los modelos en el test set
        
        tabla.valores.prediccion.test <- data.frame(matrix(unlist(lista.predicciones.test), nrow= length(lista.predicciones.test[[1]]), byrow=FALSE))#a la lista lista.predicciones.mejores.modelos la vuelvo data frame
        
        promedio<-apply(tabla.valores.prediccion.test,1,mean, na.rm= remover.NA)#aplico operador promedio en los valores predichos de los mejores modelos para cada compuesto
        
        predicciones <- ifelse( promedio > resultados.ensemble.promedio[[4]], yes = 1,no = 0) ## predicciones aplicando el ensemble de operador promedio y usando el punto de corte que obtuve con el training
        
        clase <-df.test.set[,"clase" ] #extraigo los valores de la columna clase
       
        tabla.bien.mal.clasificados <- table(predicciones,clase, dnn = c("clase predicha", "clase real"), useNA = "ifany")  ##armo la tabla clasificatoria 
        
        bien.clasificados <- predicciones == clase ## veo si el ensemble me clasifico bien
        
        porcentaje.bien.clasificados <- 100*sum(bien.clasificados, na.rm = TRUE)/length(bien.clasificados) #porcentaje de buenas clasificaciones en el test set
        
        ROC.ensemble.promedio <- roc(predictor = promedio, response = clase)#creo lista donde voy a guardar las curvas ROC
        
        AUC.ROC.ensemble.promedio <- summary(auc(ROC.ensemble.promedio))[["Median"]] ## valor del AUC de la curva ROC
        
        
        resultado.final <- list("AUC de la curva ROC", AUC.ROC.ensemble.promedio,"punto de corte", resultados.ensemble.promedio[[4]], "% bien clasificados test set", porcentaje.bien.clasificados,"Classification Matrix", tabla.bien.mal.clasificados) ## lista con todos los resultados que quiero que aparezcan cuando aplico la funcion
        
        resultado.final ## pongo el resultado final
        
        
}

########### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas.test.set, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.sensibilidad.ordenadas, remover.NA = FALSE)

clasificaciones.test.set.ensemble.promedio.lm(test.set = "Dtest.xlsx",cant.modelos = 10, x = tabla.AUC.ordenadas.dude, remover.NA = FALSE)
