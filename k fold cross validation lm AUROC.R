

k.fold.CV.AUROC.lm <- function (modelo , archivo.con.datos = "S-M training set lm.csv", folds = 10 , repeticiones = 1) {
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if (is.installed("data.table") == FALSE) {install.packages("data.table")} 
        
        if (is.installed("plyr") == FALSE) {install.packages("plyr")} 
        
        if (is.installed("caret") == FALSE) {install.packages("caret")} 
        
        if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente
        
        library(data.table) # cargo el paquete que tiene la funcion read.xlsx
        
        library(plyr)
        
        library(caret)
        
        library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion
        
        
        df <- as.data.frame(fread(input = archivo.con.datos, check.names = TRUE)) #leo el archivo con mis descriptores
        
        # Initialize progress bar
        
        pb <- txtProgressBar(min = 0, max = repeticiones, style = 3)
        
        # donde voy a guardar las predicciones para luego calcular las curvas ROC
        recoleccion <- matrix(nrow = nrow(df) , ncol = 2*repeticiones)
        
        for(j in 1:repeticiones) {
                
                set.seed(j)
                
                prediccion <- vector()
                
                clase <- vector()
                
                index <- createFolds( y = factor(df$clase) , k = folds , list = TRUE , returnTrain = FALSE)
                
                for(i in 1:folds) {
                        
                        ### train - test splitting
                        
                        train <- df[-index[[i]], ]
                        
                        test <- df[index[[i]], ]
                        
                        # re-Fitting con el training set menos los datos que se saco para hacer LGOCV
                        model <- update( object = modelo, formula. = . ~ ., data = train)
                        
                        # Predict results
                        results_prob <- predict(object = model, newdata = test,type='response')
                        
                        
                        # Actual answers
                        answers <- test$clase
                        
                        
                        # Collecting results
                        prediccion <- append(prediccion , results_prob) 
                        
                        clase <- append(clase, answers)
                        
                }
                
                recoleccion[ ,j*2-1] <- clase
                
                recoleccion[ ,j*2] <- prediccion
                
                setTxtProgressBar(pb, j)
                
        }
        
        auroc <- vector()
        
        for(j in 1:repeticiones) {
        
        auroc[j] <- roc(predictor = recoleccion[ ,j*2] , response = recoleccion[ ,j*2-1], direction = "<" , auc = TRUE)$auc[[1]]
        
        }
        
        # Average accuracy of the model
        auroc_promedio <- mean(auroc)
        
        close(pb)
        
        auroc_promedio
        
}




############ ACA SE TERMINA LA FUNCIÓN, LA CARGO Y LUEGO HAGO CORRER EL LOOP DE ABAJO


lista.AUROC.k.fold.CV.lm <- lapply(lista.modelos , k.fold.CV.AUROC.lm  , archivo.con.datos = "S-M training set lm.csv", folds = 10 , repeticiones = 5)# aplico a cada conjunto la funci?n llamada forward.stepwise.testF.lm para obtener los modelos. si quiero modifico punto de corte y numero de steps

tabla.AUROC.k.fold.CV.lm <- data.frame(modelo = 1:length(lista.modelos) , AUROC.kfold.CV =unlist(lista.AUROC.k.fold.CV.lm)) ## paso de lista a tabla

tabla.AUROC.k.fold.CV.lm <- tabla.AUROC.k.fold.CV.lm[order(tabla.AUROC.k.fold.CV.lm$AUROC.kfold.CV, decreasing = TRUE) ,] ## ordeno la tabla de manera decreciente

library(openxlsx)

write.xlsx(x= tabla.AUROC.k.fold.CV.lm, file= "Resultado k fold CV AUROC - lm.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos


## LOO  ###

## SI QUIERO LEAVE ONE OUT HAGO LO SIGUIENTE  ##############


lista.AUROC.LOO.lm <- lapply( lista.modelos , k.fold.CV.lm  , archivo.con.datos = "training set.csv", folds = nrow(lista.conjuntos2[[1]]) , repeticiones = 1)# aplico a cada conjunto la funci?n llamada forwar

tabla.AUROC.LOO.lm <- data.frame(modelo = 1:length(lista.modelos) , AUROC.LOO = unlist(lista.AUROC.LOO.lm))

tabla.AUROC.LOO.lm <- tabla.AUROC.LOO.lm[order(tabla.AUROC.LOO.lm$AUROC.LOO , decreasing = TRUE) ,] ## ordeno la tabla de manera decreciente


library(openxlsx)

write.xlsx(x= tabla.AUROC.LOO.lm, file= "Resultado LOO CV AUROC.xlsx" , colNames= TRUE, keepNA=TRUE) # funcion para guardar los resultados del screening en la base de datos


