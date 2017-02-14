
AUC.curvas.ROC <- function ( x = lista.modelos ) {

is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no

if ( is.installed("pROC") == FALSE) {install.packages("pROC")} #si pROC no está instalado hago que me lo instale automaticamente

library(pROC) #cargo el paquete pROC que tiene las funciones para esta funcion

lista.predicciones <- lapply( x , predict)#calculos los valores predecidos por el modelo para cada compuesto

lista.auc.roc.numeric<-list() #creo una lista vacia donde voy a poner los valores de AUC ROC calculados para cada modelo

for (i in 1:length(lista.predicciones)) { #loop para el calculo de las curvas ROC de todos los modelos
       
        p <-lista.conjuntos2[[1]] #extraigo el primer conjunto 
        
        q<-factor(p[,"clase"]) # creo el factor que va a usar la funcion roc asi hace las curvas
        
        lista.auc.roc.numeric[[i]]<- roc(predictor = lista.predicciones[[i]],response = q, direction = "<" , auc = TRUE)$auc[[1]] #calculo la curva ROC para cada conjunto y extraigo solo el valor de AUC que voy a utilizar
        
        lista.auc.roc.numeric # lista con las curvas ROC
       
         }


models.ranking.by.auc.roc<-order(unlist(lista.auc.roc.numeric), decreasing = TRUE) # ordenar de forma decreciente los valores de AUC de todos los modelos

matrix.ranking <- cbind(modelo = models.ranking.by.auc.roc , AUC = lista.auc.roc.numeric[models.ranking.by.auc.roc]) #tabla donde obtengo el ranking de modelos segun AUC de la curva ROC, es una matrix

tabla.ranking <-as.data.frame(matrix.ranking)#hago que la matrix se vuelva data frame

tabla.ranking<-data.frame(apply(X=tabla.ranking, MARGIN = 2, FUN = unlist))# hago de manera que mis columnas que son listas se vuelvan numeros solamente, para poder luego guardarlos con la funcion write.xlsx

tabla.ranking #para que me tire el data frame ordenado

}
###### ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

tabla.AUC.ordenadas <- AUC.curvas.ROC( x= lista.modelos) # aplico mi funcion a la lista de modelos armada con la funcion anterior, como resultado obtengo una tabla con los valores de AUC ordenadas por mejores modelos

