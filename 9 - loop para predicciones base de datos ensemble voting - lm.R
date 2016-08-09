lista.drugbank <- c("drug bank 1.xlsx","drug bank 2.xlsx","drug bank 3.xlsx","drug bank 4.xlsx","drug bank 5.xlsx","drug bank 6.xlsx","drug bank 7.xlsx","drug bank 8.xlsx","drug bank 9.xlsx","drug bank 10.xlsx","drug bank 11.xlsx","drug bank 12.xlsx","drug bank 13.xlsx","drug bank 14.xlsx")

lista.base.datos <- list()

for ( i in 1:14) {
        
        lista.base.datos[[i]] <- clasificaciones.base.datos.ensemble.voting.lm(base.datos  = lista.drugbank[i],cant.modelos = 50,fraccion.votos = 0.8, x = tabla.sensibilidad.ordenadas)  
}

lista.predicciones.drugbank <- c("predicciones drug bank 1.xlsx","predicciones drug bank 2.xlsx","predicciones drug bank 3.xlsx","predicciones drug bank 4.xlsx","predicciones drug bank 5.xlsx","predicciones drug bank 6.xlsx","predicciones drug bank 7.xlsx","predicciones drug bank 8.xlsx","predicciones drug bank 9.xlsx","predicciones drug bank 10.xlsx","predicciones drug bank 11.xlsx","predicciones drug bank 12.xlsx","predicciones drug bank 13.xlsx","predicciones drug bank 14.xlsx")

for (i in 1:14){
        
        write.xlsx(x= lista.base.datos[[i]], file= lista.predicciones.drugbank[i], colNames= TRUE, rowNames=TRUE, keepNA=TRUE) # funcion para g
}