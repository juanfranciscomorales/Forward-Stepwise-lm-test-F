### quiero armar una funcion que me ordene los modelos segun maxima sensibilidad para los puntos de corte obtenidos



modelos.ordenados.sensibilidad <- function ( tabla.puntos.corte= puntos.corte.ROC.lm) {
      
        tabla.puntos.corte$modelo <- c(1:nrow(tabla.puntos.corte)) ## le agrego una columna donde marco que fila es cada modelo  
        
        newdata<- tabla.puntos.corte[with(tabla.puntos.corte, order(-Se, -Sp)),] ## ordeno por amyor especificidad y luego por mayor sensibilidad, quedandome los mas sensibles arriba en la tabla

        newdata ## tabla que tiene los modelos ordenados por los de mayor sensibilidad
}

############# ACA TERMINA LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO 

tabla.sensibilidad.ordenadas <- modelos.ordenados.sensibilidad(tabla.puntos.corte= puntos.corte.ROC.lm) # creo una tabla que me ordena los modelos por sensibilidad