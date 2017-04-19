
forward.stepwise.testF.lm <- function(tabla.conjunto , punto.corte=0.05 , steps=6 , punto.corte.vif = 2) { # funcion para realizar los modelos por lm
        
        is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete está instalado o no
        
        if ( is.installed("faraway") == FALSE) {install.packages("faraway")} #si faraway no está instalado hago que me lo instale automaticamente
        
        library(faraway) #cargo el paquete faraway
        
        df <- as.data.frame(tabla.conjunto)#hago por las dudas que la tabla del conjunto me la lleve a clase data frame
        
        null <- lm(clase ~1,data = df, na.action = na.omit)#modelo inicial para hacer el forward. na.omit hace que se omitan las filas que tienen NA values
        
        full <- lm(clase~.,data = df, na.action = na.omit)#este es el modelo con todos los descriptores, lo hago para poder armar el scope de la funcion add1.na.omit hace que se omitan las filas que tienen NA values 
        
        repeat {      ## repeat es un loop donde todo se repite de manera infinita hasta que se cumpla alguna de las condiciones if para darle finalizacion
                
                paso <- add1(null,scope=formula(full),test="F", na.action = na.omit)#hago que pruebe como da cuando agrego cada una de las variables individualmente. na.omit hace que se omitan las filas que tienen NA values
                
                menor.probabilidad <- which.min(paso$`Pr(>F)`)#elijo cual es el que posee menor p-valor
                
                elegido <- noquote(row.names(paso)[menor.probabilidad])#el descriptor elegido
                
                formulacion <- paste(".~. +", elegido) # esta ser?a la nueva formula para comenzar el modelo
                
                if ( paso$`Pr(>F)`[menor.probabilidad] >= punto.corte ) { #me fijo si el p-valor es menor a mi punto de corte, si lo es se repite el loop, sino termina aca
                        
                        break
                }
                
                null <- update(null, formula. = formulacion)#agrego el descriptor seleccionado por tener menor p-valor
                
                if (length(null$coefficients) == 3){ # le digo que si tiene 3 coeficientes, que significa que tiene dos variables para hacer el calculo de vif.
                        
                        test.vif <- data.frame(vif(null)) # calculo el vif de cada termino. quiero que sea menor a punto.corte.vif = 2, es bastante exigente esto, porque con menor a 5 esta bien.
                        
                        if (test.vif[nrow(test.vif),1] >= punto.corte.vif) { #si algun termino no cumple con el valor de vif lo elimino. Como son solo 2 terminos, si uno no cumple el vif el otro tampoco, por ende solo elimino el que estaba intentando agregar al modelo
                                
                                formulacion2 <- paste(".~. -",paste(noquote(rownames(test.vif)[nrow(test.vif)]) ,collapse = " - ")) # selecciono los valores que no cumplen con mi VIF
                                
                                null<-update(null, formula. = formulacion2)#elimino los terminos que en el paso anterior seleccione por no cumplir con el vif luego del agregado del nuevo termino
                                
                                full <- update(full, formula. = formulacion2) ### elimino las variables del modelo full que no cumplen con el VIF para que no intenten ingresar otra vez al modelo
                                
                        }
                }
                
                if (length(null$coefficients) > 3){ # le digo que si tengo mas de 3 coeficientes, que significa que tiene 3 o mas variables haga el calculo del vif. Lo diferencio de la situación de cuando tengo 2 variables porque en este caso si puede suceder que una sola no cumple el vif, en cambio en la situacion de 2 variables si no cumple una tampoco cumple la otra
                        
                        test.vif <- data.frame(vif(null)) # calculo el vif de cada termino. quiero que sea menor a punto.corte.vif = 2, es bastante exigente esto, porque con menor a 5 esta bien.
                        
                        if (any(test.vif[,1] >= punto.corte.vif)) { #si algun termino no cumple con el valor de vif lo elimino
                                
                                formulacion2 <- paste(".~. -",paste(noquote(rownames(test.vif)[nrow(test.vif)]) ,collapse = " - ")) # selecciono los valores que no cumplen con mi VIF
                                
                                null<-update(null, formula. = formulacion2)#elimino los terminos que en el paso anterior seleccione por no cumplir con el vif luego del agregado del nuevo termino
                                
                                full <- update(full, formula. = formulacion2) ### elimino las variables del modelo full que no cumplen con el VIF para que no intenten ingresar otra vez al modelo
                                
                        }
                }
                
                test <- drop1(null, test = "F") #hago un anova al modelo con el nuevo descriptor para obtener los p-valores de todos los terminos, asi elimino los que ahora no cumplen con el p-valor ahora que agregu? un termino nuevo
                
                if (any(test$`Pr(>F)`>= punto.corte, na.rm = TRUE)) { #si algun p-valor de algun termino del modelo es mayor al punto de corte lo elimino
                        
                        formulacion3<- paste(".~. -",paste(noquote(rownames(test)[which(test$`Pr(>F)`>= punto.corte)]) ,collapse = " - ")) # selecciono los valores que no cumplen con mi p-valor
                        
                        null<-update(null, formula. = formulacion3)#elimino los terminos que en el paso anterior seleccione por no cumplir con el p-valor luego del agregado del nuevo termino
                        
                        full<-update(full, formula. = formulacion3)#elimino los terminos que en el paso anterior seleccione por no cumplir con el p-valor luego del agregado del nuevo termino
                        
                }
                
                if (length(null$coefficients) >= steps + 1){ # le digo que corte cuando cumplo con el numero de steps m?ximo, sumo 1 porque los coeficientes son los descriptores + ordenada al origen
                        
                        break
                }
                
                if (length(full$coefficients) == length(null$coefficients)){ ## si tengo el mismo numero de coeficientes en null y full significa que no tengo mas descriptores para probar y por eso break
                        
                        break
                }
                
        }
        
        result <-null
        
        result   # es el modelo que me da como resultado
        
}

###### HASTA ACA ES LA FUNCION, PRIMERO LA CARGO Y LUEGO EJECUTO LO DE ABAJO

lista.modelos<- lapply(lista.conjuntos2, forward.stepwise.testF.lm, punto.corte=0.05,steps=6 ,punto.corte.vif = 2)# aplico a cada conjunto la funci?n llamada forward.stepwise.testF.lm para obtener los modelos. si quiero modifico punto de corte y numero de steps

lista.modelos<- lapply(conjuntos.descriptores(archivo="TrainingSet.csv" , conjuntos=1000 , descrip.conjunto = 200,semilla = 125), forward.stepwise.testF.lm, punto.corte=0.05,steps=6)# aplico a cada conjunto la funci?n llamada forward.stepwise.testF.lm para obtener los modelos. si quiero modifico punto de corte y numero de steps
