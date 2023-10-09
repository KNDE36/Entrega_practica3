##### Entrega Practica 3  #####


rm(list = ls())
setwd("C:/Users/cannm/OneDrive/Escritorio/LaboR_Cande/Entrega_Practica3")

#EJERCICIO 1)

#ingreso los datos de las estaciones
aeroparque<-read.table("AEROPARQUE.txt")
azul<-read.table("AZUL.txt")
catamarca<-read.table("CATAMARCA.txt")
chilecito<-read.table("CHILECITO.txt")
iguazu<-read.table("IGUAZU.txt")
ubicacion<-read.table("estaciones.txt")

#Nombro NA a los datos faltantes
aeroparque[aeroparque==9999.9]<--NA
azul[azul==9999.9]<--NA
catamarca[catamarca==9999.9]<--NA
chilecito[chilecito==9999.9]<--NA
iguazu[iguazu==9999.9]<--NA

#Cambio la temperatura de fahrenheit a celsius 
celsius<-function(estacion) {
  C<-(estacion[,3]-32)/1.8
  return(C)
}

temp_aero<-celsius(aeroparque)
temp_azul<-celsius(azul)
temp_cata<-celsius(catamarca)
temp_chilecito<-celsius(chilecito)
temp_iguazu<-celsius(iguazu)

#Cambio la temperatura de rociode fahrenheit a celsius
celsius_rocio<-function(estacion){
  C_rocio<-(estacion[,3]-32)/1.8
  return(C_rocio)
}

temp_rocio_aero<-celsius(aeroparque)
temp_rocio_azul<-celsius(azul)
temp_rocio_cata<-celsius(catamarca)
temp_rocio_chilecito<-celsius(chilecito)
temp_rocio_iguazu<-celsius(iguazu)


#Armo el array
Estaciones<-array(list(),dim = c(5,9))
colnames(Estaciones)<-c("Nombre","Latitud","Longitud","Altura","Codigo","Fecha","Temp","Temp Rocio","Presion")

#Armo las listas de cada estacion con todos los datos
Aeroparque<-list(ubicacion[2,1],ubicacion[2,2],ubicacion[2,3],ubicacion[2,4],aeroparque[1,1],aeroparque[,2],temp_aero,temp_rocio_aero,aeroparque[,5])
Azul<-list(ubicacion[1,1],ubicacion[1,2],ubicacion[1,3],ubicacion[1,4],azul[1,1],azul[,2],temp_azul,temp_rocio_azul,azul[,5])
Catamarca<-list(ubicacion[3,1],ubicacion[3,2],ubicacion[3,3],ubicacion[3,4],catamarca[1,1],catamarca[,2],temp_cata,temp_rocio_cata,catamarca[,5])
Chilecito<-list(ubicacion[4,1],ubicacion[4,2],ubicacion[4,3],ubicacion[4,4],chilecito[1,1],chilecito[,2],temp_chilecito,temp_rocio_chilecito,chilecito[,5])
Iguazu<-list(ubicacion[5,1],ubicacion[5,2],ubicacion[5,3],ubicacion[5,4],iguazu[1,1],iguazu[,2],temp_iguazu,temp_rocio_iguazu,iguazu[,5])

#Relleno el array con las listas de cada estacion
Estaciones[1,]<-Aeroparque
Estaciones[2,]<-Azul
Estaciones[3,]<-Catamarca
Estaciones[4,]<-Chilecito
Estaciones[5,]<-Iguazu



#EJERCICIO 2)
#item a)

#Armo un array vacio para poder ir llenandolo por fila con las listas de los datos calculados para cada estacion
resumen_estacion<-array(list(),dim=c(5,16))
colnames(resumen_estacion)<-c("Nombre","Datos Totales","Fecha Inicial","Fecha Final","Temp Media","Desvio de Temp","Temp Minima","Temp Maxima","Temp de Rocio Media","Desvio de Temp de Rocio","Temp de Rocio Min","Temp de Rocio Max","Presion Media","Desvio de Presion","Presion Min","Presion Max")
  
resumen<-function(array) {
  
  for (i in 1:nrow(array)) {
       total_datos<-length(array[[i,"Fecha"]])    
       fecha1<-array[[i,"Fecha"]][1]
       fecha2<-array[[i,"Fecha"]][total_datos]
       temp_media<-mean(array[[i,"Temp"]],na.rm=T)
       rocio_media<-mean(array[[i,"Temp Rocio"]],na.rm=T)
       presion_media<-mean(array[[i,"Presion"]],na.rm=T)
       desvio_temp<-sd(array[[i,"Temp"]],na.rm=T)
       desvio_rocio<-sd(array[[i,"Temp Rocio"]],na.rm = T)
       desvio_presion<-sd(array[[i,"Presion"]],na.rm=T)
       max_temp<-max(array[[i,"Temp"]],na.rm=T)
       max_rocio<-max(array[[i,"Temp Rocio"]],na.rm=T)
       max_presion<-max(array[[i,"Presion"]],na.rm=T)
       min_temp<-min(array[[i,"Temp"]],na.rm=T)
       min_rocio<-min(array[[i,"Temp Rocio"]],na.rm=T)
       min_presion<-min(array[[i,"Presion"]],na.rm=T)
       nombre<-array[[i,"Nombre"]]
       
   resumen_estacion[i, ]=list(nombre,total_datos,fecha1,fecha2,temp_media,desvio_temp,min_temp,max_temp,rocio_media,desvio_rocio,min_rocio,max_rocio,presion_media,desvio_presion,min_presion,max_presion)
 }
  return(resumen_estacion)
}

#Guardo en una variable el resumen y lo imprimo por pantalla el resumen
#Podria no guardarlo en una variable y la funcion lo imprimiria directamente pero me parecio que lo mejor es que quede guardado en el Environment
Resumen_Estaciones<-resumen(Estaciones)
Resumen_Estaciones


#Item b)

regiones<-list()
region<-function(array,lat_min,lat_max,long_min,long_max) { 
  for (i in 1:nrow(array)) {
    if(array[[i,"Latitud"]] > lat_min & 
       array[[i,"Latitud"]] < lat_max &
       array[[i,"Longitud"]] > long_min &
       array[[i,"Longitud"]] < long_max )
    regiones[i]<-array[[i,1]] 
    else {
      print("No hay estaciones cercanas")
    }
  }
  return(regiones)
}

#Ingreso valores para probar si funciona correctamente
#Aclaracion: no estaba segura exactamente de como pedia la consigna que devuelva el resultado de la funcion,por lo que hice dos formas 

#Una forma de ver lo obtenido, abriendo las listas guardadas en las variables
R1<-region(Estaciones,-30,-25,-70,-50)   #algunas estaciones no cumplen
R2<-region(Estaciones,-40,0,-70,0)       #todas las estaciones cumplen

#Otra forma,ver por pantalla directamente
region(Estaciones,-30,-25,-70,-50)   
region(Estaciones,-40,0,-70,0) 



#item c)

save(Estaciones,file="Datos_Estaciones.Rdata")
