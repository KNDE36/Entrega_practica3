##### Correccion de la Entrega Practica 3  #####



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


#Armo las listas de cada estacion bien ordenadas
Aeroparque<-list("Codigo"=aeroparque[[1]][1],"Fecha"=aeroparque[[2]],"Nombre"=ubicacion[[1]][2],"Latitud"=ubicacion[[2]][2],"Longitud"=ubicacion[[3]][2],"Altura"=ubicacion[[4]][2],"Temp"=temp_aero,"Temp rocio"=temp_rocio_aero,"Presion"=aeroparque[[5]])
Azul<-list("Codigo"=azul[[1]][1],"Fecha"=azul[[2]],"Nombre"=ubicacion[[1]][1],"Latitud"=ubicacion[[2]][1],"Longitud"=ubicacion[[3]][1],"Altura"=ubicacion[[4]][1],"Temp"=temp_azul,"Temp rocio"=temp_rocio_azul,"Presion"=azul[[5]])
Catamarca<-list("Codigo"=catamarca[[1]][1],"Fecha"=catamarca[[2]],"Nombre"=ubicacion[[1]][3],"Latitud"=ubicacion[[2]][3],"Longitud"=ubicacion[[3]][3],"Altura"=ubicacion[[4]][3],"Temp"=temp_cata,"Temp rocio"=temp_rocio_cata,"Presion"=catamarca[[5]])
Chilecito<-list("Codigo"=chilecito[[1]][1],"Fecha"=chilecito[[2]],"Nombre"=ubicacion[[1]][4],"Latitud"=ubicacion[[2]][4],"Longitud"=ubicacion[[3]][4],"Altura"=ubicacion[[4]][4],"Temp"=temp_chilecito,"Temp rocio"=temp_rocio_chilecito,"Presion"=chilecito[[5]])
Iguazu<-list("Codigo"=iguazu[[1]][1],"Fecha"=iguazu[[2]],"Nombre"=ubicacion[[1]][5],"Latitud"=ubicacion[[2]][5],"Longitud"=ubicacion[[3]][5],"Altura"=ubicacion[[4]][5],"Temp"=temp_iguazu,"Temp rocio"=temp_rocio_iguazu,"Presion"=iguazu[[5]])

#Armo la lista de listas
Estaciones<-list("Aeroparque"=Aeroparque,"Azul"=Azul,"Catamarca"=Catamarca,"Chilecito"=Chilecito,"Iguazu"=Iguazu)  


#EJERCICIO 2)
#item a)

resumen<-function(lista) {
  M<-matrix(0,nrow=16,ncol=5)
  resumen_estacion<-data.frame(M,row.names = c("Nombre","Datos Totales","Fecha Inicial","Fecha Final","Temp Media","Desvio de Temp","Temp Minima","Temp Maxima","Temp de Rocio Media","Desvio de Temp de Rocio","Temp de Rocio Min","Temp de Rocio Max","Presion Media","Desvio de Presion","Presion Min","Presion Max"))
  colnames(resumen_estacion)<-c("Aeroparque","Azul","Catamarca","Chilecito","Iguazu")
  
  for (i in 1:length(lista)) {
    Tmedia<-mean(lista[[i]][["Temp"]],na.rm=T)
    Desvio_T<-sd(lista[[i]][["Temp"]],na.rm=T)
    min_T<-min(lista[[i]][["Temp"]],na.rm=T)
    max_T<-max(lista[[i]][["Temp"]],na.rm=T)
    TRmedia<-mean(lista[[i]][["Temp rocio"]],na.rm=T)
    Desvio_TR<-sd(lista[[i]][["Temp rocio"]],na.rm=T)
    min_TR<-min(lista[[i]][["Temp rocio"]],na.rm=T)
    max_TR<-max(lista[[i]][["Temp rocio"]],na.rm=T)
    P_media<-mean(lista[[i]][["Presion"]],na.rm=T)
    Desvio_P<-sd(lista[[i]][["Presion"]],na.rm=T)
    min_P<-min(lista[[i]][["Presion"]],na.rm=T)
    max_P<-max(lista[[i]][["Presion"]],na.rm=T)
    nombre<-lista[[i]][["Nombre"]]
    total_datos<-length(lista[[i]][["Fecha"]])
    Fecha1<-lista[[i]][["Fecha"]][1]
    Fecha2<-lista[[i]][["Fecha"]][total_datos]
    
    resumen_estacion[,i]<-c(nombre,total_datos,Fecha1,Fecha2,Tmedia,Desvio_T,min_T,max_T,TRmedia,Desvio_TR,min_TR,max_TR,P_media,Desvio_P,min_P,max_P)
  }
  return(resumen_estacion)
}

resumen(Estaciones)
# se observan valores NA e Inf en estadisticos de chilecito pero es esperable ya que todos sus valores de Presion, son codigos faltantes,tomados como NA


#item b)

region<-function(lista,lat_min,lat_max,long_min,long_max) {
  regiones<-c()
  for (i in 1:length(lista)) {
    if(lista[[i]][["Latitud"]] > lat_min & 
       lista[[i]][["Latitud"]] < lat_max &
       lista[[i]][["Longitud"]] > long_min &
       lista[[i]][["Longitud"]] < long_max )
      regiones[i]<-lista[[i]][["Nombre"]]
    else {
      print("No hay estaciones cercanas")
    }
  }
  return(regiones)
}

region(Estaciones,-30,-25,-70,-50) #algunas estaciones no cumplen
region(Estaciones,-40,0,-70,0)     #todas las estaciones cumplen


#item c)

save(Estaciones,file="Datos_Estaciones.Rdata")
