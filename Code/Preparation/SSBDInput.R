##.....................................................................
# Nomenclaturas

# Coicop

dt_coicop <- fread(paste0(wdd_in,"NomenclaturaCoicopDaneEnph.csv"))

dt_coicop <- dt_coicop[!is.na(carticulo),]

#Leading zeros: division subclase articulo
dt_coicop[,division:=sprintf("%02d", division)]
dt_coicop[,subclase:=sprintf("%02d", subclase)]
dt_coicop[,carticulo:=sprintf("%02d", carticulo)]

dt_coicop[,coicop:=paste0(division,grupo,clase,subclase,carticulo)]
dt_coicop[,coicop:=paste0(division,grupo,clase,subclase,carticulo)]

dt_coicop[,coicop:=paste0(division,grupo,clase,subclase,carticulo)]

dt_coicop[,c("division","grupo","clase","subclase","carticulo"):=NULL]

##.....................................................................
## Input data sets

# Individual demographics and household characteristics
dt_vivi <- as.data.table(read.spss(paste0(wdd_in,"Viviendas y hogares.sav"), 
                                   to.data.frame = TRUE,use.value.labels = F,r))

if(psample){
  dt_vivi <- dt_vivi[sample(.N,.N*0.01)]
  dt_vivi <- dt_vivi[!duplicated(dt_vivi$DIRECTORIO),]
}

dt_pers <- as.data.table(read.spss(paste0(wdd_in,"Caracteristicas generales personas.sav"), 
                                   to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_pers <- dt_pers[dt_pers$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

# Gastos diarios
dt_DiaUrb <- as.data.table(read.spss(paste0(wdd_in,"Gastos diarios Urbanos.sav"), 
                                     to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_DiaUrb <- dt_DiaUrb[dt_DiaUrb$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_DiaUrbMer <- as.data.table(read.spss(paste0(wdd_in,"Gastos diarios Urbanos - Mercados.sav"),
                                        to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_DiaUrbMer <- dt_DiaUrbMer[dt_DiaUrbMer$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_DiaUrbC <- as.data.table(read.spss(paste0(wdd_in,"Gastos diarios Urbano - Capitulo C.sav"),
                                      to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_DiaUrbC <- dt_DiaUrbC[dt_DiaUrbC$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_DiaUrbComi <- as.data.table(read.spss(paste0(wdd_in,"Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar.sav"),
                                         to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_DiaUrbComi <- dt_DiaUrbComi[dt_DiaUrbComi$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}


# Gastos personales
dt_PerUrb <- as.data.table(read.spss(paste0(wdd_in,"Gastos diarios personales Urbano.sav"), 
                                     to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_PerUrb <- dt_PerUrb[dt_PerUrb$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_PerRur <- as.data.table(read.spss(paste0(wdd_in,"Gastos personales Rural.sav"),
                                     to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_PerRur <- dt_PerRur[dt_PerRur$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_PerUrbComi <- as.data.table(read.spss(paste0(wdd_in,"Gastos personales Urbano - Comidas preparadas fuera del hogar.sav"),
                                         to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_PerUrbComi <- dt_PerUrbComi[dt_PerUrbComi$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_PerRurCom <- as.data.table(read.spss(paste0(wdd_in,"Gastos personales Rural - Comidas preparadas fuera del Hogar.sav"),
                                     to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_PerRurCom <- dt_PerRurCom[dt_PerRurCom$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

# # Gastos semanales
dt_SemRur <- as.data.table(read.spss(paste0(wdd_in,"Gastos semanales Rurales.sav"),
                                     to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_SemRur <- dt_SemRur[dt_SemRur$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_SemRurMerc <- as.data.table(read.spss(paste0(wdd_in,"Gastos semanales Rurales - Mercados.sav"),
                                         to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_SemRurMerc <- dt_SemRurMerc[dt_SemRurMerc$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_SemRurC <- as.data.table(read.spss(paste0(wdd_in,"Gastos semanales Rural - Capitulo C.sav"),
                                      to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_SemRurC <- dt_SemRurC[dt_SemRurC$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_SemRurComi <- as.data.table(read.spss(paste0(wdd_in,"Gastos semanales Rural - Comidas preparadas fuera del hogar.sav"),
                                         to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_SemRurComi <- dt_SemRurComi[dt_SemRurComi$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

# Gastos menos frecuentes
dt_MfrecPago <- as.data.table(read.spss(paste0(wdd_in,"Gastos menos frecuentes - Medio de pago.sav"),
                                        to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_MfrecPago <- dt_MfrecPago[dt_MfrecPago$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}

dt_MfrecArt <- as.data.table(read.spss(paste0(wdd_in,"Gastos menos frecuentes - Articulos.sav"),
                                       to.data.frame = TRUE,use.value.labels = F))

if(psample){
  dt_MfrecArt <- dt_MfrecArt[dt_MfrecArt$DIRECTORIO %in% unique(dt_vivi$DIRECTORIO),]
}
