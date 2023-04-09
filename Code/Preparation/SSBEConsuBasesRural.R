
# 1. Rural-----




# 3	Gastos personales Rural - Comidas preparadas fuera del Hogar	"dt_PerRur"     
# dt_PerRur----

dc_PerRur <- copy(dt_PerRur)

#dc_PerRur <- fclasicoicop(data=dc_PerRur,col="NC2R_CE_P2")
dc_PerRur <- fclasidrinks(data=dc_PerRur,col="NC2R_CE_P2")
#Subset no Missing Item
dc_PerRur <- dc_PerRur[Item!="",]
table(dc_PerRur$Item)
dc_PerRur <- dc_PerRur[, Texto := 0]

# Mensualizacion del gasto
#Frecuencia de Compra
# 1 1. Diario
# 2 2. Varias veces por semana
# 3 3. Semanal
# 4 4. Quincenal
# 5 5. Mensual
# 6 6. Bimestral
# 7 7. Trimestral
# 9 9. Esporádica

dc_PerRur[,FactorMensual:=1]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==1,4.28,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==2,4.28,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==3,4.28,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==4,2,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==6,0.5,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P8==7,0.33,FactorMensual)]
dc_PerRur[,FactorMensual:=ifelse(is.na(NC2R_CE_P8),1,FactorMensual)]

dc_PerRur[,FactorMensual:=ifelse(NC2R_CE_P5S2==1,FactorMensual,1)]

dc_PerRur[,UMedida:=NA]

dc_PerRur <- dc_PerRur[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NC2R_CE_P2","NC2R_CE_P4S1","UMedida","NC2R_CE_P7","NC2R_CE_P8","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

setnames(dc_PerRur,c("NC2R_CE_P2","NC2R_CE_P4S1","NC2R_CE_P7","NC2R_CE_P8"),c("Producto","Cantidad","Gasto","Frecuencia"))

# Cruzar con eiquetas articulo
#dc_PerRur <- merge(dc_PerRur,dt_coicop,by.x=c("Producto"),by.y=c("coicop"),all.x=TRUE)

dc_PerRur[,pqlitro:=1]
dc_PerRur[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_PerRur[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

dc_PerRur[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
sum(dc_PerRur$LitrosMes*dc_PerRur$FEX_C)*12

#_______________________________________________________________

dc_PerRur[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_PerRur[ ,GastoMes:=Gasto*FactorMensual]
dc_PerRur <- festconsu(data=dc_PerRur)

sum(dc_PerRur$LitrosMes[dc_PerRur$Item=="SSB"]*dc_PerRur$FEX_C[dc_PerRur$Item=="SSB"])*12
#_______________________________________________________________


dc_PerRur[,Tabla:=3]

saveRDS(dc_PerRur,file = paste0(wdd_out,"DtPerRur",".rds"))

if(pstata){
  save.dta13(dc_PerRur,file = paste0(wdd_out,"DtPerRur",".dta"))  
}

#Limpiar
#rm(dc_PerRur)



# 4	Gastos personales Rural - Comidas preparadas fuera del Hogar	"dt_PerRurCom"  
# dt_PerRurCom----

dc_PerRurCom <- copy(dt_PerRurCom)

#dc_PerRurCom <- dc_PerRurCom[NC2R_CA_P3==11110301,]
dc_PerRurCom <- fclasidrinks(data=dc_PerRurCom,col="NC2R_CA_P3")

#Subset no Missing Item
dc_PerRurCom <- dc_PerRurCom[Item!="",]
dc_PerRurCom <- dc_PerRurCom[, Texto := 0]
# #Subset no Missing Item
# dc_PerRurCom[,Item:="SSB"]

dc_PerRurCom[,FactorMensual:=1]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==1,4.28,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==2,4.28,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==3,4.28,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==4,2,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==6,0.5,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P8_S1==7,0.33,FactorMensual)]
dc_PerRurCom[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

dc_PerRurCom[,FactorMensual:=ifelse(NC2R_CA_P5_S1==1,FactorMensual,1)]

dc_PerRurCom <- dc_PerRurCom[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN",
                              "NC2R_CA_P3","NC2R_CA_P4_S1","NC2R_CA_P7_S1","NC2R_CA_P8_S1",
                              "FactorMensual","FEX_C","Item", "Texto","Canal"),with=FALSE]

setnames(dc_PerRurCom,c("NC2R_CA_P3","NC2R_CA_P4_S1","NC2R_CA_P7_S1","NC2R_CA_P8_S1"),
         c("Producto","Cantidad","Gasto","Frecuencia"))

dc_PerRurCom[,pqlitro:=1]
dc_PerRurCom[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_PerRurCom[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

# dc_PerRurCom[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
# sum(dc_PerRurCom$LitrosMes*dc_PerRurCom$FEX_C)*12

#_______________________________________________________________

dc_PerRurCom[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_PerRurCom[ ,GastoMes:=Gasto*FactorMensual]
dc_PerRurCom <- festconsu(data=dc_PerRurCom)

sum(dc_PerRurCom$LitrosMes[dc_PerRurCom$Item=="SSB"]*dc_PerRurCom$FEX_C[dc_PerRurCom$Item=="SSB"])*12
#_______________________________________________________________


dc_PerRurCom[,Tabla:=4]

saveRDS(dc_PerRurCom,file = paste0(wdd_out,"DtPerRurCom",".rds"))

if(pstata){
  save.dta13(dc_PerRurCom,file = paste0(wdd_out,"DtPerRurCom",".dta"))  
}

#Limpiar
#rm(dc_PerRurCom)


# 5	Gastos semanales Rural - Capitulo C	 "dt_SemRurC"    
# dt_SemRurC----

dc_SemRurC <- copy(dt_SemRurC)

# dc_SemRurC <- dc_SemRurC[NC2_CC_P1==24,]
# dc_SemRurC[,Item:="SSB"]

dc_SemRurC <- dc_SemRurC[NC2_CC_P1%in%c(08,10,22,24),]

dc_SemRurC[,Item:=""]
dc_SemRurC[,Item:=ifelse(NC2_CC_P1%in%c(08,10),"MilkDerivatives",Item)]
dc_SemRurC[,Item:=ifelse(NC2_CC_P1%in%c(22),"HotDrinks",Item)]
dc_SemRurC[,Item:=ifelse(NC2_CC_P1%in%c(24),"SSB",Item)]

table(dc_SemRurC$Item)

dc_SemRurC <- dc_SemRurC[, Texto := 0]


# 2. Frecuencia
# 2 » 2. Diario
# 3 » 2.1 varias veces por semana
# 4 » 3. Semanal
# 5 » 4. Quincenal
# 6 » 5. Mensual
# 7 » 6. Bimestral
# 8 » 7. Trimestral
# 9 » 9. Esporádica
# 1 » 11. Nunca

dc_SemRurC[,FactorMensual:=1]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==1,4.28,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==2,4.28,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==3,4.28,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==4,2,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==6,0.5,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(NC2_CC_P2==7,0.33,FactorMensual)]
dc_SemRurC[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

dc_SemRurC[,UMedida:=NA]
dc_SemRurC[,Cantidad:=NA]

dc_SemRurC[,Canal:="Hogar"]

dc_SemRurC <- dc_SemRurC[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NC2_CC_P3_S1","NC2_CC_P2","UMedida","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

setnames(dc_SemRurC,c("NC2_CC_P3_S1","NC2_CC_P2"),c("Gasto","Frecuencia"))

# Cruzar con eiquetas articulo
## Se le asigna COICOP y etiqueta de gaseosa
#dc_DiaUrbC[,articulo:="Gaseosas para consumo en el hogar"]
dc_SemRurC[,Producto:=""]
dc_SemRurC[,Cantidad:=1]

dc_SemRurC[,Tabla:=5]

dc_SemRurC[,pqlitro:=1]
dc_SemRurC[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_SemRurC[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]


dc_SemRurC[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_SemRurC[,GastoMes:=Gasto*FactorMensual]
dc_SemRurC[Item=="SSB",GastoMes:=ifelse(is.na(GastoMes),pglitSSB*LitrosMes,GastoMes)]

#dc_semrurc tiene NA en gasto mes, no es posible imputar precios, bienes diferenciados.

sum(dc_SemRurC$LitrosMes*dc_SemRurC$FEX_C)*12

colnames(dc_SemRurC)
saveRDS(dc_SemRurC,file = paste0(wdd_out,"DtSemRurC",".rds"))

if(pstata){
  save.dta13(dc_SemRurC,file = paste0(wdd_out,"DtSemRurC",".dta"))
}

#Limpiar
#rm(dc_SemRurC)
 

# 6	Gastos semanales Rural - Comidas preparadas fuera del hogar	"dt_SemRurComi"
# dt_SemRurComi----

dc_SemRurComi <- copy(dt_SemRurComi)

# regexes<- list(c("(gaseosa)","1"))
# 
# output_vector <- character(nrow(dc_SemRurComi))
# for(i in seq_along(regexes)){
#   output_vector[grepl(x = tolower(dc_SemRurComi$NH_CGPRCFH_P1), pattern =regexes[[i]][1])] <- regexes[[i]][2]
# }
# 
# dc_SemRurComi <- cbind(dc_SemRurComi,output_vector)
# 
# dc_SemRurComi <- dc_SemRurComi[output_vector==1 | NH_CGPRCFH_P1S1==11110301,]

dc_SemRurComi <- fclasidrinks(data=dc_SemRurComi,col="NH_CGPRCFH_P1S1")
table(dc_SemRurComi$Item)

dc_SemRurComi <- ftextdrinks(data=dc_SemRurComi,col="NH_CGPRCFH_P1")
table(dc_SemRurComi$Item)

dc_SemRurComi <- dc_SemRurComi[Item!="",]


dc_SemRurComi[,FactorMensual:=1]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==1,4.28,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==2,4.28,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==3,4.28,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==4,2,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==6,0.5,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P6==7,0.33,FactorMensual)]
dc_SemRurComi[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

dc_SemRurComi[,FactorMensual:=ifelse(NH_CGPRCFH_P3==1,FactorMensual,1)]

dc_SemRurComi <- dc_SemRurComi[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NH_CGPRCFH_P1","NH_CGPRCFH_P1S1",
                              "NH_CGPRCFH_P2","NH_CGPRCFH_P5","NH_CGPRCFH_P6","FactorMensual","FEX_C","Item", "Texto","Canal"),with=FALSE]

setnames(dc_SemRurComi,c("NH_CGPRCFH_P1","NH_CGPRCFH_P1S1","NH_CGPRCFH_P2","NH_CGPRCFH_P5","NH_CGPRCFH_P6"),
         c("DescriProducto","Producto","Cantidad","Gasto","Frecuencia"))


dc_SemRurComi[,pqlitro:=1]
dc_SemRurComi[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_SemRurComi[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

# dc_SemRurComi[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
# 
# sum(dc_SemRurComi$LitrosMes*dc_SemRurComi$FEX_C)*12

#_______________________________________________________________

dc_SemRurComi[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_SemRurComi[ ,GastoMes:=Gasto*FactorMensual]
dc_SemRurComi <- festconsu(data=dc_SemRurComi)
#_______________________________________________________________


dc_SemRurComi[,Tabla:=6]

saveRDS(dc_SemRurComi,file = paste0(wdd_out,"DtSemRurComi",".rds"))

if(pstata){
  save.dta13(dc_SemRurComi,file = paste0(wdd_out,"DtSemRurComi",".dta"))  
}

#Limpiar
#rm(dc_SemRurComi)


# 7	Gastos semanales Rurales	"dt_SemRur"    
# dt_SemRur----

dc_SemRur <- copy(dt_SemRur)

#dc_SemRur <- fclasicoicop(data=dc_SemRur,col="NC2R_CA_P3")
dc_SemRur <- fclasidrinks(data=dc_SemRur,col="NC2R_CA_P3")

# Subset no Missing Item
#dc_SemRur <- dc_SemRur[Item=="SSB",]
dc_SemRur <- dc_SemRur[Item!="",]
dc_SemRur <- dc_SemRur[, Texto := 0]

# Mensualizacion del gasto
#Frecuencia de Compra
# 1 1. Diario
# 2 2. Varias veces por semana
# 3 3. Semanal
# 4 4. Quincenal
# 5 5. Mensual
# 6 6. Bimestral
# 7 7. Trimestral
# 9 9. Esporádica

dc_SemRur[,FactorMensual:=1]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==1,4.28,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==2,4.28,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==3,4.28,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==4,2,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==6,0.5,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P8_S1==7,0.33,FactorMensual)]
dc_SemRur[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

dc_SemRur[,FactorMensual:=ifelse(NC2R_CA_P5_S1==1,FactorMensual,1)]


# Estandarizar
# Unidad de medida
# 1  1. Centímetros Cúbicos
# 2  2. Litro
# 3  3. Gramos
# 4  4. Onzas
# 5  5. Libra
# 6  6. Kilo
# 7  7. Arroba
# 8  8. Otra

dc_SemRur[,qestandar:=NA]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==1,NC2R_CA_P4_S1/1000,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==2,NC2R_CA_P4_S1,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==3,NC2R_CA_P4_S1/500,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==4,NC2R_CA_P4_S1/33.814,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==5,NC2R_CA_P4_S1,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==6,NC2R_CA_P4_S1*2,qestandar)]
dc_SemRur[,qestandar:=ifelse(NC2R_CA_P4_S2==7,NC2R_CA_P4_S1*25,qestandar)]
dc_SemRur[,qestandar:=ifelse(is.na(qestandar),pqlitro,qestandar)]

dc_SemRur <- dc_SemRur[,c("DIRECTORIO","NC2R_CA_P3","NC2R_CA_P4_S1","NC2R_CA_P4_S2","NC2R_CA_P7_S1","NC2R_CA_P8_S1","FactorMensual","qestandar","Item","Texto","FEX_C","Canal"),with=FALSE]

setnames(dc_SemRur,c("NC2R_CA_P3","NC2R_CA_P4_S1","NC2R_CA_P4_S2","NC2R_CA_P7_S1","NC2R_CA_P8_S1"),c("Producto","Cantidad","UMedida","Gasto","Frecuencia"))


# Cruzar con eiquetas articulo
#dc_SemRur <- merge(dc_SemRur,dt_coicop,by.x=c("Producto"),by.y=c("coicop"),all.x=TRUE)

dc_SemRur[,Tabla:=7]

dc_SemRur[,LitrosMes:=qestandar*FactorMensual]

dc_SemRur[ ,GastoMes:=Gasto*FactorMensual]



sum(dc_SemRur$LitrosMes*dc_SemRur$FEX_C)*12

colnames(dc_SemRur)

saveRDS(dc_SemRur,file = paste0(wdd_out,"DtSemRur",".rds"))

if(pstata){
  save.dta13(dc_SemRur,file = paste0(wdd_out,"DtSemRur",".dta"))  
}

#Limpiar
#rm(dc_SemRur)


# 8	Gastos semanales Rurales - Mercados	 "dt_SemRurMerc" 
# dt_SemRurMerc ----

#dc_SemRur <- copy(dt_SemRurMerc)


#Bases que quedan con gasto y consumo de hogar y personal
# dc_PerRur
# dc_PerRurCom
# dc_SemRurC
# dc_SemRurComi
# dc_SemRur


# ---- Consolidacion -----

#Seleccionar variables que permitan sumar



#Estandarizar dataframes para hacer rbinds
dc_PerRur <- fselecciona(dc_PerRur)
dc_PerRurCom <- fselecciona(dc_PerRurCom)
dc_SemRurC <- fselecciona(dc_SemRurC)
dc_SemRurComi <- fselecciona(dc_SemRurComi)
dc_SemRur <- fselecciona(dc_SemRur)



#1 hay una que consolida todo codificado y no codificado

RuralTotal <- rbind(dc_PerRur,
                    dc_PerRurCom,
                    dc_SemRurC,
                    dc_SemRurComi,
                    dc_SemRur,fill=T)

RuralTotal %<>%
  mutate(
    ConsumoAnual = LitrosMes*12,
    GastoAnual = GastoMes*12,
    ConsumoAnualExp = LitrosMes*FEX_C*12,
    GastoAnualExp = GastoMes*FEX_C*12
  )

table(RuralTotal$Tabla,RuralTotal$Canal)
table(RuralTotal$Tabla)

saveRDS(RuralTotal,file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))



ConsumoRuralBeverages <- 
  RuralTotal %>% 
    group_by(Item, Texto) %>% 
      summarise(
        LitrosAnuales = sum(ConsumoAnualExp, na.rm = T),
        GastoAnualExp = sum(GastoAnualExp, na.rm = T)
      )



#Tambien se necesitan las sumas por directorios para tener cuántos gastan
#se hace en muestra

ConsumoHogarRural <- 
  RuralTotal %>%
  group_by(DIRECTORIO,Item,Texto) %>% 
  summarise(
    LitrosAnuales = sum(ConsumoAnual, na.rm = T),
    GastoAnual = sum(GastoAnual, na.rm = T)
  )











