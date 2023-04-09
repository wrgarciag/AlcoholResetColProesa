##.....................................................................
## Cuadro Consumo hogares

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))
# 1. Urbano-----


# 9	Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar	"dt_DiaUrbComi" 
# dt_DiaUrbComi----

dc_DiaUrbCF <- copy(dt_DiaUrbComi)

# regexes<- list(c("(gaseosa)","1"))
# 
# output_vector <- character(nrow(dc_DiaUrbCF))
# for(i in seq_along(regexes)){
#   output_vector[grepl(x = tolower(dc_DiaUrbCF$NH_CGDUCFH_P1), pattern =regexes[[i]][1])] <- regexes[[i]][2]
# }
# 
# dc_DiaUrbCF <- cbind(dc_DiaUrbCF,output_vector)
# 
# dc_DiaUrbCF <- dc_DiaUrbCF[output_vector==1 | NH_CGDUCFH_P1_1==11110301,]


dc_DiaUrbCF <- fclasidrinks(data=dc_DiaUrbCF,col="NH_CGDUCFH_P1_1")
table(dc_DiaUrbCF$Item)

dc_DiaUrbCF <- ftextdrinks(data=dc_DiaUrbCF,col="NH_CGDUCFH_P1")
table(dc_DiaUrbCF$Item, dc_DiaUrbCF$Texto)


dc_DiaUrbCF <- dc_DiaUrbCF[Item!="",]
table(dc_DiaUrbCF$Item, dc_DiaUrbCF$Texto)
#table(dc_DiaUrbCF$Item,dc_DiaUrbCF$Canal)

dt_alimentos <- dc_DiaUrbCF[,list(Gasto=mean(NH_CGDUCFH_P5,na.rm=T),
                                  N=.N),by=list(NH_CGDUCFH_P1,Item)]

dt_alimentos[grep("\\bagua\\b|agua botella|agua con gas|botella de agua", tolower(NH_CGDUCFH_P1)),Item:="Water"]
table(dt_alimentos$Item)


#dc_consu <- dc_DiaUrbCF[Item!="",list(Count=.N),by=list(Item,NH_CGDUCFH_P1)]

dc_DiaUrbCF[,FactorMensual:=1]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==1,2.14,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==2,2.14,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==3,2.14,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==4,2,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==6,0.5,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P6==7,1/3,FactorMensual)]
dc_DiaUrbCF[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

# Solo mensualiza compra 
dc_DiaUrbCF[,FactorMensual:=ifelse(NH_CGDUCFH_P3==1,FactorMensual,1)]

dc_DiaUrbCF <- dc_DiaUrbCF[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NH_CGDUCFH_P1","NH_CGDUCFH_P1_1",
                              "NH_CGDUCFH_P2","NH_CGDUCFH_P5","NH_CGDUCFH_P6","FactorMensual","FEX_C","Item", "Texto","Canal"),with=FALSE]

setnames(dc_DiaUrbCF,c("NH_CGDUCFH_P1","NH_CGDUCFH_P1_1","NH_CGDUCFH_P2","NH_CGDUCFH_P5","NH_CGDUCFH_P6"),
         c("DescriProducto","Producto","Cantidad","Gasto","Frecuencia"))


dc_DiaUrbCF[,pqlitro:=1]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_DiaUrbCF[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

#_______________________________________________________________

dc_DiaUrbCF[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_DiaUrbCF[ ,GastoMes:=Gasto*FactorMensual]



summary(dc_DiaUrbCF$LitrosMes[dc_DiaUrbCF$Item=="SSB"],na.rm = T)
summary(dc_DiaUrbCF$GastoMes[dc_DiaUrbCF$Item=="SSB"],na.rm = T)

dc_DiaUrbCF <- festconsu(data=dc_DiaUrbCF)

summary(dc_DiaUrbCF$LitrosMes[dc_DiaUrbCF$Item=="SSB"],na.rm = T)
summary(dc_DiaUrbCF$GastoMes[dc_DiaUrbCF$Item=="SSB"],na.rm = T)

sum(dc_DiaUrbCF$LitrosMes[dc_DiaUrbCF$Item=="SSB"]*dc_DiaUrbCF$FEX_C[dc_DiaUrbCF$Item=="SSB"])*12

#dc_DiaUrbCF %<>% 
#  mutate(
#    ConsuExpAnual = LitrosMes * FEX_C * 12,
#    GastoMesExp = GastoMes * FEX_C * 12
#  )

#Consumo_mes <- dc_DiaUrbCF %>% 
#  group_by(Texto, Item) %>% 
#  summarise(
#    ConsuTotal = sum(ConsuExp, na.rm = T),
#    GastoTotal = sum(GastoMesExp, na.rm = T)
#  )

#_______________________________________________________________


dc_DiaUrbCF[,Tabla:=9]

saveRDS(dc_DiaUrbCF,file = paste0(wdd_out,"DtDiaUrbComi",".rds"))

if(pstata){
  save.dta13(dc_DiaUrbCF,file = paste0(wdd_out,"DtDiaUrbComi",".dta"))  
}

#Limpiar
#rm(dc_DiaUrbCF)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# 10	Gastos diarios personales Urbano	"dt_PerUrb"     
# dt_PerUrb----

dc_PerUrb <- copy(dt_PerUrb)

#dc_PerUrb <- fclasicoicop(data=dc_PerUrb,col="NC4_CC_P1_1")
dc_PerUrb <- fclasidrinks(data=dc_PerUrb,col="NC4_CC_P1_1")
dc_PerUrb <- dc_PerUrb[, Texto := 0]
# Subset no Missing Item
dc_PerUrb <- dc_PerUrb[Item!="",]
table(dc_PerUrb$Item)

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

dc_PerUrb[,FactorMensual:=1]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==1,4.28,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==2,4.28,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==3,4.28,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==4,2,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==6,0.5,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==7,1/3,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P3==1,FactorMensual,1)]
dc_PerUrb[,FactorMensual:=ifelse(is.na(NC4_CC_P6),1,FactorMensual)]

# Solo mensualiza compra 
#dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P3==1,FactorMensual,1)]


dc_PerUrb[,UMedida:=NA]
dc_PerUrb <- dc_PerUrb[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NC4_CC_P1_1","NC4_CC_P2","UMedida","NC4_CC_P5","NC4_CC_P6","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

setnames(dc_PerUrb,c("NC4_CC_P1_1","NC4_CC_P2","NC4_CC_P5","NC4_CC_P6"),c("Producto","Cantidad","Gasto","Frecuencia"))

# Cruzar con eiquetas articulo
#dc_PerUrb <- merge(dc_PerUrb,dt_coicop,by.x=c("Producto"),by.y=c("coicop"),all.x=TRUE)

dc_PerUrb[,pqlitro:=1]
dc_PerUrb[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_PerUrb[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

# dc_PerUrb[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
# sum(dc_PerUrb$LitrosMes*dc_PerUrb$FEX_C)*12


#_______________________________________________________________

dc_PerUrb[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_PerUrb[ ,GastoMes:=Gasto*FactorMensual]

summary(dc_PerUrb$LitrosMes[dc_PerUrb$Item=="SSB"],na.rm = T)
summary(dc_PerUrb$GastoMes[dc_PerUrb$Item=="SSB"],na.rm = T)


dc_PerUrb <- festconsu(data=dc_PerUrb)

summary(dc_PerUrb$LitrosMes[dc_PerUrb$Item=="SSB"],na.rm = T)
summary(dc_PerUrb$GastoMes[dc_PerUrb$Item=="SSB"],na.rm = T)

sum(dc_PerUrb$LitrosMes[dc_PerUrb$Item=="SSB"]*dc_PerUrb$FEX_C[dc_PerUrb$Item=="SSB"])*12
#_______________________________________________________________

dc_PerUrb[,Tabla:=10]

saveRDS(dc_PerUrb,file = paste0(wdd_out,"DtPerUrb",".rds"))

if(pstata){
  save.dta13(dc_PerUrb,file = paste0(wdd_out,"DtPerUrb",".dta"))  
}

#Limpiar
#rm(dc_PerUrb)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 11	Gastos diarios Urbano - Capitulo C	"dt_DiaUrbC"    
# dt_DiaUrbC----

# Alimentos: 24 Agua, gaseosas, refrescos, jugos, té frío y
# otras bebidas no alcohólicas

dc_DiaUrbC <- copy(dt_DiaUrbC)

#dc_DiaUrbC <- dc_DiaUrbC[NC2_CC_P1==24,]

dc_DiaUrbC <- dc_DiaUrbC[NC2_CC_P1%in%c(08,10,22,24),]

# Se dejan los que si compraran dentro de los proximos 14 dias
dc_DiaUrbC <- dc_DiaUrbC[NC2_CC_P3_S2==1,]


dc_DiaUrbC[,Item:=""]
dc_DiaUrbC[,Item:=ifelse(NC2_CC_P1%in%c(08,10),"MilkDerivatives",Item)]
dc_DiaUrbC[,Item:=ifelse(NC2_CC_P1%in%c(22),"HotDrinks",Item)]
dc_DiaUrbC[,Item:=ifelse(NC2_CC_P1%in%c(24),"SSB",Item)]

table(dc_DiaUrbC$Item)

dc_DiaUrbC <- dc_DiaUrbC[,Texto := 0]
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

dc_DiaUrbC[,FactorMensual:=1]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==1,2.14,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==2,2.14,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==3,2.14,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==4,2,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==6,0.5,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(NC2_CC_P2==7,1/3,FactorMensual)]
dc_DiaUrbC[,FactorMensual:=ifelse(is.na(NC2_CC_P2),1,FactorMensual)]

dc_DiaUrbC[,UMedida:=NA]

dc_DiaUrbC[,Canal:="Hogar"]

dc_DiaUrbC <- dc_DiaUrbC[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NC2_CC_P3_S1","NC2_CC_P2","UMedida","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

setnames(dc_DiaUrbC,c("NC2_CC_P3_S1","NC2_CC_P2"),c("Gasto","Frecuencia"))

# Cruzar con eiquetas articulo
## Se le asigna COICOP y etiqueta de gaseosa
#dc_DiaUrbC[,articulo:="Gaseosas para consumo en el hogar"]
#dc_DiaUrbC[,Producto:=01220401]
dc_DiaUrbC[,Producto:=""]
dc_DiaUrbC[,Cantidad:=1]

dc_DiaUrbC[,Tabla:=11]

dc_DiaUrbC[,pqlitro:=1]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_DiaUrbC[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

dc_DiaUrbC[,LitrosMes:=Cantidad*FactorMensual*pqlitro]
sum(dc_DiaUrbC$LitrosMes*dc_DiaUrbC$FEX_C)*12

dc_DiaUrbC[ ,GastoMes:=Gasto*FactorMensual]


colnames(dc_DiaUrbC)
saveRDS(dc_DiaUrbC,file = paste0(wdd_out,"DtDiaUrbC",".rds"))

if(pstata){
  save.dta13(dc_DiaUrbC,file = paste0(wdd_out,"DtDiaUrbC",".dta"))
}

#Limpiar
#rm(dc_DiaUrbC)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 12	Gastos diarios Urbanos	"dt_DiaUrb"
# dt_DiaUrb----
# Urbano

dc_DiaUrb <- copy(dt_DiaUrb)

#dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")

dc_DiaUrb <- fclasidrinks(data=dc_DiaUrb,col="NH_CGDU_P1")

# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item!="",]
table(dc_DiaUrb$Item)
dc_DiaUrb <- dc_DiaUrb[, Texto := 0]

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

dc_DiaUrb[,FactorMensual:=1]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==1,2.14,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==2,2.14,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==3,2.14,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==4,2,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==6,0.5,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==7,1/3,FactorMensual)]

dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P5==1,FactorMensual,1)]
dc_DiaUrb[,FactorMensual:=ifelse(is.na(NH_CGDU_P9),1,FactorMensual)]

dc_DiaUrb <- dc_DiaUrb[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NH_CGDU_P1","NH_CGDU_P2","NH_CGDU_P3","NH_CGDU_P8","NH_CGDU_P9","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

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
dc_DiaUrb[,qestandar:=NA]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==1,NH_CGDU_P2/1000,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==2,NH_CGDU_P2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==3,NH_CGDU_P2/500,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==4,NH_CGDU_P2/33.814,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==5,NH_CGDU_P2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==6,NH_CGDU_P2*2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==7,NH_CGDU_P2*25,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(is.na(qestandar),1,qestandar)]

setnames(dc_DiaUrb,c("NH_CGDU_P1","NH_CGDU_P2","NH_CGDU_P3","NH_CGDU_P8","NH_CGDU_P9"),c("Producto","Cantidad","UMedida","Gasto","Frecuencia"))
# Cruzar con eiquetas articulo
#dc_DiaUrb <- merge(dc_DiaUrb,dt_coicop,by.x=c("Producto"),by.y=c("coicop"),all.x=TRUE)

dc_DiaUrb[,Tabla:=12]

dc_DiaUrb[,LitrosMes:=qestandar*FactorMensual]
sum(dc_DiaUrb$LitrosMes*dc_DiaUrb$FEX_C,na.rm = TRUE)*12

dc_DiaUrb[ ,GastoMes:=Gasto*FactorMensual]

colnames(dc_DiaUrb)
saveRDS(dc_DiaUrb,file = paste0(wdd_out,"DtDiaUrb",".rds"))

if(pstata){
  save.dta13(dc_DiaUrb,file = paste0(wdd_out,"DtDiaUrb",".dta"))
}

#Limpiar
#rm(dc_DiaUrb)


# 13	Gastos diarios Urbanos - Mercados	"dt_DiaUrbMer"  
# dt_DiaUrbMer----

# No contempla gasto en BA ni discriuminados
#dc_DiaUrbMer <- copy(dt_DiaUrbMer)

# 14	Gastos personales Urbano - Comidas preparadas fuera del hogar	"dt_PerUrbComi"
# dt_PerUrbComi----

dc_PerUrbComi <- copy(dt_PerUrbComi)
# 
# regexes<- list(c("(gaseosa)","1"))
# 
# output_vector <- character(nrow(dc_PerUrbComi))
# for(i in seq_along(regexes)){
#   output_vector[grepl(x = tolower(dc_PerUrbComi$NH_CGPUCFH_P1), pattern =regexes[[i]][1])] <- regexes[[i]][2]
# }
# 
# dc_PerUrbComi <- cbind(dc_PerUrbComi,output_vector)
# 
# dc_PerUrbComi <- dc_PerUrbComi[output_vector==1 |NH_CGPUCFH_P1_S1==11110301,]

dc_PerUrbComi <- fclasidrinks(data=dc_PerUrbComi,col="NH_CGPUCFH_P1_S1")

dc_PerUrbComi <- ftextdrinks(data=dc_PerUrbComi,col="NH_CGPUCFH_P1")

dc_PerUrbComi <- dc_PerUrbComi[Item!="",]

table(dc_PerUrbComi$Item)


dc_PerUrbComi[,FactorMensual:=1]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==1,4.28,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==2,4.28,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==3,4.28,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==4,2,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==6,0.5,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P6==7,1/3,FactorMensual)]
dc_PerUrbComi[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]

dc_PerUrbComi[,FactorMensual:=ifelse(NH_CGPUCFH_P3==1,FactorMensual,1)]

dc_PerUrbComi <- dc_PerUrbComi[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","NH_CGPUCFH_P1","NH_CGPUCFH_P1_S1",
                              "NH_CGPUCFH_P2","NH_CGPUCFH_P5","NH_CGPUCFH_P6","FactorMensual","FEX_C","Item", "Texto","Canal"),with=FALSE]

setnames(dc_PerUrbComi,
         c("NH_CGPUCFH_P1","NH_CGPUCFH_P1_S1","NH_CGPUCFH_P2","NH_CGPUCFH_P5","NH_CGPUCFH_P6"),
         c("DescriProducto","Producto","Cantidad","Gasto","Frecuencia"))

dc_PerUrbComi[,Tabla:=14]

dc_PerUrbComi[,pqlitro:=1]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_PerUrbComi[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

dc_PerUrbComi[,LitrosMes:=Cantidad*FactorMensual*pqlitro]

sum(dc_PerUrbComi$LitrosMes*dc_PerUrbComi$FEX_C)*12


#_______________________________________________________________

dc_PerUrbComi[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_PerUrbComi[ ,GastoMes:=Gasto*FactorMensual]

summary(dc_PerUrbComi$LitrosMes[dc_PerUrbComi$Item=="SSB"],na.rm = T)
summary(dc_PerUrbComi$GastoMes[dc_PerUrbComi$Item=="SSB"],na.rm = T)

dc_PerUrbComi <- festconsu(data=dc_PerUrbComi)

summary(dc_PerUrbComi$LitrosMes[dc_PerUrbComi$Item=="SSB"],na.rm = T)
summary(dc_PerUrbComi$GastoMes[dc_PerUrbComi$Item=="SSB"],na.rm = T)

sum(dc_PerUrbComi$LitrosMes[dc_PerUrbComi$Item=="SSB"]*dc_PerUrbComi$FEX_C[dc_PerUrbComi$Item=="SSB"])*12
#_______________________________________________________________

colnames(dc_PerUrbComi)

saveRDS(dc_PerUrbComi,file = paste0(wdd_out,"DtPerUrbComi",".rds"))

if(pstata){
  save.dta13(dc_PerUrbComi,file = paste0(wdd_out,"DtPerUrbComi",".dta"))  
}

#Limpiar
#rm(dc_PerUrbComi)


# # 15	Gastos menos frecuentes - Articulos	"dt_MfrecArt"  
# # dt_MfrecArt----
dc_MfrecArt <- copy(dt_MfrecArt)

dc_MfrecArt <- fclasidrinks(data=dc_MfrecArt,col="P10270")

# Subset no Missing Item
dc_MfrecArt <- dc_MfrecArt[Item!="",]
table(dc_MfrecArt$Item)
dc_MfrecArt <- dc_MfrecArt[, Texto := 0]

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
# 10 10. semestral
table(dc_MfrecArt$P10270S3)

# Todos los items son del capitulo 11, asi que se mensualiza con ultimo mes
table(dc_MfrecArt$CAP)

# Mensualizacion ultimo mes
pmrec1 <- c("A11","B11","C11","E11","F11","H11","J11","K11","L11")

dc_MfrecArt[,FactorMensual:=NA]

dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==3,1,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==4,1,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==5,1,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==9,1,FactorMensual)]

dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==6,0.5,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==7,1/3,FactorMensual)]

dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==8,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec1,FactorMensual:=ifelse(P10270S3==10,1/6,FactorMensual)]


# Mensualizacion ultimos tres meses

pmrec3 <- c("D11","D12","D13","D14","D15","D16","F12","G11","I11","J12")

dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==3,1/3,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==4,1/3,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==5,1/3,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==6,1/3,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==7,1,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==9,1,FactorMensual)]

dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==8,0.25,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec3,FactorMensual:=ifelse(P10270S3==10,0.5,FactorMensual)]

# Mensualizacion doce meses

pmrec12 <- c("B12","C12","E12","F13","G12","G121","G122","H12","I12","J13","J131","J132","L12")
  
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==3,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==4,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==5,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==9,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==6,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==7,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==8,1/12,FactorMensual)]
dc_MfrecArt[CAP %in% pmrec12 ,FactorMensual:=ifelse(P10270S3==10,1/12,FactorMensual)]

dc_MfrecArt[,FactorMensual:=ifelse(is.na(P10270S3),1,FactorMensual)]
dc_MfrecArt[,FactorMensual:=ifelse(is.na(FactorMensual),1,FactorMensual)]
dc_MfrecArt[,FactorMensual:=ifelse(FORMA==1,FactorMensual,1)]

dc_MfrecArt <- dc_MfrecArt[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","P10270","VALOR","P10270S3","FactorMensual","Item","Texto","FEX_C","Canal"),with=FALSE]

dc_MfrecArt[,Cantidad:=1]
dc_MfrecArt[,UMedida:=NA]

setnames(dc_MfrecArt,c("P10270","VALOR","P10270S3"),c("Producto","Gasto","Frecuencia"))
# Cruzar con eiquetas articulo
#dc_MfrecArt <- merge(dc_MfrecArt,dt_coicop,by.x=c("Producto"),by.y=c("coicop"),all.x=TRUE)

dc_MfrecArt[,Tabla:=15]

dc_MfrecArt[,pqlitro:=1]
dc_MfrecArt[,pqlitro:=ifelse(Item=="SSB",pqlitSSB,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="MilkDerivatives",pqlitMD,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="HotDrinks",pqlitHot,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="Beer",pqlitBeer,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="Water",pqlitWa,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="Spirits",pqlitSpi,pqlitro)]
dc_MfrecArt[,pqlitro:=ifelse(Item=="Wine",pqlitWi,pqlitro)]

# dc_MfrecArt[,LitrosMes:=Cantidad*pqlitro*FactorMensual]
# 
# sum(dc_MfrecArt$LitrosMes*dc_MfrecArt$FEX_C,na.rm = TRUE)*12

dc_MfrecArt[,Canal:="Hogar"]
#_______________________________________________________________

dc_MfrecArt[ ,LitrosMes:=Cantidad*FactorMensual*pqlitro]
dc_MfrecArt[ ,GastoMes:=Gasto*FactorMensual]
dc_MfrecArt <- festconsu(data=dc_MfrecArt)

sum(dc_MfrecArt$LitrosMes[dc_MfrecArt$Item=="SSB"]*dc_MfrecArt$FEX_C[dc_MfrecArt$Item=="SSB"])*12
#_______________________________________________________________

colnames(dc_MfrecArt)
saveRDS(dc_MfrecArt,file = paste0(wdd_out,"DtMfrecArt",".rds"))

if(pstata){
  save.dta13(dc_MfrecArt,file = paste0(wdd_out,"DtMfrecArt",".dta"))
}

#Limpiar
#rm(dc_MfrecArt)


# ----- Consolidacion -----

#Seleccionar variables que permitan sumar

#Bases de datos utilizadas

#dc_DiaUrbCF
#dc_PerUrb
#dc_DiaUrbC
#dc_DiaUrb
#dc_PerUrbComi
#dc_MfrecArt


dc_DiaUrbCF <-fselecciona(dc_DiaUrbCF)
dc_PerUrb <- fselecciona(dc_PerUrb)
dc_DiaUrbC <- fselecciona(dc_DiaUrbC)
dc_DiaUrb <- fselecciona(dc_DiaUrb)
dc_PerUrbComi <- fselecciona(dc_PerUrbComi)
dc_MfrecArt <- fselecciona(dc_MfrecArt)


UrbanoTotal <- rbind(dc_DiaUrbCF,
                     dc_PerUrb,
                     dc_DiaUrbC,
                     dc_DiaUrb,
                     dc_PerUrbComi,
                     dc_MfrecArt,fill=T)

# UrbanoTotal <- as.data.table(UrbanoTotal)
# UrbanoTotal[,capitulo:=substr(Producto,1,2)]
# table(UrbanoTotal$capitulo,UrbanoTotal$Canal)

UrbanoTotal %<>%
  mutate(
    ConsumoAnual = LitrosMes*12,
    GastoAnual = GastoMes*12,
    ConsumoAnualExp = LitrosMes*FEX_C*12,
    GastoAnualExp = GastoMes*FEX_C*12
  )

table(UrbanoTotal$Tabla,UrbanoTotal$Canal)
table(UrbanoTotal$Tabla)

saveRDS(UrbanoTotal,file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))

#Se hace expandiendo
ConsumoUrbanoBeverages <- 
  UrbanoTotal %>% 
  group_by(Item, Texto) %>% 
  summarise(
    LitrosAnuales = sum(ConsumoAnualExp, na.rm = T),
    GastoAnualExp = sum(GastoAnualExp, na.rm = T)
  )

#Tambien se necesitan las sumas por directorios para tener cuántos gastan
#se hace en muestra

ConsumoHogarUrbano <- 
  UrbanoTotal %>%
    group_by(DIRECTORIO,Item,Texto) %>% 
  summarise(
    LitrosAnuales = sum(ConsumoAnual, na.rm = T),
    GastoAnual = sum(GastoAnual, na.rm = T)
  )

# # 16	Gastos menos frecuentes - Medio de pago	"dt_MfrecPago"  
# # dt_MfrecPago----
# dc_PerUrbComi <- copy(dt_PerUrbComi)
# 











































# # Check
# 
# dc_PerUrb <- readRDS(file = paste0(wdd_out,"DConsumoDiaPerUrb",".rds"))
# dc_DiaUrb <- readRDS(file = paste0(wdd_out,"DConsumoDiaHogUrb",".rds"))
# 
# dc_checkH <- dc_DiaUrb[DIRECTORIO=="337048" & Item=="SSB",]
# dc_checkP <- dc_PerUrb[DIRECTORIO=="337048" & Item=="SSB",]
# 
# dc_check <- dc_DiaUrb[Item=="SSB",]
# sum(dc_check$qestandar*dc_check$FEX_C*dc_check$FactorMensual,na.rm = TRUE)*12
# 
# # Consumo total
# 
# dc_DiaUrb <- 






