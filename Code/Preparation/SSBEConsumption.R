##.....................................................................
## Cuadro Consumo hogares


v.precio <- 2258



dt_pers <- merge(dt_pers,dt_vivi[,c("DIRECTORIO","P3"),with=FALSE], all.x = TRUE)

v.poblacion <- sum(dt_pers$FEX_C[dt_pers$P3==1])

# 1. Hogares-----

# Urbano

dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")

# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item=="SSB",]


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
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==4,2,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==6,0.5,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P9==7,0.33,FactorMensual)]
dc_DiaUrb[,FactorMensual:=ifelse(NH_CGDU_P5==1,FactorMensual,1)]

dc_DiaUrb <- dc_DiaUrb[,c("DIRECTORIO","NH_CGDU_P8","FactorMensual","FEX_C"),with=FALSE]

# dc_DiaUrb[,Consu:=(NH_CGDU_P8*FactorMensual)/v.precio]
# 
# dc_DiaUrb  <- dc_DiaUrb[,list(Consu=sum(Consu)),by=list(DIRECTORIO,FEX_C)]
# 
# Consumo <- sum(dc_DiaUrb$Consu*dc_DiaUrb$FEX_C)*12
# 
# Consu.pc <- Consumo/v.poblacion


dc_DiaUrb[,Consu:=(NH_CGDU_P8*FactorMensual*FEX_C)/v.precio]
#dc_DiaUrb  <- dc_DiaUrb[,list(Consu=sum(Consu)),by=list(DIRECTORIO,FEX_C)]
Consumo <- sum(dc_DiaUrb$Consu)*12
Consu.pc <- Consumo/v.poblacion


# 2. Persona----

# Urbano

dc_PerUrb <- copy(dt_PerUrb)

dc_PerUrb <- fclasicoicop(data=dc_PerUrb,col="NC4_CC_P1_1")

# Subset no Missing Item
dc_PerUrb <- dc_PerUrb[Item=="SSB",]

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
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==4,2,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==6,0.5,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==7,0.33,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P3==1,FactorMensual,1)]

dc_PerUrb <- dc_PerUrb[,c("DIRECTORIO","NC4_CC_P5","FactorMensual","FEX_C"),with=FALSE]

dc_PerUrb[,Consu:=(NC4_CC_P5*FactorMensual)/v.precio]

dc_PerUrb  <- dc_PerUrb[,list(Consu=sum(Consu)),by=list(FEX_C)]

ConsumoPer <- sum(dc_PerUrb$Consu*dc_PerUrb$FEX_C)*12

ConsuPer.pc <- ConsumoPer/v.poblacion


## Comidas fuera del hogar

dc_DiaUrbCF <- copy(dt_DiaUrbComi)

regexes<- list(c("(gaseosa)","1"))

output_vector <- character(nrow(dc_DiaUrbCF))
for(i in seq_along(regexes)){
  output_vector[grepl(x = tolower(dc_DiaUrbCF$NH_CGDUCFH_P1), pattern =regexes[[i]][1])] <- regexes[[i]][2]
}

dc_DiaUrbCF <- cbind(dc_DiaUrbCF,output_vector)

dc_DiaUrbCF <- dc_DiaUrbCF[NH_CGDUCFH_P1_1==11110301,]


dc_PerUrb[,FactorMensual:=1]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==1,4.28,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==4,2,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==6,0.5,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P6==7,0.33,FactorMensual)]
dc_PerUrb[,FactorMensual:=ifelse(NC4_CC_P3==1,FactorMensual,1)]

dc_PerUrb <- dc_PerUrb[,c("DIRECTORIO","NC4_CC_P5","FactorMensual","FEX_C"),with=FALSE]

dc_PerUrb[,Consu:=(NC4_CC_P5*FactorMensual)/v.precio]

dc_PerUrb  <- dc_PerUrb[,list(Consu=sum(Consu)),by=list(DIRECTORIO,FEX_C)]

ConsumoPer <- sum(dc_DiaUrb$Consu*dc_DiaUrb$FEX_C)*12

ConsuPer.pc <- Consumo/v.poblacion


