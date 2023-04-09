##.....................................................................
## Expenditure dataset

dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")

# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item!="",]

# Agrega region, zona y tercil de ingreso para variabilidad en precios

dc_DiaUrb <- merge(dc_DiaUrb,Data[,c("DIRECTORIO","DOMINIO","TercilIngr","P3"),with=FALSE],by="DIRECTORIO",all.x = TRUE)

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

dc_DiaUrb[,Expe:=NH_CGDU_P8*FactorMensual]

dc_DiaUrb  <- dc_DiaUrb[,list(Expe=sum(Expe)),by=list(DIRECTORIO,Item)]

# Consulta para cuadro de consumo
dc_consumo_h <- copy(dc_DiaUrb)

dc_DiaUrb <- reshape(dc_DiaUrb,idvar="DIRECTORIO",timevar="Item",direction = "wide",sep = "_")

cols <- colnames(dc_DiaUrb)
dc_DiaUrb[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


##.....................................................................
## Gastos diarios Personal Urbano

dc_PerUrb <- copy(dt_PerUrb)

dc_PerUrb <- fclasicoicop(data=dc_PerUrb,col="NC4_CC_P1_1")

# Subset no Missing Item
dc_PerUrb <- dc_PerUrb[Item!="",]

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

dc_PerUrb[,Expe:=NC4_CC_P5*FactorMensual]

dc_PerUrb  <- dc_PerUrb[,list(Expe=sum(Expe)),by=list(DIRECTORIO,Item)]

# Consulta para cuadro de consumo
dc_consumo_p <- copy(dc_PerUrb)

dc_PerUrb <- reshape(dc_PerUrb,idvar="DIRECTORIO",timevar="Item",direction = "wide",sep = "_")

cols <- colnames(dc_PerUrb)
dc_PerUrb[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


# ##.....................................................................
# ## Gastos diarios comida fuera del hogar
# 
# dc_DiaUrbComi <- copy(dt_DiaUrbComi)
# 
# dcc_DiaUrbComi <- fclasicoicop(data=dc_DiaUrbComi,col="NH_CGDUCFH_P1_1")
# 
# # Subset no Missing Item
# dc_DiaUrbComi <- dc_DiaUrbComi[Item!="",]

##.....................................................................
## Consolida

dc_Expe <- rbind(dc_DiaUrb,dc_PerUrb,fill=TRUE)

dc_Expe <- dc_Expe[,lapply(.SD, na.omit(sum)),by=list(DIRECTORIO)]

cols <- colnames(dc_Expe)
dc_Expe[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


# ## generate dummy variables for zero consumption
# dummy <- copy(dc_Expe)
# 
# cols  <- colnames(dummy)[2:length(colnames(dummy))]
# dummy <- data.table(dummy[, (cols) := lapply(.SD, function(x){as.numeric(x>0)}), .SDcols = cols])
# 
# setnames(dummy,old =names(dummy)[2:length(colnames(dummy))],new = paste0("Di_",names(dummy)[2:length(colnames(dummy))]))
# setnames(dummy, names(dummy), gsub("Expe_", "", names(dummy)))
# 
# dc_Expe <- merge(dc_Expe,dummy,by=c("DIRECTORIO"),all.x = TRUE)
# 
# 
# ## Total expenditure
# cols <- grep(pattern ="^[Expe_]" ,
#              x = colnames(dc_Expe),perl = T,value = T)
# 
# dc_Expe[,Expe:=rowSums(.SD),.SDcols=cols]

# Shares

# dc_Expe[,w_Bread:=Expe_Bread/Expe]
# dc_Expe[,w_Meat:=Expe_Meat/Expe]
# dc_Expe[,w_Fruits:=Expe_Fruits/Expe]
# dc_Expe[,w_Candies:=Expe_Candies/Expe]
# dc_Expe[,w_Coffe:=Expe_Coffe/Expe]
# dc_Expe[,w_SSB:=Expe_SSB/Expe]
# dc_Expe[,w_Milk:=Expe_Milk/Expe]
# dc_Expe[,w_Snacks:=Expe_Snacks/Expe]
# dc_Expe[,w_Water:=Expe_Water/Expe]
# dc_Expe[,w_Water:=Expe_Water/Expe]

# Join to data set
Data <- merge(Data,dc_Expe,by=c("DIRECTORIO"),all.x = TRUE)

# Rellenar con 0 gasto missing
## Total expenditure
cols <- grep(pattern ="^[Expe_]" ,
             x = colnames(dc_Expe),perl = T,value = T)

Data[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


# Limpiar
rm(dummy,cols,dc_DiaUrb,dc_PerUrb,dc_Expe)


# Cuadro consumo
dc_consumo <- rbind(dc_consumo_h,dc_consumo_p)

dc_consumo <- merge(dc_consumo,Data[,c("DIRECTORIO","xtamanioh","FEX_C"),with=FALSE],by=c("DIRECTORIO"),all.x = TRUE,all.y = TRUE)

dc_consumo <- dc_consumo[,list(Hogares=uniqueN(DIRECTORIO),
                                Gasto=sum(Expe*FEX_C,na.rm = TRUE),
                                Promedio=weighted.mean(x=Expe,w=FEX_C,na.rm=TRUE),
                                Desv=sqrt(wtd.var(Expe, w=FEX_C,na.rm=TRUE)),
                                Min=min(Expe,na.rm=TRUE),
                                Max=max(Expe,na.rm=TRUE)),
                                by=list(Item)]

dc_consumo[,ItemE:=Item]
dc_consumo[,ItemE:=ifelse(Item=="SSB","01BA",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Milk","02Leche",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Yogurt","03Yogurt",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Water","04Agua",ItemE)]

dc_consumo[,ItemE:=ifelse(Item=="Meat","05Carnes",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Fruits","06Frutas",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Bread","07Pan",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Coffe","08Cafe",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Candies","09Dulces",ItemE)]
dc_consumo[,ItemE:=ifelse(Item=="Snacks","10Snacks",ItemE)]

dc_consumo[,ItemE:=ifelse(Item=="Beer","11Cerveza",ItemE)]

dc_consumo <- dc_consumo[order(dc_consumo$ItemE),]

TotalHogares <- nrow(Data)
dc_consumo[,pjehogares:=Hogares/TotalHogares]
dc_consumo[,w:=Gasto/sum(Gasto,na.rm = TRUE)]

fwrite(dc_consumo,file=paste0(wd_resu,"CuadroConsumoMedio.csv"))

#Limpiar
rm(dc_consumo,dc_consumo_h,dc_consumo_p)
