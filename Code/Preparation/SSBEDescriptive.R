##.....................................................................
## Cuadro Consumo hogares

Data <- saveRDS(Data,file=paste0(wdd_out,"DataEstimates.rds"))

# Categoría	N con Gasto>0	%	"Promedio
# Gasto >0"	"Desv. Est
# Gasto >0"	"Minimo
# Gasto >0"	"Maximo
# Gasto >0"

# 
# 
# dc_consumo <- Data[,c("Expe_Bread","Expe_Meat","Expe_Fruits","Expe_Candies","Expe_Coffe"        
#                       "Expe_SSB"           "Expe_Milk"          "Expe_Snacks"        "Expe_Yogurt"        "Expe_Water"        
#                       "Expe_Beer"  ),with=FALSE]
# 
# 
# dc_consumo <- Data[,list(HTHogares=.N,
#                          HogaresConume=sum(as.)),by=list(Item)]






##.....................................................................
## Household data set
# P3 Clase
# P6008 Total de personas en el hogar 

dc_vivi <- dt_vivi[,c("DIRECTORIO","P3",
                      "P6008"),with=FALSE]

##.....................................................................
## People

# P6020 Sexo: 1 - Hombre 2 - Mujer
# P6040 ¿Cuántos años cumplidos tiene ... ?
# P6050	¿Cuál es el parentesco de ... con el ó la jefe del hogar? 

dc_pers <- dt_pers[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","FEX_C",
                      "P6020","P6040","P6050"),with=FALSE]

# sex
dc_pers[,sexo:=ifelse(P6020==1,"Hombre","Mujer")]
# age
dc_pers[,gedad:=cut(P6040,breaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                    labels = c("1","7","12","17","22","27","32","37","42","47","52","57","62","67","72",
                               "77","82","85"),
                    right = FALSE)]

dc_pers <- merge(dc_pers,dc_vivi,by=c("DIRECTORIO"),all.x = TRUE)



## Grafico de cantidades y gasto=====

Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))

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

dc_DiaUrb[,Expe:=NH_CGDU_P8*FactorMensual]

dc_Gasto_h  <- dc_DiaUrb[,list(Cantidad=.N,
                             Expe=sum(Expe)),by=list(DIRECTORIO,Item)]

#.....................................................................
## Gastos diarios Personal Urbano

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

dc_PerUrb[,Expe:=NC4_CC_P5*FactorMensual]

dc_Gasto_P  <- dc_PerUrb[,list(Cantidad=.N,
                               Expe=sum(Expe)),by=list(DIRECTORIO,Item)]

dc_Gasto <- rbind(dc_Gasto_h,dc_Gasto_P)

dc_Gasto <- dc_Gasto[,list(Cantidad=sum(Cantidad),
                               Expe=sum(Expe)),by=list(DIRECTORIO,Item)]

dc_Gasto <- merge(dc_Gasto,Data[,c("DIRECTORIO","FEX_C","ITPC","percentile"),with=FALSE],by=c("DIRECTORIO"),all.y = TRUE)

# Llena missing con 0
cols <- c("Cantidad","Expe")
dc_Gasto[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]

# Dummy gasto positivo
dc_Gasto[,Di_SSB:=as.numeric(Expe>0)]

# Cantidades

dc_Gasto <- dc_Gasto[Expe>0,]

dc_Gasto <- dc_Gasto[,Litros:=Expe/2258]

vp.01 <- quantile(dc_Gasto$Litros,0.01)
vp.95 <- quantile(dc_Gasto$Litros,0.95)

summary(dc_Gasto[dc_Gasto$Litros>0 & dc_Gasto$Litros>vp.01 & dc_Gasto$Litros<vp.95,])

g <- ggplot(dc_Gasto[Litros>0 & Litros>vp.01 & Litros<vp.95,], aes(x=Litros)) 
g <- g + geom_histogram(aes(y=..density.., weight = FEX_C), colour="black", fill="blue")
g <- g + geom_density(alpha=.2, fill="blue") 
g <- g + ggtitle("") 
g <- g + ylab("Densidad/Frecuencia") + xlab("Litros")
g <- g + theme_bw()
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
#g <- g + labs(caption = "Notas:  se pondera con los factores de expansión de la encuesta. \n cuadernillo gastos diarios del hogar y gastos personales diario.")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g

ggsave(file = paste0(wdr_gra,"SSBDensidadCantidades",".jpg"), plot = g, device = "jpg",height = 10, width = 18)

# Gasto mensual

vp.01 <- quantile(dc_Gasto$Expe,0.01)
vp.95 <- quantile(dc_Gasto$Expe,0.95)

summary(dc_Gasto[Di_SSB>0 & Expe>vp.01 & Expe<vp.95,])


g <- ggplot(dc_Gasto[Di_SSB>0 & Expe>vp.01 & Expe<vp.95,], aes(x=Expe/1000)) 
g <- g + geom_histogram(aes(y=..density.., weight = FEX_C), colour="black", fill="blue")
g <- g + geom_density(alpha=.2, fill="blue") 
#g <- g + ggtitle("Gasto mensual del hogar. Hogares con gasto reportado mayor a 0") 
g <- g + ylab("Densidad/Frecuencia") + xlab("Miles de COP")
g <- g + theme_bw()
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
#g <- g + labs(caption = "Notas:  se pondera con los factores de expansión de la encuesta. 
#                        Cuadernillo gastos diarios del hogar y gastos personales diario.
#                        Gasto mensualizado con equivalencias de la encuesta.")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g

ggsave(file = paste0(wdr_gra,"SSBDensidadGasto",".jpg"), plot = g, device = "jpg",height = 10, width = 18)


# Dummy gasto por quntil de ingreso

dc_Gasto[ , Quintil:= cut(ITPC,
                        breaks = quantile(ITPC, probs = 0:5/5),
                        labels = 1:5, right = FALSE)]

dc_quintil <- dc_Gasto[,list(Hogares=sum(FEX_C)),by=list(Quintil,Di_SSB)]
dc_quintil <- na.omit(dc_quintil)

dc_quintil[,Total:=sum(Hogares),by=c("Quintil")]
dc_quintil[,share:=Hogares/Total*100]

dc_quintil[,condition:=ifelse(Di_SSB==1,"Si","No")]

g <- ggplot(dc_quintil, aes(fill=condition, y=share, x=Quintil)) 
g <- g + geom_bar(position = "stack",stat="identity") 
g <- g + ggtitle("Gasto mensual en BA reportado mayor a 0") 
g <- g + scale_fill_discrete(name = "")
g <- g + ylab("Porcentaje de hogares (%)") + xlab("Quintil de ingreso")
g <- g + theme_bw()
g <- g + theme(legend.position="bottom")
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=14))
#g <- g + labs(caption = "Notas:  se pondera con los factores de expansión de la encuesta. 
#              Cuadernillo gastos diarios del hogar y gastos personales diario.")
#g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=34),
               axis.text=element_text(size=30),
               axis.title=element_text(size=34,face="bold"))
g 

ggsave(file = paste0(wdr_gra,"SSBDummyGastoQuintil",".jpg"), plot = g, device = "jpg",height = 10, width = 18)



## Grafico de precios=====

dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")

# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item=="SSB",]

# Armonizar valores para precio


# 5.2 Unidad de Medida
# 
# 1 » 1. Centímetros Cúbicos
# 2 » 2. Litro
# 3 » 3. Gramos
# 4 » 4. Onzas
# 5 » 5. Libra
# 6 » 6. Kilo
# 7 » 7. Arroba
# 8 » 8. Otra


table(dc_DiaUrb$NH_CGDU_P3)

# 1->2 *1000
# 4-> *0,0295735 33,814

dc_DiaUrb[,Equivalencia:=NA]
dc_DiaUrb[,Equivalencia:=ifelse(NH_CGDU_P3==1,0.001,Equivalencia)]
dc_DiaUrb[,Equivalencia:=ifelse(NH_CGDU_P3==2,1,Equivalencia)]
dc_DiaUrb[,Equivalencia:=ifelse(NH_CGDU_P3==4,0.0295735,Equivalencia)]

# Percentil 90
v.p1 <- quantile(dc_DiaUrb$NH_CGDU_P8[dc_DiaUrb$NH_CGDU_P3==2],0.01)
v.p95 <- quantile(dc_DiaUrb$NH_CGDU_P8[dc_DiaUrb$NH_CGDU_P3==2],0.95)

dc_DiaUrb <- dc_DiaUrb[NH_CGDU_P3==2 & NH_CGDU_P8<v.p95 & NH_CGDU_P8>v.p1,]

options(scipen = 9999)
g <- ggplot(dc_DiaUrb, aes(x=NH_CGDU_P8)) 
g <- g + geom_histogram(aes(y=..density.., weight = FEX_C), colour="black", fill="blue")
g <- g + geom_density(alpha=.2, fill="blue") 
#g <- g + ggtitle("Valor pagado por litro") 
g <- g + ylab("Densidad/Frecuencia") + xlab("Pesos colombianos (COP)")
g <- g + theme_bw()
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
#g <- g + labs(caption = "Notas: cuadernillo gastos diarios del hogar. Valores recortados en el percentil 95")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g

ggsave(file = paste0(wdr_gra,"SSBDensidadPrecio",".jpg"), plot = g, device = "jpg",height = 10, width = 18)


## Grafico de ingreso per capita del hogar=====

options(scipen = 9999)
Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))

v.p99 <- quantile(Data$ITPC,0.99)

g <- ggplot(Data[ITPC<v.p99], aes(x=ITPC/1000)) 
g <- g + geom_histogram(aes(y=..density.., weight = FEX_C), colour="black", fill="white")
g <- g + geom_density(alpha=.2, fill="blue") 
g <- g + ggtitle("") 
g <- g + ylab("Densidad/Frecuencia") + xlab("Pesos colombianos (COP)")
g <- g + theme_bw()
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
#g <- g + labs(caption = "Notas: se pondera con los factores de expansión de la encuesta. Valores recortados en el percentil 99")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g

ggsave(file = paste0(wdr_gra,"SSBDensidadIngresoPC",".jpg"), plot = g, device = "jpg",height = 10, width = 18)


## Grafico de cantidades consumdidas por hogar



##.....................................................................
# Maps

#install.packages(c("rgeos","ggmap","RgoogleMaps"))

##.....................................................................
# Consumption frequency

dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")
# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item!="",]

dc_DiaUrb <- dc_DiaUrb[Item=="SSB",]

dc_DiaUrb <- dc_DiaUrb[,list(Hogares=sum(FEX_C)),by=list(NH_CGDU_P9)]

dc_DiaUrb[,share:=Hogares/sum(Hogares)*100]

# 9. Frecuencia de Compra
# 1 » 2. Diario
# 2 » 2.1. Varias veces por semana
# 3 » 3. Semanal
# 4 » 4. Quincenal
# 5 » 5. Mensual
# 6 » 6. Bimestral
# 7 » 7. Trimestral
# 9 » 9. Esporádica

dc_DiaUrb[,Frecuencia:="No responde"]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==1,"Diario",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==2,"Var. veces/semana",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==3,"Semanal",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==4,"Quincenal",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==5,"Mensual",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==6,"Bimestral",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==7,"Trimestral",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P9==9,"Esporadica",Frecuencia)]


# levels_<- c("<40","40-160","160-400",	"400-800","800-1200","1200-1600",">1600")
# outfile$Position<- factor(outfile$rango_duracion, levels = levels_)

dc_DiaUrb$Frecuencia <- factor(dc_DiaUrb$Frecuencia, levels = dc_DiaUrb$Frecuencia[order(-dc_DiaUrb$share)])

g <- ggplot(dc_DiaUrb,aes(x=Frecuencia, y=share,ymax=share*1.2))
g <- g + labs(x="Frecuencia de compra", y="")
#g <- g + labs(caption = "Notas: participaciones estimadas utilizando los factores de expansion de la ENPH 2017")
g <- g + geom_bar(colour="blue", stat="identity",fill="blue") 
g <- g + guides(fill=FALSE)
g <- g + theme_bw()
g <- g + theme(plot.margin = unit(c(0,0,0,0), "cm"))
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.y=element_blank())
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g <- g + geom_text(aes(y = share, label = paste(round(share,1),"%",sep="")), vjust=-0.2, color="black",position = position_dodge(0.9), size=20)
g

ggsave(plot=g, file= paste0(wdr_gra, "SSBFrecuenciaCompraHogar.jpg"), height = 10, width = 18)



##.....................................................................
# Unidad de medida frecuencia

dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")
# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item!="",]

dc_DiaUrb <- dc_DiaUrb[Item=="SSB",]

dc_DiaUrb <- dc_DiaUrb[,list(Hogares=sum(FEX_C)),by=list(NH_CGDU_P3)]

dc_DiaUrb[,share:=Hogares/sum(Hogares)*100]

# unidad de medida
# 1	Centímetros Cúbicos
# 2	Litro
# 3	Gramos
# 4	Onzas
# 5	Libra
# 6	Kilo
# 7	Arroba
# 8	Otra


dc_DiaUrb[,Frecuencia:="No responde"]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==1,"Cm3",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==2,"Litro",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==3,"Gramos",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==4,"Onzas",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==5,"Libra",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==6,"Kilo",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==7,"Arroba",Frecuencia)]
dc_DiaUrb[,Frecuencia:=ifelse(NH_CGDU_P3==8,"Otra",Frecuencia)]


# levels_<- c("<40","40-160","160-400",	"400-800","800-1200","1200-1600",">1600")
# outfile$Position<- factor(outfile$rango_duracion, levels = levels_)

dc_DiaUrb$Frecuencia <- factor(dc_DiaUrb$Frecuencia, levels = dc_DiaUrb$Frecuencia[order(-dc_DiaUrb$share)])

g <- ggplot(dc_DiaUrb,aes(x=Frecuencia, y=share,ymax=share*1.2))
g <- g + labs(x="Unidade de medida", y="")
#g <- g + labs(caption = "Notas: participaciones estimadas utilizando los factores de expansion de la ENPH 2017")
g <- g + geom_bar(colour="blue", stat="identity",fill="blue") 
g <- g + guides(fill=FALSE)
g <- g + theme_bw()
g <- g + theme(plot.margin = unit(c(0,0,0,0), "cm"))
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.y=element_blank())
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=40),
               axis.text=element_text(size=36),
               axis.title=element_text(size=40,face="bold"))
g <- g + geom_text(aes(y = share, label = paste(round(share,1),"%",sep="")), vjust=-0.2, color="black",position = position_dodge(0.9), size=20)
g

ggsave(plot=g, file= paste0(wdr_gra, "SSBUnidadMedida.jpg"), height = 10, width = 18)











# Agrega region, zona y tercil de ingreso para variabilidad en precios
dc_DiaUrb <- merge(dc_DiaUrb,Data[,c("DIRECTORIO","DOMINIO","TercilIngr","P3"),with=FALSE],by="DIRECTORIO",all.x = TRUE)


# Boxplot by city

dc_BpPrice <- dc_DiaUrb[Item=="SSB",]
v_UniCom <- unique(dc_BpPrice$NH_CGDU_P3)
v_strata <- unique(dc_BpPrice$DOMINIO)

for(ii in 1:length(v_UniCom)){
  
  dc_BpPrice_ <- dc_BpPrice[NH_CGDU_P3==v_UniCom[ii],]
  
  g <- ggplot(dc_BpPrice_, aes(x = DOMINIO, y = NH_CGDU_P8/NH_CGDU_P2)) + geom_boxplot()
  g <- g + labs(title=paste0("Unidad ",v_UniCom[ii]), x = "", y = "ValorReportado")
  g <- g + guides(shape = FALSE)
  g <- g + theme_minimal() 
  g <- g + theme_bw() 
  g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
  g
  
  ggsave(file = paste0(wdr_gra,"SSBCity","Unidad",v_UniCom[ii], ".jpg"), plot = g, device = "jpg")
  
}




# Boxplot by income

dc_BpPrice <- dc_DiaUrb[Item=="SSB",]
v_UniCom <- unique(dc_BpPrice$NH_CGDU_P3)
v_strata <- as.character(unique(dc_BpPrice$TercilIngr))

for(ii in 1:length(v_UniCom)){
  
  dc_BpPrice_ <- dc_BpPrice[NH_CGDU_P3==v_UniCom[ii],]
  
  g <- ggplot(dc_BpPrice_, aes(x = as.character(TercilIngr), y = NH_CGDU_P8/NH_CGDU_P2)) + geom_boxplot()
  g <- g + labs(title=paste0("Unidad ",v_UniCom[ii]), x = "", y = "ValorReportado")
  g <- g + guides(shape = FALSE)
  g <- g + theme_minimal() 
  g <- g + theme_bw() 
  g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
  g
  
  ggsave(file = paste0(wdr_gra,"SSBIncome","Unidad",v_UniCom[ii], ".jpg"), plot = g, device = "jpg")
  
}











# Implicit prices boxplot
v_items <- unique(dc_DiaUrb$Item)

#dc_BpPrice <- dc_DiaUrb[Item==v_items[6],]
#dc_BpPrice <- copy(dc_DiaUrb)
v_UniCom <- unique(dc_DiaUrb$NH_CGDU_P3)

for(ii in 1:length(v_UniCom)){
  
  dc_BpPrice <- dc_DiaUrb[NH_CGDU_P3==v_UniCom[ii],]
  
  g <- ggplot(dc_BpPrice, aes(x = Item, y = NH_CGDU_P8/NH_CGDU_P2)) + geom_boxplot()
  g <- g + labs(title=paste0("Unidad ",v_UniCom[ii]), x = "", y = "ValorReportado")
  g <- g + guides(shape = FALSE)
  g <- g + theme_minimal() 
  g <- g + theme_bw() 
  g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
  g
  
  ggsave(file = paste0(wdr_gra,"Unidad",v_UniCom[ii], ".jpg"), plot = g, device = "jpg")
  
}

#Precios-----
Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))

dc_PerUrb <- copy(dt_PerUrb)

dc_PerUrb <- fclasicoicop(data=dc_PerUrb,col="NC4_CC_P1_1")

# Subset no Missing Item
dc_PerUrb <- dc_PerUrb[Item!="",]

# Agrega region, zona y tercil de ingreso para variabilidad en precios
dc_PerUrb <- merge(dc_PerUrb,Data[,c("DIRECTORIO","DOMINIO","REGION","TercilIngr","P3"),with=FALSE],
                   by="DIRECTORIO",all.x = TRUE)

# Solo articulos comprados
dc_PerUrb <- dc_PerUrb[NC4_CC_P3==1,]

dc_PerUrb[,Pr:=NC4_CC_P5/NC4_CC_P2]

dc_vunitarioP <- dc_PerUrb[,list(Pr=weighted.mean(x=Pr,w=FEX_C,na.rm=TRUE)),
                           by=c("Item","DIRECTORIO")]

##.....................................................................
## Gasto del hogar
dc_DiaUrb <- copy(dt_DiaUrb)

dc_DiaUrb <- fclasicoicop(data=dc_DiaUrb,col="NH_CGDU_P1")
# Subset no Missing Item
dc_DiaUrb <- dc_DiaUrb[Item!="",]

# Agrega region, zona y tercil de ingreso para variabilidad en precios
dc_DiaUrb <- merge(dc_DiaUrb,Data[,c("DIRECTORIO","DOMINIO","REGION","TercilIngr","P3"),with=FALSE],by="DIRECTORIO",all.x = TRUE)

# Prices table
# Remover valores reportados en otras unidades
dc_DiaUrb <- dc_DiaUrb[NH_CGDU_P3!=8,]

# Solo articulos comprados
dc_DiaUrb <- dc_DiaUrb[NH_CGDU_P5==1,]

# Tabla de unidades reportadas
dc_unidades <- dc_DiaUrb[,list(N=.N),by=list(Item,NH_CGDU_P3)]

dc_unidades <- reshape(dc_unidades,idvar = "Item",timevar = "NH_CGDU_P3"
                       ,direction = "wide")


# length(unique(dc_Price$REGION))
# length(unique(dc_Price$Item))

# Dimension del vector de precios
v.prdim <- length(unique(dc_DiaUrb$DOMINIO))*length(unique(dc_DiaUrb$TercilIngr))
v.prdim


# Estandarizar

# dc_DiaUrb[,vestandar:=NA]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==1,NH_CGDU_P8/1000,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==2,NH_CGDU_P8,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==3,NH_CGDU_P8/500,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==4,NH_CGDU_P8/33.814,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==5,NH_CGDU_P8,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==6,NH_CGDU_P8*2,vestandar)]
# dc_DiaUrb[,vestandar:=ifelse(NH_CGDU_P3==7,NH_CGDU_P8*25,vestandar)]
#dc_DiaUrb[,Pre:=vestandar/qestandar]

dc_DiaUrb[,qestandar:=NA]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==1,NH_CGDU_P2/1000,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==2,NH_CGDU_P2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==3,NH_CGDU_P2/500,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==4,NH_CGDU_P2/33.814,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==5,NH_CGDU_P2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==6,NH_CGDU_P2*2,qestandar)]
dc_DiaUrb[,qestandar:=ifelse(NH_CGDU_P3==7,NH_CGDU_P2*25,qestandar)]

dc_DiaUrb[,Pr:=NH_CGDU_P8/qestandar]



# plot(density(dc_DiaUrb$Pr))
# lines(density(dc_DiaUrb$Pre))

View(head(dc_DiaUrb,100))

# Recortar en percentil 95

v_items <- unique(dc_DiaUrb$Item)
dc_DiaUrb[,PrR:=Pr]

for(ii in 1:length(v_items)){
  
  cat(paste0("Item ",v_items[ii]),sep = "\n") 
  dc_BpPrice <- dc_DiaUrb[Item==v_items[ii],]
  # Recortar p95
  v.p95 <- quantile(dc_BpPrice$Pr,0.95)
  # Recortar p1
  v.p01 <- quantile(dc_BpPrice$Pr,0.01)
  
  dc_DiaUrb[Item==v_items[ii],PrR:=ifelse(Pr>v.p95,NA,PrR)]
  dc_DiaUrb[Item==v_items[ii],PrR:=ifelse(Pr<v.p01,NA,PrR)]
}


# 
# # Implicit prices boxplot
# v_items <- unique(dc_DiaUrb$Item)
# 
# 
# for(ii in 1:length(v_items)){
#   
#   dc_BpPrice <- dc_DiaUrb[Item==v_items[ii],]
#   # Recortar p95
#   v.p95 <- quantile(dc_BpPrice$Pr,0.95)
#     
#   g <- ggplot(dc_DiaUrb[Pr<v.p95,], aes(x = REGION, y = Pr)) + geom_boxplot()
#   g <- g + labs(title=paste0("Item ",v_items[ii]), x = "", y = "ValorReportado")
#   g <- g + guides(shape = FALSE)
#   g <- g + theme_minimal() 
#   g <- g + theme_bw() 
#   g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
#   g
#   
#   ggsave(file = paste0(wdr_gra,"Item",v_items[ii], ".jpg"), plot = g, device = "jpg")
#   
# }


# Cuadro precios medios por item
dc_Price <- dc_DiaUrb[,list(Promedio=weighted.mean(x=PrR,w=FEX_C,na.rm=TRUE),
                            Desv=sqrt(wtd.var(PrR, w=FEX_C,na.rm=TRUE)),
                            Min=min(PrR,na.rm=TRUE),
                            Max=max(PrR,na.rm=TRUE)),
                      by=c("Item")]

dc_Price[,ItemE:=Item]
dc_Price[,ItemE:=ifelse(Item=="SSB","01BA",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Milk","02Leche",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Yogurt","03Yogurt",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Water","04Agua",ItemE)]

dc_Price[,ItemE:=ifelse(Item=="Meat","05Carnes",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Fruits","06Frutas",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Bread","07Pan",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Coffe","08Cafe",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Candies","09Dulces",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Snacks","10Snacks",ItemE)]

dc_Price[,ItemE:=ifelse(Item=="Beer","11Cerveza",ItemE)]
dc_Price <- dc_Price[order(dc_Price$ItemE),]

fwrite(dc_Price,file=paste0(wd_resu,"CuadroPreciosMedios.csv"))

# Barplot precios medios por item estrato

dc_Price <- dc_DiaUrb[,list(Promedio=weighted.mean(x=PrR,w=FEX_C,na.rm=TRUE)),
                      by=c("Item","TercilIngr")]

dc_Price[,ItemE:=Item]
dc_Price[,ItemE:=ifelse(Item=="SSB","01BA",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Milk","02Leche",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Yogurt","03Yogurt",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Water","04Agua",ItemE)]

dc_Price[,ItemE:=ifelse(Item=="Meat","05Carnes",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Fruits","06Frutas",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Bread","07Pan",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Coffe","08Cafe",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Candies","09Dulces",ItemE)]
dc_Price[,ItemE:=ifelse(Item=="Snacks","10Snacks",ItemE)]

dc_Price[,ItemE:=ifelse(Item=="Beer","11Cerveza",ItemE)]

dc_Price <- dc_Price[Item!="Beer",]

g <- ggplot(dc_Price,aes(fill=TercilIngr, y=Promedio, x=ItemE))
g <- g + geom_bar(position="dodge", stat="identity") 
g <- g + labs(x="", y="Valor Unitario (COP)")
#g <- g + labs(caption = "Notas: participaciones estimadas utilizando los factores de expansion de la ENPH 2017")
#g <- g + guides(fill=FALSE)
g <- g + theme_bw()
g <- g + theme(legend.position="bottom")
g <- g + theme(plot.margin = unit(c(0,0,0,0), "cm"))
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + scale_fill_discrete(name = "",labels = c("T1","T2","T3"))
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
#g <- g + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.y=element_blank())
g <- g + theme(
  plot.caption = element_text(hjust = 0, size= 20),
  legend.text = element_text(size=34),
  axis.text=element_text(size=30),
  axis.title=element_text(size=34,face="bold"))
g

ggsave(plot=g, file= paste0(wdr_gra, "PreciosQUAIDSTercil.jpg"), height = 10, width = 18)



# Boxplot precios item

dc_DiaUrb[,ItemE:=Item]
dc_DiaUrb[,ItemE:=ifelse(Item=="SSB","01BA",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Milk","02Leche",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Yogurt","03Yogurt",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Water","04Agua",ItemE)]

dc_DiaUrb[,ItemE:=ifelse(Item=="Meat","05Carnes",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Fruits","06Frutas",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Bread","07Pan",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Coffe","08Cafe",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Candies","09Dulces",ItemE)]
dc_DiaUrb[,ItemE:=ifelse(Item=="Snacks","10Snacks",ItemE)]

dc_DiaUrb[,ItemE:=ifelse(Item=="Beer","11Cerveza",ItemE)]


g <- ggplot(dc_DiaUrb[!(Item %in% c("Beer","Coffe","Snacks"))], aes(x = ItemE, y = PrR,fill=ItemE))
g <- g + geom_boxplot()
g <- g + scale_fill_brewer(palette="PuOr")
g <- g + labs(title="", x = "", y = "Valor Unitario (COP)")
g <- g + guides(shape = FALSE)
g <- g + theme_bw()
g <- g + theme(legend.position= "none")
g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=34),
               axis.text=element_text(size=30),
               axis.title=element_text(size=34,face="bold"))
g

ggsave(file = paste0(wdr_gra,"PreciosQUAIDSNCS",".jpg"), plot = g, device = "jpg", height = 10, width = 18)


g <- ggplot(dc_DiaUrb[Item %in% c("Snacks","Coffe") & PrR<30000], aes(x = ItemE, y = PrR,fill=ItemE))
g <- g + geom_boxplot()
g <- g + scale_fill_brewer(palette="PuOr")
g <- g + labs(title="", x = "", y = "Valor Unitario (COP)")
g <- g + guides(shape = FALSE)
g <- g + theme_bw()
g <- g + theme(legend.position= "none")
g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
g <- g + theme(plot.caption = element_text(hjust = 0, size= 20),
               legend.text = element_text(size=34),
               axis.text=element_text(size=30),
               axis.title=element_text(size=34,face="bold"))
g

ggsave(file = paste0(wdr_gra,"PreciosQUAIDSCS",".jpg"), plot = g, device = "jpg", height = 10, width = 18)

# Tabla de valores unitarios

dc_vunitario <- dc_DiaUrb[,list(Pr=weighted.mean(x=PrR,w=FEX_C,na.rm=TRUE)),
                          by=c("Item","DIRECTORIO")]


dc_vunitario <- rbind(dc_vunitario,dc_vunitarioP)

dc_vunitario <- dc_DiaUrb[,list(Pr=mean(Pr,na.rm=TRUE)),
                          by=c("Item","DIRECTORIO")]

#View(head(dc_vunitario,1000))


# Recortar e imputar mediana percentil 95

v_items <- unique(dc_DiaUrb$Item)
dc_vunitario[,PrR:=Pr]

for(ii in 1:length(v_items)){
  
  cat(paste0("Item ",v_items[ii]),sep = "\n") 
  dc_BpPrice <- dc_vunitario[Item==v_items[ii],]
  # Recortar p95
  v.p95 <- quantile(dc_BpPrice$Pr,0.95)
  # Recortar p1
  v.p01 <- quantile(dc_BpPrice$Pr,0.01)
  # mediana
  v.p05 <- quantile(dc_BpPrice$Pr,0.05)
  dc_vunitario[Item==v_items[ii],PrR:=ifelse(Pr>v.p95,v.p05,PrR)]
  dc_vunitario[Item==v_items[ii],PrR:=ifelse(Pr<v.p01,v.p05,PrR)]
}


setnames(dc_vunitario,"PrR","Vu")

dc_vunitario[,c("Pr"):=NULL]

# Reshape precios por ciudad
dc_vunitario <- reshape(dc_vunitario,idvar=c("DIRECTORIO"),timevar="Item",direction = "wide",sep = "_")



# Tabla de precios
dc_Price <- dc_DiaUrb[,list(Pr=weighted.mean(x=PrR,w=FEX_C,na.rm=TRUE)),
                      by=c("Item","DOMINIO","REGION","TercilIngr")]

dc_Price <- dc_Price[,c("Item","DOMINIO","REGION","TercilIngr","Pr")]

# Reshape precios por ciudad
dc_Price <- reshape(dc_Price,idvar=c("DOMINIO","REGION","TercilIngr"),timevar="Item",direction = "wide",sep = "_")



# Join to data set
Data <- merge(Data,dc_Price,by=c("DOMINIO","REGION","TercilIngr"),all.x = TRUE)
Data <- merge(Data,dc_vunitario,by=c("DIRECTORIO"),all.x = TRUE)

fwrite(dc_Price,file=paste0(wdd_out,"dc_Price.csv"))
# Limpiar
rm(dc_Price,dc_vunitario)







