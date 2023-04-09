##.....................................................................
## Implicit Prices dataset

# Unidad de medida
# 1  1. Centímetros Cúbicos
# 2  2. Litro
# 3  3. Gramos
# 4  4. Onzas
# 5  5. Libra
# 6  6. Kilo
# 7  7. Arroba
# 8  8. Otra

# Bread and Grain based staples	3
# Meats and animal-based products 5	
# Milk	2
# Fruits and vegetables 5	
# Sweets and candies	5
# Condiments and snacks 3
# Tea, cacao and coffee	3
# Water 2
# SSBs 2
# Beer 1
# Yogurt 2


# Equivalencias

# 1 ->2 /1000
# 4 ->4 /33.814
# 
# 3 ->5 /500
# 6 ->5 *2
# 7 ->5 *25

##.....................................................................
## Gastos diarios Personal Urbano

dc_PerUrb <- copy(dt_PerUrb)

dc_PerUrb <- fclasicoicop(data=dc_PerUrb,col="NC4_CC_P1_1")

# Subset no Missing Item
dc_PerUrb <- dc_PerUrb[Item!="",]

# Agrega region, zona y tercil de ingreso para variabilidad en precios
dc_PerUrb <- merge(dc_PerUrb,Data[,c("DIRECTORIO","DOMINIO","REGION","TercilIngr","P3"),with=FALSE],
                   by="DIRECTORIO",all.x = TRUE)

#View(head(dt_PerUrb))

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





