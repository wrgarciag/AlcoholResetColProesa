##.....................................................................
## Implicit Prices dataset

dt_pr <- as.data.table(readRDS(file=paste0(wdd_out,"DataSSB.rds")))

dt_pr <- merge(dt_pr,Data[,c("DIRECTORIO","TercilIngr","REGION","DOMINIO","xrural"),with=FALSE], by=c("DIRECTORIO"), all.x = TRUE)

# Recortar e imputar mediana percentil 95

v_items <- unique(dt_pr$Item)
dt_pr[,PrR:=Precio]

for(ii in 1:length(v_items)){
  
  cat(paste0("Item ",v_items[ii]),sep = "\n") 
  dc_BpPrice <- dt_pr[Item==v_items[ii],]
  # Recortar p95
  v.p95 <- quantile(dc_BpPrice$Precio,0.95,na.rm = TRUE)
  # Recortar p1
  v.p01 <- quantile(dc_BpPrice$Precio,0.01,na.rm = TRUE)
  # mediana
  v.p05 <- quantile(dc_BpPrice$Precio,0.05,na.rm = TRUE)
  dt_pr[Item==v_items[ii],Precio:=ifelse(Precio>v.p95,v.p05,Precio)]
  dt_pr[Item==v_items[ii],Precio:=ifelse(Precio<v.p01,v.p05,Precio)]
}

dt_reg <- Data[,list(n=.N),by=list(TercilIngr,REGION,DOMINIO)]

dt_pr_r <- dt_pr[,list(PrR=mean(Precio,na.rm = TRUE)),
               by=list(TercilIngr,REGION,Item)]

dt_pr <- dt_pr[,list(Precio=mean(Precio,na.rm = TRUE)),
                 by=list(TercilIngr,REGION,DOMINIO,Item)]

dt_pr <- dt_pr[Item!="",]

dt_pr <- reshape(dt_pr,idvar=c("DOMINIO","REGION","TercilIngr"),
                 timevar="Item",direction = "wide",sep = "_")

dt_pr <- melt(dt_pr,id.vars = c("DOMINIO","REGION","TercilIngr"),
              variable.name = "Item",value.name = "Precio")

dt_pr[,Item:=gsub("Precio_","",Item)]
dt_pr <- merge(dt_pr,dt_pr_r,by=c("Item","REGION","TercilIngr"),all.x=TRUE)

dt_pr[,Precio:=ifelse(is.na(Precio),PrR,Precio)]

sum(is.na(dt_pr$Precio))

dt_pr[,Precio2:=mean(Precio,na.rm = TRUE),by=c("Item","REGION")]

dt_pr[,Precio3:=mean(Precio,na.rm = TRUE),by=c("Item")]

dt_pr[,Precio:=ifelse(is.na(Precio),Precio2,Precio)]
dt_pr[,Precio:=ifelse(is.na(Precio),Precio3,Precio)]

sum(is.na(dt_pr$Precio))

dt_pr[,c("PrR","Precio2","Precio3"):=NULL]

fwrite(dt_pr,file=paste0(wdd_out,"DataPrice.csv"))

# Reshape precios por ciudad
dt_pr <- reshape(dt_pr,idvar=c("DOMINIO","REGION","TercilIngr"),
                 timevar="Item",direction = "wide",sep = "_")

Data <- merge(Data,dt_pr,by=c("DOMINIO","REGION","TercilIngr"),all.x = TRUE)

#Limpiar
rm(dt_pr_r)

