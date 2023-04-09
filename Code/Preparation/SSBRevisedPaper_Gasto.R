

### Poner restriccion fisiologica de cantidades a todas las bebidas

## UNIT VALUES------

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates5.dta"))

Data <- as.data.table(Data)
UrbanoTotal <- readRDS(file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))
RuralTotal  <- readRDS(file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))

total_2 <- rbind(UrbanoTotal,RuralTotal)

Data[,Cluster:=substr(VIVIENDA,1,8)]

# Logaritmo para incluir el ingreso en las regresiones
Data[,IT:=log(IT)]

# Ouliers en ingreso
CovariableSubSet <- Data[,c("DIRECTORIO","FEX_C", "REGION", "DOMINIO",
                            "TercilIngr","Quintil", "xrural","Cluster"),with=FALSE]

total_2 <- as.data.table(total_2)

#Limpia los Gasto 99 que corresponde a no respuesta en formulario
total_2[GastoMes<100,GastoMes:=NA]


total_3 <- copy(total_2)
total_3[,Canal:="Total"]

total_2 <- rbind(total_2,total_3)

#Limpiar
rm(total_3)
canal <- c("Hogar","Total","Fuera")

bienes <- unique(total_2$Item)

dt_consumo <- c()


for(ii in canal){

  cat(paste("Canal",ii), sep='\n')

  total_3 <- total_2[Canal==ii,]

  IdSubSet <- Data[,c("DIRECTORIO"),with=FALSE]

  ## Gasto----
  #Restriccion fisica de 16 x #miembros#30
  # ConsumoRestringe <- total_3[,list(Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO")]
  # Miembros <- Data[,c("DIRECTORIO","xtamanioh"),with=FALSE]
  # ConsumoRestringe <- merge(ConsumoRestringe,Miembros,by=c("DIRECTORIO"),all.x = T)
  # ConsumoRestringe[,restriccion:=16*xtamanioh*30]
  # ConsumoRestringe <- ConsumoRestringe[Cantidades>restriccion,]
  
  ConsumoMesTotal <- total_3[,list(Gasto=sum(GastoMes,na.rm = T),
                                   Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]

  # summary(ConsumoMesTotal$Gasto)
  # summary(ConsumoMesTotal$Cantidades)
  
  ConsumoMesTotal[,Gasto:=remove_outliers(Gasto,k=3),by="Item"]
  ConsumoMesTotal[,Cantidades:=remove_outliers(Cantidades,k=3),by="Item"]
  # summary(ConsumoMesTotal$Gasto)
  # summary(ConsumoMesTotal$Cantidades)
  # 
  # report_na <- ConsumoMesTotal[,list(Hogares=length(unique(DIRECTORIO)),
  #                                    Missing=sum(as.numeric(is.na(Gasto)))),by=list(Item)] 
  # report_na[,pje:=Missing/Hogares]
  ConsumoMesTotal <- reshape(ConsumoMesTotal,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  ConsumoMesTotal_codi <- total_3[Texto==0,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotal_codi[,Gasto:=remove_outliers(Gasto,k=3),by="Item"]
  ConsumoMesTotal_codi[,Cantidades:=remove_outliers(Cantidades,k=3),by="Item"]
  
  ConsumoMesTotal_codi <- reshape(ConsumoMesTotal_codi,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Gasto", replacement = "GastoCodi")  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Cantidades", replacement = "CantidadesCodi")
  
  ConsumoMesTotaltexto <- total_3[Texto==1,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotaltexto[,Gasto:=remove_outliers(Gasto,k=3),by="Item"]
  ConsumoMesTotaltexto[,Cantidades:=remove_outliers(Cantidades,k=3),by="Item"]
  
  ConsumoMesTotaltexto <- reshape(ConsumoMesTotaltexto,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotaltexto) <- gsub(x = names(ConsumoMesTotaltexto), pattern = "Gasto", replacement = "GastoTexto")
  names(ConsumoMesTotaltexto) <- gsub(x = names(ConsumoMesTotaltexto), pattern = "Cantidades", replacement = "CantidadesTexto")
  
  ConsumoMes <- merge(ConsumoMesTotal,ConsumoMesTotal_codi,by=c("DIRECTORIO"),all.x = T)
  ConsumoMes <- merge(ConsumoMes,ConsumoMesTotaltexto,by=c("DIRECTORIO"),all.x = T)
  
  ConsumoMes <- merge(IdSubSet,ConsumoMes,by=c("DIRECTORIO"),all.x = T)
  
  setnafill(ConsumoMes, fill = 0)
  
  ConsumoMes[,Canal:=ii]
  
  dt_consumo <- rbind(dt_consumo,ConsumoMes,fill=T)
  #save.dta13(ConsumoMes,file = paste0(wdd_out,"Gasto_",ii,".dta"))
  
  # Limpiar
  rm(ConsumoMes,ConsumoMesTotal,ConsumoMesTotaltexto,ConsumoMesTotal_codi)
  
   }

  dt_consumo_hogar <- dt_consumo[Canal==ii,]
  dt_precios_hogar <- dt_precios[Canal==ii,c("DIRECTORIO","Item","PrecioM"),with=FALSE]

  dt_precios_hogar <- as.data.frame(dt_precios_hogar)
  dt_precios_hogar %<>% pivot_wider(names_from = Item,
                                    names_prefix = "PrecioM_",
                                    values_from = PrecioM)


  save.dta13(dt_precios_hogar,file = paste0(wdd_out,"QualityAdjustedPrices_",ii,".dta"))
  save.dta13(dt_consumo_hogar,file = paste0(wdd_out,"Gasto_",ii,".dta"))

}

