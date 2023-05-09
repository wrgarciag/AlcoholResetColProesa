

### Poner restriccion fisiologica de cantidades a todas las bebidas

## UNIT VALUES------

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates5.dta"))

Data <- as.data.table(Data)
UrbanoTotal <- readRDS(file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))
RuralTotal  <- readRDS(file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))

total_2 <- rbind(UrbanoTotal,RuralTotal)

Data[,Cluster:=substr(VIVIENDA,1,9)]
clus <- Data[,list(N=.N),by=list(Cluster)]
summary(clus)

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
canal <- c("Total","Hogar","Fuera")

total_2[Item %in% c("MilkDerivatives","HotDrinks"),Item:="Dairy"]
bienes <- unique(total_2$Item)

dt_consumo <- c()
dt_precios <- c()

# consulta de litros consumidos

dt_agregado <- total_2[,list(Gasto=sum(GastoMes*FEX_C,na.rm = T),
                      Cantidades=sum(LitrosMes*FEX_C,na.rm = T)),by=c("Canal","Item","Texto")]

fwrite(dt_agregado,file = paste0(wd_resu,"ConsumoAgregadoLitrosPaper.csv"))

for(ii in canal){
  
  cat(paste("Canal",ii), sep='\n')  
  
  total_3 <- total_2[Canal==ii,]
  
  IdSubSet <- Data[,c("DIRECTORIO"),with=FALSE]
  
  ## Gasto----
  
  ConsumoMesTotal <- total_3[,list(Gasto=sum(GastoMes,na.rm = T),
                                   Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]

  ConsumoMesTotal[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotal[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  summary(ConsumoMesTotal$Gasto)

  ConsumoMesTotal <- reshape(ConsumoMesTotal,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  ConsumoMesTotal_codi <- total_3[Texto==0,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotal_codi[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotal_codi[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  
  ConsumoMesTotal_codi <- reshape(ConsumoMesTotal_codi,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Gasto", replacement = "GastoCodi")  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Cantidades", replacement = "CantidadesCodi")
  
  ConsumoMesTotaltexto <- total_3[Texto==1,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotaltexto[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotaltexto[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  
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
  
  ## Unit value----
  
  unitvalues_2 <- total_3[Canal==ii,list(litrosmes = sum(LitrosMes, na.rm = T),
                                         GastoMes  = sum(GastoMes, na.rm = T),
                                         unitvalue = (sum(GastoMes, na.rm = T)/sum(LitrosMes, na.rm = T))),by=list(DIRECTORIO,Item)]


  unitvalues_2[,unitvalue:=remove_outliers(x=unitvalue,k=6),by="Item"]
  summary(unitvalues_2$unitvalue)   
  
  unitvalues_2 <- as.data.frame(unitvalues_2)
  unitvalues_2 %<>% filter(!is.nan(unitvalue)) 
  unitvalues_2 %<>% filter(unitvalue > 0) 
  
  unitvalues_2 <- as.data.table(unitvalues_2)
  # Modelo precios----
  
  # Filtar bien
  
  for(jj in bienes){
    
    cat(paste("Bien",jj), sep='\n')  
    
    db <- copy(unitvalues_2)
    
    # Unir covariables
    db_x <- Data[,c("DIRECTORIO","DOMINIO","xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
                    "IT","Cluster","REGION"),with=FALSE]
    
    db[,hid:=as.character(DIRECTORIO)]
    
    db <- db[Item==jj,]
    
    db <- merge(db,db_x,by="DIRECTORIO",all.x = T)
    
    db <- db[complete.cases(db),]
    # Estimar modelo
    
    db[,lnu:=log(unitvalue)]
    
    db[,Item:=as.factor(Item)]
    
    if(jj %in% c("Beer","Wine","Water") & ii %in% c("Fuera")){

      Y <- "lnu"
      X <- c("xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
             "IT","REGION")
      
      formula.v <- c(paste0(Y, "~", paste(paste(X, collapse="+"),
                                          sep = "+")))
      
      lmlnu <- lm(formula.v, data = db)
      summary(lmlnu)
      
    }else{
      
      Y <- "lnu"
      X <- c("xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
             "IT","DOMINIO")
      
      formula.v <- c(paste0(Y, "~", paste(paste(X, collapse="+"),
                                          sep = "+")))
      
      lmlnu <- lm(formula.v, data = db)
      summary(lmlnu)
      
    }

    # Predecir precios h con obs
    
    db_obs <- copy(db)
    
    db_obs[,lnp:=lnu] 

    db_obs <- db_obs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
    
    db_obs[,d_obs:=1]
    
    # Predecir precios h sin obs
    
    db_nobs <- Data[!(DIRECTORIO %in% unique(db_obs$DIRECTORIO)),c("DIRECTORIO","xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
                                                                   "IT","DOMINIO","REGION"),with=FALSE]
    
    db_nobs[,lnu:= predict(lmlnu, newdata = db_nobs)]
    
    db_nobs[,lnp:= lnu]
    db_nobs <- db_nobs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
    
    db_nobs[,d_obs:=0]
    
    db_p <- rbind(db_obs,db_nobs)
    
    db_p[,PrecioM:=exp(lnp)]
    
    db_p[,Item:=jj]
    db_p[,Canal:=ii]
    
    dt_precios <- rbind(dt_precios,db_p)
    
  }
  
  # Limpiar
  rm(unitvalues_2)
  
  dt_consumo_hogar <- dt_consumo[Canal==ii,]
  #dt_precios_hogar <- dt_precios[Canal==ii,c("DIRECTORIO","Item","PrecioM"),with=FALSE]
  
  dt_precios_hogar <- dt_precios[Canal==ii & d_obs==1,c("DIRECTORIO","Item","PrecioM"),with=FALSE]
  dt_precios_hogar$d_obs <- NULL
  
  dt_precios_hogar <- as.data.frame(dt_precios_hogar)
  dt_precios_hogar %<>% pivot_wider(names_from = Item,
                                    names_prefix = "PrecioM_",
                                    values_from = PrecioM)  
  
  
  save.dta13(dt_precios_hogar,file = paste0(wdd_out,"UnitValuePrices_",ii,".dta"))
  save.dta13(dt_consumo_hogar,file = paste0(wdd_out,"UnitValueGasto_",ii,".dta"))
  
}



## HEDONIC PRICES------

#Limpiar

bienes <- unique(total_2$Item)

dt_consumo <- c()
dt_precios <- c()

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
  
  ConsumoMesTotal[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotal[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  # summary(ConsumoMesTotal$Gasto)
  # summary(ConsumoMesTotal$Cantidades)
  # 
  # report_na <- ConsumoMesTotal[,list(Hogares=length(unique(DIRECTORIO)),
  #                                    Missing=sum(as.numeric(is.na(Gasto)))),by=list(Item)] 
  # report_na[,pje:=Missing/Hogares]
  ConsumoMesTotal <- reshape(ConsumoMesTotal,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  ConsumoMesTotal_codi <- total_3[Texto==0,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotal_codi[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotal_codi[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  
  ConsumoMesTotal_codi <- reshape(ConsumoMesTotal_codi,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Gasto", replacement = "GastoCodi")  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Cantidades", replacement = "CantidadesCodi")
  
  ConsumoMesTotaltexto <- total_3[Texto==1,list(Gasto=sum(GastoMes,na.rm = T),
                                                Cantidades=sum(LitrosMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotaltexto[,Gasto:=remove_outliers(Gasto,k=1.5),by="Item"]
  ConsumoMesTotaltexto[,Cantidades:=remove_outliers(Cantidades,k=1.5),by="Item"]
  
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
  
  ## Unit value----

  unitvalues_2 <- total_3[Canal==ii,list(litrosmes = sum(LitrosMes, na.rm = T),
                                         GastoMes  = sum(GastoMes, na.rm = T),
                                         unitvalue = (sum(GastoMes, na.rm = T)/sum(LitrosMes, na.rm = T))),by=list(DIRECTORIO,Item)]

  unitvalues_2[,unitvalue:=remove_outliers(x=unitvalue,k=6),by="Item"]
  summary(unitvalues_2$unitvalue)   
  
  unitvalues_2 <- as.data.frame(unitvalues_2)
  unitvalues_2 %<>% filter(!is.nan(unitvalue))
  unitvalues_2 %<>% filter(unitvalue > 0)

  unitvalues_2 <- as.data.table(unitvalues_2)
  # Modelo precios----

  # Filtar bien

  for(jj in bienes){

    cat(paste("Bien",jj), sep='\n')

    db <- copy(unitvalues_2)

    # Unir covariables
    db_x <- Data[,c("DIRECTORIO","DOMINIO","xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
                    "IT","xrural","Cluster","REGION"),with=FALSE]

    db[,hid:=as.character(DIRECTORIO)]

    db <- db[Item==jj,]

    db <- merge(db,db_x,all.x = T)

    db <- db[complete.cases(db),]
    # Estimar modelo

    db[,lnu:=log(unitvalue)]

    db[,Item:=as.factor(Item)]

    if(jj %in% c("Beer","Wine","Water") & ii %in% c("Fuera")){
      
      Y <- "lnu"
      X <- c("xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
             "IT","xrural","REGION")
      
      formula.v <- c(paste0(Y, "~", paste(paste(X, collapse="+"),
                                          sep = "+")))
      
      lmlnu <- lm(formula.v, data = db)
      summary(lmlnu)
      
      
      lmlnu_fe <- feols(lnu~xsexo+xedad+xtamanioh+xuniper+xninios+IT | Cluster, db)
      
      summary(lmlnu_fe)
      
      # Predecir precios h con obs
      
      X_hat    <- Data[DIRECTORIO  %in% unique(db$DIRECTORIO),c("xsexo","xedad","xtamanioh","xuniper","xninios","IT"),with=FALSE]
      
      # X_hat <- dummy_cols(X_hat, select_columns = 'xeduc')
      # X_hat[,c("xeduc","xeduc_Ninguno"):=NULL]
      X_hat <- as.matrix(X_hat)
      
      gamma_hat <- as.matrix(lmlnu_fe[["coefficients"]])
      
      lnu_hat <- as.data.frame(X_hat%*%gamma_hat)
      
      db_obs <- cbind(db,lnu_hat)
      
      db_obs[,lnp:=lnu-V1]
      
      db_obs <- db_obs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
      
      # Predecir precios h sin obs
      
      db_nobs <- Data[!(DIRECTORIO %in% unique(db_obs$DIRECTORIO)),c("DIRECTORIO","xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
                                                                     "IT","xrural","REGION"),with=FALSE]
      
      db_nobs[,lnu:= predict(lmlnu, newdata = db_nobs)]
      
      db_nobs[,lnp:= lnu]
      db_nobs <- db_nobs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
      
    }else{    
      
    Y <- "lnu"
    X <- c("xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
           "IT","DOMINIO")
    
    formula.v <- c(paste0(Y, "~", paste(paste(X, collapse="+"),
                                        sep = "+")))
    
    lmlnu <- lm(formula.v, data = db)
    summary(lmlnu)
    
    
    lmlnu_fe <- feols(lnu~xsexo+xedad+xtamanioh+xuniper+xninios+IT+xeduc | Cluster, db)
    
    summary(lmlnu_fe)
    
    # Predecir precios h con obs
    
    X_hat    <- Data[DIRECTORIO  %in% unique(db$DIRECTORIO),c("xsexo","xedad","xtamanioh","xuniper","xninios",
                                                              "IT","xeduc"),with=FALSE]
    
    X_hat <- dummy_cols(X_hat, select_columns = 'xeduc')
    X_hat[,c("xeduc","xeduc_Ninguno"):=NULL]
    X_hat <- as.matrix(X_hat)
    
    gamma_hat <- as.matrix(lmlnu_fe[["coefficients"]])
    
    lnu_hat <- as.data.frame(X_hat%*%gamma_hat)
    
    db_obs <- cbind(db,lnu_hat)
    
    db_obs[,lnp:=lnu-V1]
    
    db_obs <- db_obs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
    
    # Predecir precios h sin obs
    
    db_nobs <- Data[!(DIRECTORIO %in% unique(db_obs$DIRECTORIO)),c("DIRECTORIO","xsexo","xedad","xeduc","xtamanioh","xuniper","xninios",
                                                                   "IT","xrural","DOMINIO"),with=FALSE]
    
    db_nobs[,lnu:= predict(lmlnu, newdata = db_nobs)]
    
    db_nobs[,lnp:= lnu]
    db_nobs <- db_nobs[,c("DIRECTORIO","lnu","lnp"),with=FALSE]
    
    }

    db_p <- rbind(db_obs,db_nobs)

    db_p[,PrecioM:=exp(lnp)]

    db_p[,Item:=jj]
    db_p[,Canal:=ii]

    dt_precios <- rbind(dt_precios,db_p)

  }

  dt_consumo_hogar <- dt_consumo[Canal==ii,]
  dt_precios_hogar <- dt_precios[Canal==ii,c("DIRECTORIO","Item","PrecioM"),with=FALSE]

  dt_precios_hogar <- as.data.frame(dt_precios_hogar)
  dt_precios_hogar %<>% pivot_wider(names_from = Item,
                                    names_prefix = "PrecioM_",
                                    values_from = PrecioM)


  save.dta13(dt_precios_hogar,file = paste0(wdd_out,"QualityAdjustedPrices_",ii,".dta"))
  #save.dta13(dt_consumo_hogar,file = paste0(wdd_out,"Gasto_",ii,".dta"))

}


# dt_consumo_hogar <- dt_consumo[Canal=="Hogar",]
# dt_precios_hogar <- dt_precios[Canal=="Hogar",c("DIRECTORIO","Item","PrecioM"),with=FALSE]
#
# dt_precios_hogar <- as.data.frame(dt_precios_hogar)
# dt_precios_hogar %<>% pivot_wider(names_from = Item,
#                               names_prefix = "PrecioM_",
#                               values_from = PrecioM)
#
#
# save.dta13(dt_precios_hogar,file = paste0(wdd_out,"QualityAdjustedPrices_","Hogar",".dta"))
# save.dta13(dt_consumo_hogar,file = paste0(wdd_out,"Gasto_","Hogar",".dta"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## CANAL PRICES------
# ## Revise Paper
# #Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))
# 
# # Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))
# # 
# # Data <- as.data.table(Data)
# # 
# # UrbanoTotal <- readRDS(file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))
# # RuralTotal  <- readRDS(file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))
# 
# canal <- c("Hogar","Fuera")
# 
# IdSubSet <- Data[,c("DIRECTORIO"),with=FALSE]
# 
# for(ii in canal){
# 
#   total_2 <- as.data.table(rbind(UrbanoTotal,RuralTotal))
#   
#   CovariableSubSet <- Data[,c("DIRECTORIO","FEX_C", "REGION", "DOMINIO",
#                               "TercilIngr","Quintil", "xrural","Cluster"),with=FALSE]
#   
#   total_2 <- total_2[Canal==ii,]
#   
#   ## Gasto----
#   
#   ConsumoMesTotal <- total_2[,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
#   
#   ConsumoMesTotal <- reshape(ConsumoMesTotal,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
#   
#   
#   ConsumoMesTotal_codi <- total_2[Texto==0,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
#   
#   ConsumoMesTotal_codi <- reshape(ConsumoMesTotal_codi,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
#   
#   names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Gasto", replacement = "GastoCodi")  
# 
#   ConsumoMesTotaltexto <- total_2[Texto==1,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
#   
#   ConsumoMesTotaltexto <- reshape(ConsumoMesTotaltexto,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
#   
#   names(ConsumoMesTotaltexto) <- gsub(x = names(ConsumoMesTotaltexto), pattern = "Gasto", replacement = "GastoTexto")  
#   
#   ConsumoMes <- merge(ConsumoMesTotal,ConsumoMesTotal_codi,by=c("DIRECTORIO"),all.x = T)
#   ConsumoMes <- merge(ConsumoMes,ConsumoMesTotaltexto,by=c("DIRECTORIO"),all.x = T)
#   
#   ConsumoMes <- merge(IdSubSet,ConsumoMes,by=c("DIRECTORIO"),all.x = T)
#   
#   setnafill(ConsumoMes, fill = 0)
#   
#   save.dta13(ConsumoMes,file = paste0(wdd_out,"Gasto_",ii,".dta"))
#   
#   # Limpiar
#   rm(ConsumoMes,ConsumoMesTotal,ConsumoMesTotaltexto,ConsumoMesTotal_codi)
#   
#   
#   ## Unit value----
# 
#   unitvalues_2 <- total_2 %>%
#     group_by(DIRECTORIO, Item) %>% summarise(
#       litrosmes = sum(LitrosMes, na.rm = T),
#       GastoMes = sum(GastoMes, na.rm = T)
#     )
#   
#   unitvalues_2 %<>% mutate(
#     unitvalue = GastoMes/litrosmes
#   )
#   
#   unitvalues_2 <- as.data.table(unitvalues_2)
#   unitvalues_outlier <- unitvalues_2[,list(p25=quantile(unitvalue,probs=.25,na.rm=T),
#                                            p50=quantile(unitvalue,probs=.50,na.rm=T),
#                                            p75=quantile(unitvalue,probs=.75,na.rm=T),
#                                            iqr=quantile(unitvalue,probs=.75,na.rm=T)-quantile(unitvalue,probs=.25,na.rm=T)),by=list(Item)]
#   
#   unitvalues_2 <- merge(unitvalues_2,unitvalues_outlier,all.x = T)
#   
#   unitvalues_2[unitvalue<p25-1.5*iqr,unitvalue:=p50]
#   unitvalues_2[unitvalue>p75+1.5*iqr,unitvalue:=p50]
#   
#   unitvalues_2[,c("p25","p50","p75","iqr"):=NULL]
#   
#   
#   unitvalues_2 <- as.data.frame(unitvalues_2)
#   # PEgar a unit values subset de variables
#   
#   
#   unitvalues_2 %<>% filter(!is.nan(unitvalue)) 
#   unitvalues_2 %<>% filter(unitvalue > 0) 
# 
#   unitvalues_2 <- left_join(unitvalues_2, 
#                             CovariableSubSet, 
#                             by = c("DIRECTORIO"))
#   
#   # Calcular unit values subset de variables
#   
#   unitvalues_2 %<>% select(DIRECTORIO,
#                            FEX_C,
#                            REGION,
#                            DOMINIO,
#                            TercilIngr,
#                            Item,
#                            Quintil,
#                            xrural,
#                            Cluster,
#                            unitvalue) 
#   unitvalues_region <- unitvalues_2 %>% 
#     group_by(REGION,
#              DOMINIO,
#              Item) %>% summarise(
#                unitvalue = weighted.mean(unitvalue, FEX_C)
#              )
#   
#   unitvalues_2 %<>% 
#     group_by (REGION,
#               DOMINIO,
#               Cluster,
#               Item) %>%
#     summarise(unitvalue = weighted.mean(unitvalue, FEX_C)) 
#   
#   unitvalues_2 <- as.data.table(unitvalues_2)
#   
#   unitvalues_outlier <- unitvalues_2[,list(p25=quantile(unitvalue,probs=.25,na.rm=T),
#                                            p50=quantile(unitvalue,probs=.50,na.rm=T),
#                                            p75=quantile(unitvalue,probs=.75,na.rm=T),
#                                            iqr=quantile(unitvalue,probs=.75,na.rm=T)-quantile(unitvalue,probs=.25,na.rm=T)),by=list(REGION,DOMINIO,Item)]
#   
#   unitvalues_2 <- merge(unitvalues_2,unitvalues_outlier,all.x = T)
#   
#   unitvalues_2[unitvalue<p25-1.5*iqr,unitvalue:=p50]
#   unitvalues_2[unitvalue>p75+1.5*iqr,unitvalue:=p50]
#   
#   unitvalues_2[,c("p25","p50","p75","iqr"):=NULL]
#   unitvalues_2 %<>% pivot_wider(names_from = Item,
#                                 names_prefix = "PrecioM_",
#                                 values_from = unitvalue)  
#   
#   unitvalues_region %<>% pivot_wider(names_from = Item,
#                                      names_prefix = "PrecioMR_",
#                                      values_from = unitvalue)
#   
#   unitvalues_2 <- left_join(unitvalues_2, 
#                             unitvalues_region, 
#                             by = c("REGION",
#                                    "DOMINIO"))
#   
#   unitvalues_2 <- unitvalues_2 %>% mutate(
#     PrecioM_Beer = ifelse(is.na(PrecioM_Beer), PrecioMR_Beer, PrecioM_Beer),
#     PrecioM_Wine = ifelse(is.na(PrecioM_Wine), PrecioMR_Wine, PrecioM_Wine),
#     PrecioM_HotDrinks = ifelse(is.na(PrecioM_HotDrinks), PrecioMR_HotDrinks, PrecioM_HotDrinks),
#     PrecioM_MilkDerivatives = ifelse(is.na(PrecioM_MilkDerivatives), PrecioMR_MilkDerivatives, PrecioM_MilkDerivatives),
#     PrecioM_SSB = ifelse(is.na(PrecioM_SSB), PrecioMR_SSB, PrecioM_SSB),
#     PrecioM_Water = ifelse(is.na(PrecioM_Water), PrecioMR_Water, PrecioM_Water),
#     PrecioM_Spirits = ifelse(is.na(PrecioM_Spirits), PrecioMR_Spirits, PrecioM_Spirits)
#   )
#   
#   unitvalues_2 %<>% select(c(REGION,
#                              DOMINIO,
#                              Cluster,
#                              PrecioM_Beer,
#                              PrecioM_HotDrinks,
#                              PrecioM_MilkDerivatives,
#                              PrecioM_SSB,
#                              PrecioM_Spirits,
#                              PrecioM_Water,
#                              PrecioM_Wine
#   )) 
#   
#   
#   unitvalues_2 <- as.data.frame(unitvalues_2)
#   save.dta13(unitvalues_2,file = paste0(wdd_out,"MeanUnitValuesCluster_",ii,".dta"))
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # VECTOR A UTILIZAR------
# 
# Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))
# 
# Data <- as.data.table(Data)
# Data[,Cluster:=substr(VIVIENDA,1,10)]
# 
# Data <- Data[,c("DIRECTORIO","REGION", "DOMINIO","TercilIngr","Cluster"),with=FALSE]
# 
# PrecioM <- read.dta13(file = paste0(wdd_out,"MeanUnitValuesCluster_","Hogar",".dta"))
# 
# PrecioR <- read.dta13(file = paste0(wdd_out,"MeanUnitValuesTercil_","Hogar",".dta"))
# 
# 
# Data <- merge(Data,PrecioM,all.x = T)
# 
# Data <- merge(Data,PrecioR,by=c("REGION", "DOMINIO","TercilIngr"),all.x = T)
# summary(Data)
# 
# Data <- as.data.frame(Data)
# Data <- Data %>% mutate(
#   PrecioM_Beer.x = ifelse(is.na(PrecioM_Beer.x), PrecioM_Beer.y, PrecioM_Beer.x),
#   PrecioM_Wine.x = ifelse(is.na(PrecioM_Wine.x), PrecioM_Wine.y, PrecioM_Wine.x),
#   PrecioM_HotDrinks.x = ifelse(is.na(PrecioM_HotDrinks.x), PrecioM_HotDrinks.y, PrecioM_HotDrinks.x),
#   PrecioM_MilkDerivatives.x = ifelse(is.na(PrecioM_MilkDerivatives.x), PrecioM_MilkDerivatives.y, PrecioM_MilkDerivatives.x),
#   PrecioM_SSB.x = ifelse(is.na(PrecioM_SSB.x), PrecioM_SSB.y, PrecioM_SSB.x),
#   PrecioM_Water.x = ifelse(is.na(PrecioM_Water.x), PrecioM_Water.y, PrecioM_Water.x),
#   PrecioM_Spirits.x = ifelse(is.na(PrecioM_Spirits.x), PrecioM_Spirits.y, PrecioM_Spirits.x)
# )
# 
# Data <- as.data.table(Data)
# Data[,c("PrecioM_HotDrinks.y","PrecioM_MilkDerivatives.y","PrecioM_SSB.y","PrecioM_Spirits.y","PrecioM_Water.y","PrecioM_Wine.y","PrecioM_Beer.y"):=NULL]
# 
# colnames(Data) <- gsub(".x","",colnames(Data))
# 
# save.dta13(Data,file = paste0(wdd_out,"MeanUnitValuesHousehold_","Hogar",".dta"))
# 
# g <- ggplot(dt_precios, aes(x=Item, y=PrecioM, fill=Item)) + 
#   geom_boxplot()
# 
# g