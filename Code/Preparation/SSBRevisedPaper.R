
## Full prices------
## Revise Paper
#Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))

Data <- as.data.table(Data)
UrbanoTotal <- readRDS(file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))
RuralTotal  <- readRDS(file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))

total_2 <- rbind(UrbanoTotal,RuralTotal)

Data[,Cluster1:=substr(VIVIENDA,1,8)]
Data[,Cluster2:=substr(VIVIENDA,1,10)]
Data[,Cluster:=substr(VIVIENDA,1,10)]

length(unique(Data$Cluster1))
length(unique(Data$Cluster2))

CovariableSubSet <- Data[,c("DIRECTORIO","FEX_C", "REGION", "DOMINIO",
                            "TercilIngr","Quintil", "xrural","Cluster"),with=FALSE]


## CANAL PRICES------
## Revise Paper
#Data <- readRDS(file=paste0(wdd_out,"DataEstimates.rds"))

# Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))
# 
# Data <- as.data.table(Data)
# 
# UrbanoTotal <- readRDS(file = paste0(wdd_out,"DtConsuUrbanoTotal",".rds"))
# RuralTotal  <- readRDS(file = paste0(wdd_out,"DtConsuRuralTotal",".rds"))

canal <- c("Hogar","Fuera")

IdSubSet <- Data[,c("DIRECTORIO"),with=FALSE]

for(ii in canal){

  total_2 <- as.data.table(rbind(UrbanoTotal,RuralTotal))
  
  CovariableSubSet <- Data[,c("DIRECTORIO","FEX_C", "REGION", "DOMINIO",
                              "TercilIngr","Quintil", "xrural","Cluster"),with=FALSE]
  
  total_2 <- total_2[Canal==ii,]
  
  ## Gasto----
  
  ConsumoMesTotal <- total_2[,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotal <- reshape(ConsumoMesTotal,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  
  ConsumoMesTotal_codi <- total_2[Texto==0,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotal_codi <- reshape(ConsumoMesTotal_codi,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotal_codi) <- gsub(x = names(ConsumoMesTotal_codi), pattern = "Gasto", replacement = "GastoCodi")  

  ConsumoMesTotaltexto <- total_2[Texto==1,list(Gasto=sum(GastoMes,na.rm = T)),by=c("DIRECTORIO","Item")]
  
  ConsumoMesTotaltexto <- reshape(ConsumoMesTotaltexto,idvar = "DIRECTORIO",timevar = "Item",direction = "wide",sep="_")
  
  names(ConsumoMesTotaltexto) <- gsub(x = names(ConsumoMesTotaltexto), pattern = "Gasto", replacement = "GastoTexto")  
  
  ConsumoMes <- merge(ConsumoMesTotal,ConsumoMesTotal_codi,by=c("DIRECTORIO"),all.x = T)
  ConsumoMes <- merge(ConsumoMes,ConsumoMesTotaltexto,by=c("DIRECTORIO"),all.x = T)
  
  ConsumoMes <- merge(IdSubSet,ConsumoMes,by=c("DIRECTORIO"),all.x = T)
  
  setnafill(ConsumoMes, fill = 0)
  
  save.dta13(ConsumoMes,file = paste0(wdd_out,"Gasto_",ii,".dta"))
  
  # Limpiar
  rm(ConsumoMes,ConsumoMesTotal,ConsumoMesTotaltexto,ConsumoMesTotal_codi)
  
  
  ## Unit value----

  unitvalues_2 <- total_2 %>%
    group_by(DIRECTORIO, Item) %>% summarise(
      litrosmes = sum(LitrosMes, na.rm = T),
      GastoMes = sum(GastoMes, na.rm = T)
    )
  
  unitvalues_2 %<>% mutate(
    unitvalue = GastoMes/litrosmes
  )
  
  unitvalues_2 <- as.data.table(unitvalues_2)
  unitvalues_outlier <- unitvalues_2[,list(p25=quantile(unitvalue,probs=.25,na.rm=T),
                                           p50=quantile(unitvalue,probs=.50,na.rm=T),
                                           p75=quantile(unitvalue,probs=.75,na.rm=T),
                                           iqr=quantile(unitvalue,probs=.75,na.rm=T)-quantile(unitvalue,probs=.25,na.rm=T)),by=list(Item)]
  
  unitvalues_2 <- merge(unitvalues_2,unitvalues_outlier,all.x = T)
  
  unitvalues_2[unitvalue<p25-1.5*iqr,unitvalue:=p50]
  unitvalues_2[unitvalue>p75+1.5*iqr,unitvalue:=p50]
  
  unitvalues_2[,c("p25","p50","p75","iqr"):=NULL]
  
  
  unitvalues_2 <- as.data.frame(unitvalues_2)
  # PEgar a unit values subset de variables
  
  
  unitvalues_2 %<>% filter(!is.nan(unitvalue)) 
  unitvalues_2 %<>% filter(unitvalue > 0) 

  unitvalues_2 <- left_join(unitvalues_2, 
                            CovariableSubSet, 
                            by = c("DIRECTORIO"))
  
  # Calcular unit values subset de variables
  
  unitvalues_2 %<>% select(DIRECTORIO,
                           FEX_C,
                           REGION,
                           DOMINIO,
                           TercilIngr,
                           Item,
                           Quintil,
                           xrural,
                           Cluster,
                           unitvalue) 
  unitvalues_region <- unitvalues_2 %>% 
    group_by(REGION,
             DOMINIO,
             Item) %>% summarise(
               unitvalue = weighted.mean(unitvalue, FEX_C)
             )
  
  unitvalues_2 %<>% 
    group_by (REGION,
              DOMINIO,
              Cluster,
              Item) %>%
    summarise(unitvalue = weighted.mean(unitvalue, FEX_C)) 
  
  unitvalues_2 <- as.data.table(unitvalues_2)
  
  unitvalues_outlier <- unitvalues_2[,list(p25=quantile(unitvalue,probs=.25,na.rm=T),
                                           p50=quantile(unitvalue,probs=.50,na.rm=T),
                                           p75=quantile(unitvalue,probs=.75,na.rm=T),
                                           iqr=quantile(unitvalue,probs=.75,na.rm=T)-quantile(unitvalue,probs=.25,na.rm=T)),by=list(REGION,DOMINIO,Item)]
  
  unitvalues_2 <- merge(unitvalues_2,unitvalues_outlier,all.x = T)
  
  unitvalues_2[unitvalue<p25-1.5*iqr,unitvalue:=p50]
  unitvalues_2[unitvalue>p75+1.5*iqr,unitvalue:=p50]
  
  unitvalues_2[,c("p25","p50","p75","iqr"):=NULL]
  unitvalues_2 %<>% pivot_wider(names_from = Item,
                                names_prefix = "PrecioM_",
                                values_from = unitvalue)  
  
  unitvalues_region %<>% pivot_wider(names_from = Item,
                                     names_prefix = "PrecioMR_",
                                     values_from = unitvalue)
  
  unitvalues_2 <- left_join(unitvalues_2, 
                            unitvalues_region, 
                            by = c("REGION",
                                   "DOMINIO"))
  
  unitvalues_2 <- unitvalues_2 %>% mutate(
    PrecioM_Beer = ifelse(is.na(PrecioM_Beer), PrecioMR_Beer, PrecioM_Beer),
    PrecioM_Wine = ifelse(is.na(PrecioM_Wine), PrecioMR_Wine, PrecioM_Wine),
    PrecioM_HotDrinks = ifelse(is.na(PrecioM_HotDrinks), PrecioMR_HotDrinks, PrecioM_HotDrinks),
    PrecioM_MilkDerivatives = ifelse(is.na(PrecioM_MilkDerivatives), PrecioMR_MilkDerivatives, PrecioM_MilkDerivatives),
    PrecioM_SSB = ifelse(is.na(PrecioM_SSB), PrecioMR_SSB, PrecioM_SSB),
    PrecioM_Water = ifelse(is.na(PrecioM_Water), PrecioMR_Water, PrecioM_Water),
    PrecioM_Spirits = ifelse(is.na(PrecioM_Spirits), PrecioMR_Spirits, PrecioM_Spirits)
  )
  
  unitvalues_2 %<>% select(c(REGION,
                             DOMINIO,
                             Cluster,
                             PrecioM_Beer,
                             PrecioM_HotDrinks,
                             PrecioM_MilkDerivatives,
                             PrecioM_SSB,
                             PrecioM_Spirits,
                             PrecioM_Water,
                             PrecioM_Wine
  )) 
  
  
  unitvalues_2 <- as.data.frame(unitvalues_2)
  save.dta13(unitvalues_2,file = paste0(wdd_out,"MeanUnitValuesCluster_",ii,".dta"))
  
}

# VECTOR A UTILIZAR------

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))

Data <- as.data.table(Data)
Data[,Cluster:=substr(VIVIENDA,1,10)]

Data <- Data[,c("DIRECTORIO","REGION", "DOMINIO","TercilIngr","Cluster"),with=FALSE]

PrecioM <- read.dta13(file = paste0(wdd_out,"MeanUnitValuesCluster_","Hogar",".dta"))

PrecioR <- read.dta13(file = paste0(wdd_out,"MeanUnitValuesTercil_","Hogar",".dta"))


Data <- merge(Data,PrecioM,all.x = T)

Data <- merge(Data,PrecioR,by=c("REGION", "DOMINIO","TercilIngr"),all.x = T)
summary(Data)

Data <- as.data.frame(Data)
Data <- Data %>% mutate(
  PrecioM_Beer.x = ifelse(is.na(PrecioM_Beer.x), PrecioM_Beer.y, PrecioM_Beer.x),
  PrecioM_Wine.x = ifelse(is.na(PrecioM_Wine.x), PrecioM_Wine.y, PrecioM_Wine.x),
  PrecioM_HotDrinks.x = ifelse(is.na(PrecioM_HotDrinks.x), PrecioM_HotDrinks.y, PrecioM_HotDrinks.x),
  PrecioM_MilkDerivatives.x = ifelse(is.na(PrecioM_MilkDerivatives.x), PrecioM_MilkDerivatives.y, PrecioM_MilkDerivatives.x),
  PrecioM_SSB.x = ifelse(is.na(PrecioM_SSB.x), PrecioM_SSB.y, PrecioM_SSB.x),
  PrecioM_Water.x = ifelse(is.na(PrecioM_Water.x), PrecioM_Water.y, PrecioM_Water.x),
  PrecioM_Spirits.x = ifelse(is.na(PrecioM_Spirits.x), PrecioM_Spirits.y, PrecioM_Spirits.x)
)

Data <- as.data.table(Data)
Data[,c("PrecioM_HotDrinks.y","PrecioM_MilkDerivatives.y","PrecioM_SSB.y","PrecioM_Spirits.y","PrecioM_Water.y","PrecioM_Wine.y","PrecioM_Beer.y"):=NULL]

colnames(Data) <- gsub(".x","",colnames(Data))

save.dta13(Data,file = paste0(wdd_out,"MeanUnitValuesHousehold_","Hogar",".dta"))

g <- ggplot(unitvalues_2, aes(x=Item, y=unitvalue, fill=Item)) + 
  geom_boxplot()

g