##### Survey design

data <- copy(dt_SSB)

## Categorias
data[,Item:=ifelse(Item=="SSB","1SSB",Item)]
data[,Item:=ifelse(Item=="Beer","2Beer",Item)]
data[,Item:=ifelse(Item=="Spirits","3Spirits",Item)]
data[,Item:=ifelse(Item=="Wine","4Wine",Item)]
data[,Item:=ifelse(Item=="HotDrinks","5Hot drinks",Item)]
data[,Item:=ifelse(Item=="MilkDerivatives","6Milk",Item)]
data[,Item:=ifelse(Item=="Water","7Water",Item)]

# data <- data[Item=="1SSB",]
# 
# fwrite(data,fil=paste0(wd_resu,"dtssb.csv"))

## Canal de compras
#data[,Finalidad:=ifelse(substr(Producto,1,2)==11,"1Out","2Home")]
data[,Finalidad:=ifelse(Tabla %in% c(4,6,9,14),"1Out","2Home")]

#data <- dt_SSB[substr(Producto,1,4)=="0212",]

data <- merge(data,dc_vivi,by=c("DIRECTORIO"),all.x = TRUE)

# dtssb<- data[Item=="1SSB",]
# sum(dtssb$LitrosMes*dtssb$FEX_C*12)

### Hogares

dataH <- unique(data,by=c("DIRECTORIO","Item"))

dataH[,Hogares:=1]

dataH[,sweights:=1/FEX_C]

designH <- svydesign(id =~ DIRECTORIO,
                    weights =~ FEX_C, 
                    data = dataH)

#dsvresignH <- svrepdesign(data = dataH,repweights =~ FEX_C,type = "JK1")

formulaJj <- as.formula(paste('~', "Hogares"))
formulaIi <- as.formula(paste('~', "Item"))

tN1       <- svyby(formulaJj, formulaIi,
                   design = designH, 
                   data= dataH,FUN=svytotal, vartype = "cvpct")

tN1   <- as.data.table(tN1)
tN1[,total:=Hogares/1000]
tN1[,c("Hogares"):=NULL]
tN1 <- melt(tN1,id.vars = "Item",value.name = "Households")

#tN1[,value:=value*1]
tN1 <- tN1[order(tN1$Item)]

fwrite(tN1,file=paste0(wd_resu,"Esti/","Tab1SSBHouseholds",".csv"))

### Personas

dataH[,Personas:=as.numeric(as.character(P6008))]

formulaJj <- as.formula(paste('~', "Personas"))
formulaIi <- as.formula(paste('~', "Item"))

tN2       <- svyby(formulaJj, formulaIi,
                   design = designH, 
                   data= dataH,FUN=svytotal, vartype = "cvpct")

tN2   <- as.data.table(tN2)
tN2[,Personas:=Personas/1000]

tN2[,total:=Personas]
tN2[,c("Personas"):=NULL]

tN2 <- melt(tN2,id.vars = "Item",value.name = "People")

#tN2[,value:=value*1]
tN2 <- tN2[order(tN2$Item)]

fwrite(tN2,file=paste0(wd_resu,"Esti/","Tab1SSBPeople",".csv"))

### Expenditure 
designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = data)

formulaJj <- as.formula(paste('~', "GastoMes"))
formulaIi <- as.formula(paste('~', "Item"))

tN3       <- svyby(formulaJj, formulaIi,
                   design = designI, 
                   data= data,FUN=svytotal, vartype = "cvpct",na.rm=TRUE)

tN3   <- as.data.table(tN3)

tN3[,GastoMes:=GastoMes/1000000]

tN3[,total:=GastoMes]

tN3[,c("GastoMes"):=NULL]

tN3 <- melt(tN3,id.vars = "Item",value.name = "Expenses")
#tN3[,value:=value*1]

tN3 <- tN3[order(tN3$Item)]

fwrite(tN3,file=paste0(wd_resu,"Esti/","Tab1SSBExpend",".csv"))

### Consumption

formulaJj <- as.formula(paste('~', "LitrosMes"))
formulaIi <- as.formula(paste('~', "Item"))

tN4       <- svyby(formulaJj, formulaIi,
                   design = designI, 
                   data= data,FUN=svytotal, vartype = "cvpct",na.rm=TRUE)

tN4   <- as.data.table(tN4)
tN4[,LitrosMes:=LitrosMes/1000000]

tN4[,total:=LitrosMes]

tN4[,c("LitrosMes"):=NULL]

tN4 <- melt(tN4,id.vars = "Item",value.name = "Consu")

#tN4[,value:=value*1]

tN4 <- tN4[order(tN4$Item)]

fwrite(tN4,file=paste0(wd_resu,"Esti/","Tab1SSBConsu",".csv"))


# ###
# # Expense per household
# 
# formulaJj <- as.formula(paste('~', "LitrosMes"))
# formulaIi <- as.formula(paste('~', "Item"))
# 
# tN4       <- svyby(formulaJj, formulaIi,
#                    design = designI, 
#                    data= data,FUN=svytotal, vartype = "cvpct",na.rm=TRUE)
# 
# 
# svyratio(~alive, ~arrests, design=scddes)
# 
# 
# 
# 
# 

# Cuadro de salida

tN <- Reduce(function(...) merge(..., all=TRUE), list(tN1, tN2, tN3, tN4))

tN <- tN[order(tN$Item,-tN$variable)]

fwrite(tN,file=paste0(wd_resu,"Esti/","Tab1SSB",".csv"))






## Share Gasto Canal

designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = data)

formulaJj <- as.formula(paste('~', "GastoMensual"))
formulaIi <- as.formula(paste('~', "Item+Finalidad"))

tNs       <- svyby(formulaJj, formulaIi,
                   design = designI, 
                   data= data,FUN=svytotal, vartype = "cvpct",na.rm=TRUE)

tNs   <- as.data.table(tNs)
tNs[,GastoMensual:=GastoMensual*12/1000000000]

tNs <- melt(tNs,id.vars = c("Item","Finalidad"))
tNs[,value:=value*1]

tNs <- tNs[order(tNs$Item,tNs$Finalidad)]










# 
# 
# 
# 
# festitotalH <- function(data,design){
#   
#   # Hogares
#   
#   tN1       <- svyby(formulaJj, formulaIi, 
#                      subset(disenoMuJk, P6040 >= 15 & P6040 <= 28
#                             & !is.na(Ingreso)),
#                      svymean, vartype = 'cvpct')
#   
#   
#   yhat.resu <- svytotal(~Hogares, design, na.rm=TRUE)
#   yhat.resu
#   
#   cv.resu <- cv(svytotal(~Hogares, design, na.rm=TRUE,vartype = 'cvpct'))
#   cv.resu
#   
#   ci.resu <- confint(svytotal(~Hogares, design, na.rm=TRUE))
#   ci.resu
#   
# }
# 
# 
# 
# 
# 
# 
# 
# # Hogares
# design <- svydesign(id =~ DIRECTORIO,
#                                    weights =~ FEX_C, 
#                                    data = dataH)
# 
# yhat.resu <- svytotal(~Hogares, design, na.rm=TRUE)
# yhat.resu
# 
# cv.resu <- cv(svytotal(~Hogares, design, na.rm=TRUE,vartype = 'cvpct'))
# cv.resu
# 
# ci.resu <- confint(svytotal(~Hogares, design, na.rm=TRUE))
# ci.resu
# 
# data[,GastoMensual:=GastoMensual/1000000]
# 
# 
# ormulaIi <- as.formula(paste('~', ii))
# tN1       <- svyby(formulaJj, formulaIi, 
#                    subset(disenoMuJk, P6040 >= 15 & P6040 <= 28
#                           & !is.na(Ingreso)),
#                    svymean, vartype = 'cvpct')
# 
# 
# 
# # Gasto
# design <- svydesign(id =~ DIRECTORIO,
#                     weights =~ FEX_C, 
#                     data = data)
# 
# yhat.resu <- svytotal(~GastoMensual, design, na.rm=TRUE)
# yhat.resu
# 
# cv.resu <- cv(svytotal(~GastoMensual, design, na.rm=TRUE,vartype = 'cvpct'))
# cv.resu
# 
# ci.resu <- confint(svytotal(~GastoMensual, design, na.rm=TRUE))
# ci.resu
# 
# # Litros
# design <- svydesign(id =~ DIRECTORIO,
#                     weights =~ FEX_C, 
#                     data = data)
# 
# yhat.resu <- svytotal(~LitrosMes, design, na.rm=TRUE)
# yhat.resu
# 
# cv.resu <- cv(svytotal(~LitrosMes, design, na.rm=TRUE,vartype = 'cvpct'))
# cv.resu
# 
# ci.resu <- confint(svytotal(~LitrosMes, design, na.rm=TRUE))
# ci.resu
# 
# 
# # Litros per capita
# 
# 
# design <- svydesign(id =~ DIRECTORIO,
#                     weights =~ FEX_C, 
#                     data = data)
# 
# yhat.resu <- svytotal(~GastoMensual, design, na.rm=TRUE)
# yhat.resu
# 
# cv.resu <- cv(svytotal(~GastoMensual, design, na.rm=TRUE,vartype = 'cvpct'))
# cv.resu
# 
# ci.resu <- confint(svytotal(~GastoMensual, design, na.rm=TRUE))
# ci.resu
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
# #disenoMuJk <- as.svrepdesign(design, type = "bootstrap", replicates = 1000)
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
# library(boot)
# 
# # fyhat <- function(d, i, x){
# #   d <- as.data.frame(d)
# #   d2 <- d[i,]
# #   w  <- d2$FEX_C
# #   x  <- d2[c(match(x,colnames(d2)))]
# #   y  <- sum(x*w)
# #   return(y)
# # }
# 
# fyhat <- function(d, i){
#   d <- as.data.frame(d)
#   d2 <- d[i,]
#   w  <- d2$FEX_C
#   x  <- d2[c(match("GastoMensual",colnames(d2)))]
#   y  <- sum(x*w)
#   return(y)
# }
# 
# # d <- dc_PerRurCom
# # x <- "LitrosMes"
# 
# yhat <- fyhat(d=dc_PerRurCom,x="LitrosMes")
# yhat
# 
# sum(dc_PerRurCom$FEX_C*dc_PerRurCom$LitrosMes)
# 
# #turn off set.seed() if you want the results to vary
# set.seed(123)
# 
# results <- boot(data=dt_SSB[Item=="Wine" & substr(Producto,1,4)=="0212",], statistic=fyhat, R=500)
# 
# summary(results)
# results
# plot(results)
# 
# boot.ci(boot.out = results, type = c("norm"))
# 
# cve <- sd(results$t)/mean(results$t)*100
# cve
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
# dt_vivi <- as.data.table(read.spss(paste0(wdd_in,"Viviendas y hogares.sav"), 
#                                    to.data.frame = TRUE,use.value.labels = F,r))
# 
# 
# dt_vivi <- dt_vivi[,c("DIRECTORIO","P3","FEX_C"),with=FALSE]
# dt_vivi[,W:=FEX_C]
# 
# library(boot)
# 
# fc <- function(d, i){
#   d2 <- d[i,]
#   return(sum(d2$FEX_C))
# }
# 
# 
# #turn off set.seed() if you want the results to vary
# set.seed(626)
# results <- boot(dt_vivi, fc, R=1000)
# summary(results)
# results
# plot(results)
# 
# boot.ci(boot.out = results, type = c("norm"))
# 
# # function to obtain R-Squared from the data
# rsq <- function(formula, data, indices) {
#   d <- data[indices,] # allows boot to select sample
#   fit <- sum(FEX_C)
#   return(fit)
# }
# # bootstrapping with 1000 replications
# results <- boot(data=dt_vivi, statistic=rsq,
#                 R=1000)
# 
# # view results
# results
# plot(results)
# 
# 
# 
# 
# if(psample){
#   dt_vivi <- dt_vivi[sample(.N,.N*0.01)]
#   dt_vivi <- dt_vivi[!duplicated(dt_vivi$DIRECTORIO),]
# }
