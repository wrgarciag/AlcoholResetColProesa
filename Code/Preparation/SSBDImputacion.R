

### Se imputan datos de ingresos y gastos

Data <- read.dta13(file=paste0(wdd_out,"DataEstimates4.dta"))

Data <- as.data.table(Data)

Data[,key:=seq(.N)]
Data[IT<1000,IT:=NA]
Data[GastoTotal<1000,GastoTotal:=NA]
Data[,IT:=remove_outliers(IT,k=6)]
summary(Data$IT)
Data[,GastoTotal:=remove_outliers(GastoTotal,k=6)]
summary(Data$GastoTotal)

#Imputacion ingreso y gasto
imp <- mice(Data[,c("IT","GastoTotal","xtamanioh","xninios","xrural","superior","xsexo","xedad"),with=F])
imp

gasto_imp <- as.data.frame(imp[["imp"]][["GastoTotal"]])
colnames(gasto_imp) <- paste("gasto_imp",colnames(gasto_imp),sep = "_")
gasto_imp$key <- as.numeric(rownames(gasto_imp))
gasto_imp <- as.data.table(gasto_imp)
gasto_imp[,GastoImp:=gasto_imp_1]

gasto_imp <- gasto_imp[,c("key","GastoImp"),with=FALSE]

Data <- merge(Data,gasto_imp,by="key",all.x = T)

summary(Data$GastoTotal)
Data[is.na(GastoTotal),GastoTotal:=GastoImp]
summary(Data$GastoTotal)

ingresoimp <- as.data.frame(imp[["imp"]][["IT"]])
colnames(ingresoimp) <- paste("ingresoimp",colnames(ingresoimp),sep = "_")
ingresoimp$key <- as.numeric(rownames(ingresoimp))
ingresoimp <- as.data.table(ingresoimp)
ingresoimp[,ITImp:=ingresoimp_1]

ingresoimp <- ingresoimp[,c("key","ITImp"),with=FALSE]

Data <- merge(Data,ingresoimp,by="key",all.x = T)

summary(Data$IT)
Data[is.na(IT),IT:=ITImp]
summary(Data$IT)

Data[,c("GastoImp","ITImp","key"):=NULL]

#Reemplaza valores

# Ingreso per capita
Data[,GastoPC:=GastoTotal/xtamanioh]
Data[,ITPC:=IT/xtamanioh]

# Terciles de ingreso 
perc.rank <- function(x) trunc(rank(x))/length(x)

Data[,percentile:=perc.rank(ITPC)]
Data[,TercilIngr:=1]
Data[,TercilIngr:=ifelse(percentile>=0.33 & percentile <0.66,2,TercilIngr)]
Data[,TercilIngr:=ifelse(percentile>=0.66,3,TercilIngr)]

Data[ ,Quintil:= cut(ITPC,
                      breaks = quantile(ITPC, probs = 0:5/5),
                      labels = 1:5, right = FALSE)]

write_dta(Data, paste0(wdd_out, "DataEstimates5.dta"))

