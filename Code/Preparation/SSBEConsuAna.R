# Tabla de consumo por hogar

dt_SSB <- c()

dlist <- c("DtDiaUrb","DtDiaUrbComi","DtPerUrb","DtPerUrbComi","DtDiaUrbc","DtMfrecArt",
           "DtPerRur","DtPerRurCom","DtSemRurC","DtSemRur","DtSemRurComi")

for(ii in 1:length(dlist)) {
  
  cat("Tabla ",dlist[ii],sep="\n")

  dc <- as.data.table(readRDS(file = paste0(wdd_out,dlist[ii],".rds")))
  dc[,chogar:=as.numeric(substr(Producto,1,2) %in% c("01","02"))]
  
  dc <- dc[,list(Gasto=sum(GastoMes,na.rm = TRUE),
                 Litros=sum(LitrosMes,na.rm = TRUE)),by=list(DIRECTORIO,Item,chogar)]
  dt_SSB <- rbind(dt_SSB,dc,fill=TRUE)  

  # Limpiar
  rm(dc)
  
}

dt_SSB <- dt_SSB[,list(Gasto=sum(Gasto,na.rm = TRUE),
               Litros=sum(Litros,na.rm = TRUE)),by=list(DIRECTORIO,Item,chogar)]

dt_SSB[,Precio:=Gasto/Litros]

saveRDS(dt_SSB,file=paste0(wdd_out,"DataSSB.rds"))

dt_SSB <- as.data.table(readRDS(file=paste0(wdd_out,"DataSSB.rds")))

dt_exp <- copy(dt_SSB)
dt_exp <- dt_exp[Item!="",]

dt_exp <- dt_exp[,list(Gasto=sum(Gasto,na.rm = TRUE)),by=list(DIRECTORIO,Item)]
dt_exp <- reshape(dt_exp,idvar=c("DIRECTORIO"),
                   timevar=c("Item"),direction = "wide",sep = "_")

cols <- colnames(dt_exp)
dt_exp[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


dt_exp1 <- dt_SSB[chogar==1,]
dt_exp1 <- dt_exp1[,c("Litros","Precio","chogar"):=NULL]
dt_exp1 <- dt_exp1[Item!="",]

setnames(dt_exp1,"Gasto","Gasto1")
dt_exp1 <- reshape(dt_exp1,idvar=c("DIRECTORIO"),
                 timevar=c("Item"),direction = "wide",sep = "_")

cols <- colnames(dt_exp1)
dt_exp1[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]


dt_exp0 <- dt_SSB[chogar==0,]
dt_exp0 <- dt_exp0[,c("Litros","Precio","chogar"):=NULL]
dt_exp0 <- dt_exp0[Item!="",]

setnames(dt_exp0,"Gasto","Gasto0")
dt_exp0 <- reshape(dt_exp0,idvar=c("DIRECTORIO"),
                   timevar=c("Item"),direction = "wide",sep = "_")

cols <- colnames(dt_exp0)
dt_exp0[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]

dt_exp <- merge(dt_exp,dt_exp1,all.x = TRUE,all.y = TRUE)
dt_exp <- merge(dt_exp,dt_exp0,all.x = TRUE,all.y = TRUE)

Data <- merge(Data,dt_exp,by=c("DIRECTORIO"),all.x = TRUE)

# 
# 
# 
# 
# 
# 
# #------------------------------------------------------
# ### Consolida Tabla Urbano
# 
# dc_DiaUrb      <- readRDS(file = paste0(wdd_out,"DtDiaUrb",".rds"))
# dc_DiaUrbCF    <- readRDS(file = paste0(wdd_out,"DtDiaUrbComi",".rds"))
# dc_PerUrb      <- readRDS(file = paste0(wdd_out,"DtPerUrb",".rds"))
# dc_PerUrbComi  <- readRDS(file = paste0(wdd_out,"DtPerUrbComi",".rds"))
# dc_DiaUrbC     <- readRDS(file = paste0(wdd_out,"DtDiaUrbc",".rds"))
# dc_MfrecArt     <- readRDS(file = paste0(wdd_out,"DtMfrecArt",".rds"))
# 
# 
# ### Consolida Tabla Rural
# 
# dc_PerRur     <- readRDS(file = paste0(wdd_out,"DtPerRur",".rds"))
# dc_PerRurCom  <- readRDS(file = paste0(wdd_out,"DtPerRurCom",".rds"))
# dc_SemRurC    <- readRDS(file = paste0(wdd_out,"DtSemRurC",".rds"))
# dc_SemRur     <- readRDS(file = paste0(wdd_out,"DtSemRur",".rds"))
# dc_SemRurComi <- readRDS(file = paste0(wdd_out,"DtSemRurComi",".rds"))
# 
# ### Consolida Tabla Consumo
# dt_SSB    <- as.data.table(rbind(dc_DiaUrb,dc_DiaUrbC,dc_DiaUrbCF,dc_PerUrb,dc_PerUrbComi,dc_MfrecArt,fill=TRUE))
# 
# dt_SSB    <- rbind(dt_SSB,dc_PerRur,dc_PerRurCom,dc_SemRurC,dc_SemRur,dc_SemRurComi,fill=TRUE)
# 
# 
# # summary(dt_SSB$GastoUni[dt_SSB$Item=="SSB" & dt_SSB$Tabla %in% c(4,6,9,14)],na.rm = T)
# # quantile(dt_SSB$GastoUni[dt_SSB$Item=="SSB" & dt_SSB$Tabla %in% c(4,6,9,14)],0.5,na.rm = T)
# 
# summary(dt_SSB$LitrosMes[dt_SSB$Item=="SSB"],na.rm = T)
# quantile(dt_SSB$LitrosMes[dt_SSB$Item=="SSB"],0.99,na.rm = T)
# 
# 
# summary(dt_SSB$GastoMes[dt_SSB$Item=="SSB"],na.rm = T)
# quantile(dt_SSB$GastoMes[dt_SSB$Item=="SSB"],0.99,na.rm = T)
# 
# # A las cantidades de SSB con cantidades mayores a (50), se les imputara un litro y 250 en gasto (valor mediano). De resto se les imputo 1 litro ya que es 
# # el valor mediano en gasto
# # dt_SSB[Item=="SSB" & dt_SSB$Tabla %in% c(4,6,9,14), Gasto:=ifelse(Cantidad>50,pglitSSB,Gasto)]
# # dt_SSB[Item=="SSB" & dt_SSB$Tabla %in% c(4,6,9,14), LitrosMes:=ifelse(Cantidad>50,pqlitSSB,Cantidad)] 
# 
# # dt_SSB[Item=="SSB", LitrosMes:=ifelse(LitrosMes>11,11,LitrosMes)]
# # 
# # # Se le imputa la cantidad de percentil 99 por el gasto por litro mediano 2500
# # 
# # dt_SSB[Item=="SSB", GastoMes:=ifelse(GastoMes>11,pglitSSB*11,GastoMes)]
# 
# 
# # dt_SSB[ ,GastoUni:=Gasto/Cantidad]
# # dt_SSB[ ,GastoMensual:=Gasto]
# 
# ### Tabla de los hogares y personas
# 
# dt_vivi <- as.data.table(read.spss(paste0(wdd_in,"Viviendas y hogares.sav"), 
#                                    to.data.frame = TRUE,use.value.labels = F,r))
# 
# dc_vivi <- dt_vivi[,c("DIRECTORIO","REGION","DOMINIO","P3","P6008"),with=FALSE]
# 
# dc_vivi[ ,ESTRATO:=paste(REGION,DOMINIO,sep="-")]
# 
# dc_vivi[ ,zona:=ifelse(P3==1,"1Urban","2Rural")]
# 
# 
# 
# 
# 
# # # 
# # # dt_SSB[,zona:=1]
# # # dt_SSB[,zona:=ifelse(is.na(zona),2,zona)]
# # # #Consuperc
# # # dt_SSB[,GastoMensual:=Gasto*FactorMensual]
# # # dc_SSB <- dt_SSB[Item=="SSB",list(Litros=sum(LitrosMes*FEX_C*12,na.rm = TRUE)/1000,
# # #                        Gasto=sum(GastoMensual*FEX_C*12,na.rm = TRUE)/1000000),by=list(DIRECTORIO)]
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # Cuadro gasto
# # dt_SSB[,Grupo:=substr(Producto,1,4)]
# # 
# # dc_SSB <- dt_SSB[,list(GastoMensual=sum(Gasto*FactorMensual*FEX_C,na.rm = TRUE)),by=list(Grupo,Item)]
# # 
# # 
# # sum(dt_SSB$LitrosMes*dt_SSB$FEX_C)*12
# # 
# # # Arregla valor de la cerveza
# # 
# # dt_SSB[Producto==11110601 & Item=="Beer" & Gasto>10000 & Cantidad<=2,LitrosMes:= (Cantidad*(Gasto/1500)*pqlitBeer*FactorMensual)]
# # 
# # 
# # dt_SSB[is.na(qestandar),qestandar:=Cantidad]
# # 
# # 
# # dt_SSB[,GastoMensual:=Gasto*FactorMensual]
# # dc_SSB <- dt_SSB[,list(Litros=sum(LitrosMes*FEX_C*12,na.rm = TRUE),
# #                        Gasto=sum(GastoMensual*FEX_C*12,na.rm = TRUE)/1000000),by=list(Item)]
# # 
# # 
# # dt_SSB[,Valor:=Gasto/qestandar]
# # dc_SSB <- dt_SSB[,list(Litros=sum(LitrosMes*FEX_C*12),
# #                        Valor=mean(Valor,na.rm = TRUE),
# #                        Valorp50=quantile(Valor,0.5,na.rm = TRUE)),by=list(Item)]
# # 
# # dc_SSB[,Participacion:=Litros/sum(Litros)] 
