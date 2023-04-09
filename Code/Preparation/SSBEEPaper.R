##.....................................................................
## Cuadro Consumo hogares

Data <- readRDS(Data,file=paste0(wdd_out,"DataEstimates.rds"))

dt_SSB <- as.data.table(readRDS(file=paste0(wdd_out,"DataSSB.rds")))

p99 <- quantile(dt_SSB$Precio,.95)
  
g <- ggplot(dt_SSB[Item!="Spirits" & Precio<p99,], aes(x = Item, y = Precio)) + geom_boxplot()
g <- g + labs(title="Item", y = "ValorReportado")
g <- g + guides(shape = FALSE)
g <- g + theme_minimal() 
g <- g + theme_bw() 
g <- g + theme(text = element_text(size = 15), axis.text.x = element_text(angle = 60, hjust = 1))
g


## Shares SSB/Ingreso

dc_g <- dt_SSB[Item=="SSB",c("DIRECTORIO","Gasto","chogar"),with=FALSE]
dc_g <- reshape(dc_g,idvar=c("DIRECTORIO"),timevar="chogar",direction = "wide",sep = "SSB")
dc_g <- merge(Data,dc_g, by=c("DIRECTORIO"), all.x = TRUE,all.y = TRUE)

dc <- dc_g[,list(Ingreso=sum(IT*FEX_C,na.rm = TRUE),
                 GastoSSB1=sum(GastoSSB1*FEX_C,na.rm = TRUE),
                 GastoSSB0=sum(GastoSSB0*FEX_C,na.rm = TRUE)),by=list(Quintil)]

dc[,Share:=Gasto/Ingreso]


## Shares SSB Texto vs otros

dc <- dc_g[,list(Ingreso=sum(IT*FEX_C,na.rm = TRUE),
                 GastoSSB1=sum(GastoSSB1*FEX_C,na.rm = TRUE),
                 GastoSSB0=sum(GastoSSB0*FEX_C,na.rm = TRUE)),by=list(Quintil)]

dc[,GT:=GastoSSB1+GastoSSB0]
dc[,GTS1:=GastoSSB1/GT]
dc[,GTS0:=GastoSSB0/GT]

dc <- melt(dc[!is.na(Quintil)],id.vars = c("Quintil","Ingreso"),variable.name = "Finalidad",value.name = "Gasto")
  

