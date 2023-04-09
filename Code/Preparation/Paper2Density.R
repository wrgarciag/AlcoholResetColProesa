
## Chack 

data <- copy(dt_SSB)

## Categorias
data[,Item:=ifelse(Item=="SSB","1SSB",Item)]
data[,Item:=ifelse(Item=="Beer","2Beer",Item)]
data[,Item:=ifelse(Item=="Spirits","3Spirits",Item)]
data[,Item:=ifelse(Item=="Wine","4Wine",Item)]
data[,Item:=ifelse(Item=="HotDrinks","5Hot drinks",Item)]
data[,Item:=ifelse(Item=="MilkDerivatives","6Milk",Item)]
data[,Item:=ifelse(Item=="Water","7Water",Item)]


dc <- data[,list(nag=sum(is.na(GastoMes)),
                 nac=sum(is.na(LitrosMes))),by=list(Tabla,Item)]

dc_total <- data[,list(gasto=sum(GastoMes*FEX_C,na.rm = T),
                       litros=sum(LitrosMes*FEX_C,na.rm = T)),by=list(Tabla,Item)]
  
## Cantidad originales
data <- data[,c("Cantidad","LitrosMes","Gasto","GastoMes","Item","FEX_C"),with=FALSE]

summary(data$GastoMes)
p99g <- quantile(data$GastoMes,0.975,na.rm = TRUE)
p99g

data[,GastoMes:=ifelse(GastoMes>p99g,NA,GastoMes)]

summary(data$LitrosMes)
p99q <- quantile(data$LitrosMes,0.975,na.rm = TRUE)
p99q

data[,LitrosMes:=ifelse(LitrosMes>p99q,NA,LitrosMes)]

data[,Vu:=GastoMes/LitrosMes]
summary(data$Vu)
p99q <- quantile(data$Vu,0.975,na.rm = TRUE)
p99q

data[,Vu:=ifelse(Vu>p99q,NA,Vu)]


g <- ggplot(data = data, aes(x=Vu)) 
#g <- g + geom_histogram(aes(y=..density.., weight = FEX_C), colour="black", fill="blue")
#g <- g + geom_histogram(aes(weight = FEX_C, position="dodge"))
g <- g + geom_density(aes(weight=FEX_C,color=Item), alpha = 0.4)
g <- g + guides(color=FALSE)
g <- g + facet_wrap( ~ Item)
g <- g + scale_fill_brewer()
g <- g + ylab("Densidad") + xlab("")
g <- g + theme_bw()
g <- g + theme(text=element_text(family="Times New Roman", face="bold", size=12))
g <- g + theme(strip.text = element_text(size=20))
g

## Cantidad imputada


