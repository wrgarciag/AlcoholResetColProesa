
# Funcion identificar canal
fchanneldrinks <- function(data,col){
  
  data <- as.data.table(data)
  
  data[,Codigo_cuest:=as.character(substr(get(col),1,2)),]
  
  # Crea categorias
  data[,Canal:=""]
  data[Codigo_cuest %in% c("01","02"),Canal:="Hogar"]
  data[Codigo_cuest %in% c("11"),Canal:="Fuera"]
  
  data[,Codigo_cuest:=NULL]
  return(data)
  
}


## function to classify items

fclasicoicop <- function(data,col){

  data <- as.data.table(data)

  #index <- match(col,names(data))
  ## Etiqueta Productos

  # Filtra solamente alimentos
  data <- data[as.character(substr(get(col),1,2))=="01" | as.character(substr(get(col),1,2))=="02" ,]

  # Crea categorias
  data[,Item:=""]

  # Grain based staples
  # Pan y cereales 0111
  data[substr(get(col),1,4)=="0111",Item:="Bread"]

  # Meats and animal-based products
  # Meat 0112
  # Fish  0113
  data[substr(get(col),1,4) %in% c("0112","0113"),Item:="Meat"]

  # Milk
  # 011401
  data[substr(get(col),1,6)=="011401",Item:="Milk"]

  # Fruits and vegetables
  # Fruits 0116
  # vegetables 0117
  data[substr(get(col),1,4) %in% c("0116","0117"),Item:="Fruits"]

  # Sweets and candies
  # 0118 Azúcar, mermelada, miel, chocolate y dulces de azúcar
  data[substr(get(col),1,4) %in% c("0118"),Item:="Candies"]

  # Condiments and snacks
  # Frituras: papas fritas, chitos, maizitos, patacones, besitos, etc, para consumo en el hogar 011906
  data[substr(get(col),1,6) %in% c("011906"),Item:="Snacks"]

  # Tea, water and coffee
  # Cafe, te y cacao 0121
  data[substr(get(col),1,4) %in% c("0121"),Item:="Coffe"]

  # Water bottled 012201
  # Water tank 012202
  data[substr(get(col),1,6) %in% c("012201","012202"),Item:="Water"]

  # SSBs
  # 012204 Gaseosas para consumo en el hogar
  # 012205 Maltas para consumo en el hogar
  # 012206 Refrescos líquidos empacados (jugos, té fríos y bebidas hidratantes)
  # 012207 Concentrados para preparar refrescos
  # 012208 Bebidas energizantes (a base de glucosa, guaraná, cafeína, entre otros) para consumo en el hogar

  data[substr(get(col),1,6) %in% c("012204","012205","012206","012208"),Item:="SSB"]

  # Beer
  # 0213 Cerveza
  data[substr(get(col),1,4) %in% c("0213"),Item:="Beer"]

  # Yogurt/ Kimis
  #01149903	Yogurt para consumo en el hogar
  #01149904	Kumis para consumo en el hogar
  data[get(col) %in% c("01149903","01149904"),Item:="Yogurt"]

  # Guaro/aguardiente y Ron

  #021101		Aguardiente
  #021102		Ron
  data[substr(get(col),1,6) %in% c("021101","021102"),Item:="Rum/Spirits"]

  return(data)
}


fclasidrinks <- function(data,col){
  
  data <- as.data.table(data)
  
  #index <- match(col,names(data))
  ## Etiqueta Productos
  
  # Filtra solamente alimentos
  data <- data[as.character(substr(get(col),1,2))=="01" | 
               as.character(substr(get(col),1,2))=="02" | as.character(substr(get(col),1,2))=="11" ,]
  
  # Crea categorias
  data[,Item:=""]
  

  # Milk
  # 011401
  data[substr(get(col),1,6)=="011401",Item:="MilkDerivatives"]

  # Yogurt/ Kimis
  #01149903	Yogurt para consumo en el hogar
  #01149904	Kumis para consumo en el hogar
  data[get(col) %in% c("01149903","01149904"),Item:="MilkDerivatives"]

  data[substr(get(col),1,8) %in% c("11110303","11110304"),Item:="MilkDerivatives"]

  # Tea, water and coffee
  # Cafe, te y cacao 0121
  data[substr(get(col),1,4) %in% c("0121"),Item:="HotDrinks"]

  #11	1	1	02
  data[substr(get(col),1,6) %in% c("111102"),Item:="HotDrinks"]
  
  
  # Water bottled 012201
  # Water tank 012202
  data[substr(get(col),1,6) %in% c("012201","012202","012203"),Item:="Water"]
  
  # SSBs	
  # 012204 Gaseosas para consumo en el hogar
  # 012205 Maltas para consumo en el hogar
  # 012206 Refrescos líquidos empacados (jugos, té fríos y bebidas hidratantes)
  # 012207 Concentrados para preparar refrescos
  # 012208 Bebidas energizantes (a base de glucosa, guaraná, cafeína, entre otros) para consumo en el hogar
  
  data[substr(get(col),1,6) %in% c("012204","012205","012206","012208","012209"),Item:="SSB"]
  #data[substr(get(col),1,4) %in% c("0122"),Item:="SSB"]
  
  # Gaseosa y otros refrescos  en establecimientos de servicio a la mesa y autoservicio,  
  #medios de transporte, maquinas expendedoras, puestos moviles, y lugares de esparcimiento;
  #se incluyen tambien las adquiridas para llevar y servicio a domicilio
  data[substr(get(col),1,8) %in% c("11110301"),Item:="SSB"]
  
  
  # Beer
  # 0213 Cerveza
  data[substr(get(col),1,4) %in% c("0213"),Item:="Beer"]
  
  
  # Guaro/aguardiente y Ron
  
  #021101		Aguardiente
  #021102		Ron
  # 02	1	1	03		Whisky, brandy, vodka, ginebra, cognac, tequila
  # 02	1	1	03	01	Whisky, brandy, vodka, ginebra, cognac, tequila y similares para consumo en el hogar
  # 02	1	1	04		Cremas de licor (de amareto, de whisky, etc.), sabajón y ponche crema para consumo en el hogar
  # 02	1	1	04	01	Cremas de licor (de amareto, de whisky, etc.), sabajón y ponche crema para consumo en el hogar
  # 02	1	1	99		Otras bebidas alcohólicas
  #11	1	1	06		Gastos en bebidas alcohólicas (cerveza, aguardiente, ron, etc) en tiendas, fondas rurales y al aire libre

  data[substr(get(col),1,4) %in% c("0211"),Item:="Spirits"]
  data[substr(get(col),1,6) %in% c("111106"),Item:="Spirits"]
  
  # Wine
  data[substr(get(col),1,4) %in% c("0212"),Item:="Spirits"]
  
  # Identificacion del canal
  data <- fchanneldrinks(data,col)
  
  return(data)
}


ftextdrinks <- function(data,col){
  
  data <- as.data.table(data)
  
  data[,Item2:=""]
  
  # Milk
  data[grep("leche|yogurt|kumis", tolower(get(col))), "Item2"] <- "MilkDerivatives"
  
  # Water
  data[grep("\\bagua\\b|agua botella|agua con gas|botella de agua|botella con agua", tolower(get(col))), "Item2"] <- "Water"
  
  # Cafe,te,chocolate
  data[grep("tinto|cafe|capuchino|aguadepanela|aguapanela|agua panela|aromatica|agua de panela|chocolate", tolower(get(col))), "Item2"] <- "HotDrinks"
  
  # SSB
  data[grep("gaseosa|gaseosas|energizante|energizantes", tolower(get(col))), "Item2"] <- "SSB"

  data[grep("coca cola|pepsi|vive 100|refajo|pony|pony malta|jugo hit", tolower(get(col))), "Item2"] <- "SSB"

  # Spirits
  data[grep("whisky|brandy|vodka|ginebra|cognac|tequila|guaro|aguardiente|coctel|wisky|\\bron\\b|champagna", tolower(get(col))), "Item2"] <- "Spirits"

  # Wine
  data[grep("vino|vinos|wine", tolower(get(col))), "Item2"] <- "Spirits"
  #data[grep("vino", tolower(get(col))), "Item2"] <- "Wine"

  # Beer
  data[grep("cerveza|cervezas|pola|aguila|poker|pilsen|club colombia|cervesa", tolower(get(col))), "Item2"] <- "Spirits"
  #data[grep("cerveza|cervezas|pola|aguila|poker|pilsen|club colombia", tolower(get(col))), "Item2"] <- "Beer"
  
  data[, Texto:= ifelse(Item=="" & Item2!="",1,0)]
  data[,Item:=ifelse(Item=="" & Item2!="",Item2,Item)]
  
  #Corregir agua afuera clasificada como SSB
  #data[(grep("\\bagua\\b", tolower(get(col)))) & (substr(get(col),1,8) %in% c("11110301")),Item:="Water"]
  #data[(grep("/^agua$/|agua botella|agua con gas|botella de agua", tolower(get(col)))) & (substr(get(col),1,8) %in% c("11110301")),Texto:=1]
  
  data[,c("Item2"):=NULL]
  
  return(data)
  
}



festconsu <- function(data){
  
  data <- as.data.table(data)
  data[Item=="SSB", GastoMes:=ifelse(Cantidad>50,pglitSSB*FactorMensual,GastoMes)]
  data[Item=="SSB", LitrosMes:=ifelse(Cantidad>50,pqlitSSB*FactorMensual,LitrosMes)] 
  
  data[Item=="Beer", GastoMes:=ifelse(Cantidad>50,pglitBeer*FactorMensual,GastoMes)]
  data[Item=="Beer", LitrosMes:=ifelse(Cantidad>50,pqlitBeer*FactorMensual,LitrosMes)] 
  
  data[Item=="Spirits", GastoMes:=ifelse(Cantidad>50,pglitSpi*FactorMensual,GastoMes)]
  data[Item=="Spirits", LitrosMes:=ifelse(Cantidad>50,pqlitSpi*FactorMensual,LitrosMes)] 
  
  data[Item=="Wine", GastoMes:=ifelse(Cantidad>50,pglitWi*FactorMensual,GastoMes)]
  data[Item=="Wine", LitrosMes:=ifelse(Cantidad>50,pqlitWi*FactorMensual,LitrosMes)] 
  
  data[Item=="Water", GastoMes:=ifelse(Cantidad>50,pglitWa*FactorMensual,GastoMes)]
  data[Item=="Water", LitrosMes:=ifelse(Cantidad>50,pqlitWa*FactorMensual,LitrosMes)] 
  
  return(data)
}
  
  


# Grain based staples	
# Pan y cereales 0111

# Meats and animal-based products	
# Meat 0112
# Fish  0113

# Milk	
# 011401

# Fruits and vegetables	
# Fruits 0116
# vegetables 0117

# Sweets and candies	
# 0118 Azúcar, mermelada, miel, chocolate y dulces de azúcar 

# Condiments and snacks
# Frituras: papas fritas, chitos, maizitos, patacones, besitos, etc, para consumo en el hogar 011906


# Tea, water and coffee	
# Cafe, te y cacao 0121


# Water bottled 012201
# Water tank 012202

# SSBs	
# 012204 Gaseosas para consumo en el hogar
# 012205 Maltas para consumo en el hogar
# 012206 Refrescos líquidos empacados (jugos, té fríos y bebidas hidratantes)
# 012207 Concentrados para preparar refrescos
# 012208 Bebidas energizantes (a base de glucosa, guaraná, cafeína, entre otros) para consumo en el hogar



# Dairy products (excl. milk)	
# Milk derivatives
# 01149903
# 01149904
# Beer 0213

#funciones para consolidar

fselecciona <- function(data){
  if("DescriProducto" %in% colnames(data)){
    data %<>% 
      select(c(DIRECTORIO,
               Item,
               Texto,
               FEX_C,
               LitrosMes,
               GastoMes,
               Tabla,
               Canal,
               Producto,
               DescriProducto
      )) 
  }else{
    data %<>% 
      select(c(DIRECTORIO,
               Item,
               Texto,
               FEX_C,
               LitrosMes,
               GastoMes,
               Tabla,
               Canal,
               Producto
      )) 
  }
  return(data)
}


remove_outliers <- function(x, k = 1.5) {   
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)   
  h <- k * IQR(x, na.rm = TRUE)   
  x[x < (qnt[1] - h) | x > (qnt[2] + h)] <- NA
  return(x) 
}


remove_outliers_by_group <- function(df, group_col, value_col, k = 1.5) {
  require("plyr")
  df_clean <- ddply(df, group_col, function(x) {
    qnt <- quantile(x[[value_col]], probs = c(0.25, 0.75), na.rm = TRUE)
    h <- k * IQR(x[[value_col]], na.rm = TRUE)
    x[[value_col]][!(x[[value_col]] < (qnt[1] - h) | x[[value_col]] > (qnt[2] + h))] 
  })
  return(df_clean)
}

# remove_outliers <- function(data,col, k = 1.5) {
#   qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
#   h <- k * IQR(x, na.rm = TRUE)
#   x[x < (qnt[1] - h) | x > (qnt[2] + h)] <- NA
#   return(x)
#   }

# remove_outliers <- function(dt, col, k = 1.5) { 
#   qnt <- quantile(dt[[col]], probs = c(0.25, 0.75), na.rm = TRUE) 
#   h   <- k * IQR(dt[[col]], na.rm = TRUE) 
#   dt[!(dt[[col]] < (qnt[1] - h) | dt[[col]] > (qnt[2] + h)),]
#   # dt  <- dt[(dt[[col]] < (qnt[1] - h) | dt[[col]] > (qnt[2] + h)),get(col):=NA]
#   # return(dt)
# }


