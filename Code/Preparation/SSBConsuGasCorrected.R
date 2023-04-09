# Prueba con nuevos calculos de factores mensuales

# ---- Total -----

total_2 <- rbind(UrbanoTotal,
                 RuralTotal)


# Estadísticas  ----

## Consumo y Gasto totales ----

ConGas2 <- total_2 %>% 
  group_by(DIRECTORIO, Item) %>% summarise(
    ConsumoAnual = sum(ConsumoAnual, na.rm = T),
    GastoAnual = sum(GastoAnual, na.rm = T)
  )


fexc <- dt_vivi %>% select(DIRECTORIO, FEX_C)
ConGas2 <- left_join(ConGas2, fexc,
                     by = "DIRECTORIO")

ConGas2 <- left_join(ConGas2, tamanio, 
                     by = "DIRECTORIO")

ConGas2 <- left_join(ConGas2, menor2anios,
                     by = "DIRECTORIO")
ConGas2 %<>% mutate(tamanioadj = xtamanioh - cantidadmenores) 


## Dejar hogares donde el consumo de bebidas sea < 16 litros ----

consumototalbebidas <- ConGas2 %>% 
  group_by(DIRECTORIO, tamanioadj) %>% 
    summarise(ConsumoAnual = sum(ConsumoAnual, na.rm = T)) 

consumototalbebidas %<>% 
  mutate(litrosdiapersona = ConsumoAnual/tamanioadj/365) 

consumototalbebidas %<>% filter(litrosdiapersona < 16)
consumototalbebidas %<>% select(DIRECTORIO,
                                litrosdiapersona) 

ConGas2 <- left_join(ConGas2, consumototalbebidas,
                     by = "DIRECTORIO")

ConGas2 %<>% filter(!is.na(litrosdiapersona)) 


### Hogares consumidores de cada bebida ----
ConGas2 %<>% mutate(Hogares = 1) 

designH <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = ConGas2)

formulaJj <- as.formula(paste('~', "Hogares"))
formulaIi <- as.formula(paste('~', "Item"))



tN1 <- svyby(formulaJj, formulaIi,
             design = designH, 
             data= ConGas2,
             FUN=svytotal, 
             vartype = "cvpct") #coeficiente de variacion % para precision

tN1 <- as.data.table(tN1)


### Personas consumidoras de cada bebida ----

formulaJj <- as.formula(paste('~', "tamanioadj"))
formulaIi <- as.formula(paste('~', "Item"))

tN2   <- svyby(formulaJj, formulaIi,
               design = designH, 
               data=ConGas2,FUN=svytotal, vartype = "cvpct")

tN2   <- as.data.table(tN2)


### Gasto total ----

formulaJj <- as.formula(paste('~', "GastoAnual"))
formulaIi <- as.formula(paste('~', "Item"))

tN3       <- svyby(formulaJj, formulaIi,
                   design = designH, 
                   data= ConGas2,
                   FUN=svytotal,
                   vartype = "cvpct",
                   na.rm=TRUE)

tN3   <- as.data.table(tN3)


total_2 <- left_join(total_2, 
                     directorio_subset,
                     by = c("DIRECTORIO"))

### Consumo total ----

formulaJj <- as.formula(paste('~', "ConsumoAnual"))
formulaIi <- as.formula(paste('~', "Item"))

tN4       <- svyby(formulaJj, formulaIi,
                   design = designH, 
                   data= ConGas2,
                   FUN=svytotal, 
                   vartype = "cvpct",
                   na.rm=TRUE)

tN4   <- as.data.table(tN4)


### Consumo por hogar ----

formulaJj <- as.formula(paste('~', "ConsumoAnual"))
formulaIi <- as.formula(paste('~', "Item"))

tN5        <- svyby(formulaJj, formulaIi,
                    design = designH, 
                    data= ConGas2,
                    FUN=svymean, 
                    vartype = "cvpct",
                    na.rm=TRUE)

tN5   <- as.data.table(tN6)

### Gasto por hogar ----

formulaJj <- as.formula(paste('~', "GastoAnual"))
formulaIi <- as.formula(paste('~', "Item"))

tN6        <- svyby(formulaJj, formulaIi,
                    design = designH, 
                    data= ConGas2 ,
                    FUN=svymean, 
                    vartype = "cvpct",
                    na.rm=TRUE)

tN6   <- as.data.table(tN8)

## Totales capita micro estimations ----
ConGas2 %<>% mutate(ConsumoCapita = ConsumoAnual/tamanioadj,
                    GastoCapita = GastoAnual/tamanioadj) 

designH <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = ConGas2)
### Consumo per capita ----
formulaJj <- as.formula(paste('~', "ConsumoCapita"))
formulaIi <- as.formula(paste('~', "Item"))

tN7 <- svyby(formulaJj, formulaIi,
             design = designH, 
             data= ConGas2,
             FUN=svymean, 
             vartype = "cvpct",
             na.rm=TRUE)

tN7 <- as.data.table(tN7)

### Consumo per capita ssb ----

ConsuPercapitaSSB <- ConGas2 %>% 
  filter(Item == "SSB")

ConsuPercapitaSSB %<>% ungroup() %>% mutate(
  q1 = weighted.quantile(ConsumoCapita,FEX_C,0.25),
  q3 = weighted.quantile(ConsumoCapita,FEX_C,0.75),
  riq = q3 - q1,
  min = q1 - 1.5*riq,
  max = q3 + 3*riq,
  atipico = ifelse(ConsumoCapita < min | ConsumoCapita > max, 1, 0)
) 


ConsuPercapitaSSB %<>% filter(atipico == 0) 

ConsuPerCapitaSSB_graph <- ggplot(ConsuPercapitaSSB) + 
  geom_histogram(aes(x = ConsumoCapita, y = ..density.., 
                     weight = FEX_C), 
                 color = "darkblue",
                 fill = "lightblue") + 
  scale_x_continuous(expand = c(0,0),
                     breaks = round(seq(0,
                                        max(ConsuPercapitaSSB$ConsumoCapita), 
                                        by = 20),1)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Per Capita Consumption",
       y = "Density") 


ConsuPerCapitaSSB_graph + theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))


### Gasto per capita ----

formulaJj <- as.formula(paste('~', "GastoCapita"))
formulaIi <- as.formula(paste('~', "Item"))

tN8 <- svyby(formulaJj, formulaIi,
             design = designH, 
             data= ConGas2,
             FUN=svymean, 
             vartype = "cvpct",
             na.rm=TRUE)

tN8 <- as.data.table(tN8)

## UnitValues ----

unitvalues <- total_2 %>% filter(Tabla != 5) %>%
  group_by(Item, DIRECTORIO) %>% summarise(
    GastoAnual = sum(GastoAnual, na.rm = T),
    ConsumoAnual = sum(ConsumoAnual, na.rm = T)
  )

ConGas2UV <- unitvalues %>% filter(GastoAnual != 0 & ConsumoAnual != 0)
ConGas2UV <- left_join(ConGas2UV, fexc,
                       by = "DIRECTORIO")

directorio_subset <- ConGas2 %>% 
  mutate(indicador = 1) %>% 
  group_by(DIRECTORIO) %>% 
  filter(row_number()== 1) %>% select(c(DIRECTORIO, indicador, tamanioadj))

ConGas2UV <- left_join(ConGas2UV,
                       directorio_subset,
                       by = "DIRECTORIO")
ConGas2UV %<>% filter(!is.na(tamanioadj)) 

ConGas2UV %<>% mutate(unitvalue = GastoAnual/ConsumoAnual) 

designUV <- svydesign(id =~ DIRECTORIO,
                      weights =~ FEX_C, 
                      data = ConGas2UV)

formulaJj <- as.formula(paste('~', "unitvalue"))
formulaIi <- as.formula(paste('~', "Item"))

tUV        <- svyby(formulaJj, formulaIi,
                    design = designUV, 
                    data= ConGas2UV,
                    FUN=svymean, 
                    vartype = "cvpct",
                    na.rm=TRUE)

tUV   <- as.data.table(tUV)


### Grafico Unit Values ----

unitvalues2_subset <- ConGas2UV %>% 
  filter(Item != "Wine" & Item != "Spirits")

unitvalues2_subset %<>% mutate(unitvalue = unitvalue/1000) 

 unitvalues2_subset %<>% 
   group_by(Item) %>% mutate(
     q1 = weighted.quantile(unitvalue,FEX_C,0.25),
     q3 = weighted.quantile(unitvalue,FEX_C,0.75),
     riq = q3-q1,
     min = q1 - (1.5*riq),
     max = q3 + (1.5*riq),
     atipico = ifelse(unitvalue < min | unitvalue > max, 1, 0)
   )
 
 unitvalues2_subset %<>% filter(atipico == 0)

unitvalues_graph <- ggplot(unitvalues2_subset, aes(x=Item, y=unitvalue, fill=unitvalue)) + 
  geom_boxplot(fatten = 2,
               lwd = 0.9) + 
  labs(x = NULL,
       y = "Unit Value (Thousands COP$)") + 
  scale_y_continuous(breaks = round(seq(min(unitvalues2_subset$unitvalue),
                                        max(unitvalues2_subset$unitvalue), 
                                        by = 1),1))
unitvalues_graph + theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(3, "pt"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black")) +
  stat_summary(fun= weighted.mean,
               geom="point", 
               shape=20, 
               show.legend = TRUE,
               size=5, 
               color="blue", 
               fill="lightblue")


### UnitValues Median ----

tUVMedian <- svyby(formulaJj, formulaIi,
              design = designUV,
              data = ConGas2UV, 
              FUN = svyquantile, 
              quantiles=0.5,
              ci = TRUE,
              vartype="ci")

tUVMedian <- as.data.table(tUVMedian)


### UnitValues p25 ----

tUVP25 <- svyby(formulaJj, formulaIi,
                   design = designUV,
                   data = ConGas2UV, 
                   FUN = svyquantile, 
                   quantiles=0.25,
                   ci = TRUE,
                   vartype="ci")

tUVP25 <- as.data.table(tUVP25)


### Unitvalues p75 ----

tUVP75 <- svyby(formulaJj, formulaIi,
                design = designUV,
                data = ConGas2UV, 
                FUN = svyquantile, 
                quantiles=0.75,
                ci = TRUE,
                vartype="ci")

tUVP75 <- as.data.table(tUVP75)


### Estimación litros persona día por bebida ----

litrosdiapersona <- ConGas2 %>% select(-c(litrosdiapersona))

litrosdiapersona <- ConGas2 %>% group_by(DIRECTORIO,
                                         Item,
                                         FEX_C,
                                         tamanioadj)
litrosdiapersona %<>% summarise(
  litrosdiapersona = ConsumoAnual/365/tamanioadj
) 

designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = litrosdiapersona)

formulaJj <- as.formula(paste('~', "litrosdiapersona"))
formulaIi <- as.formula(paste('~', "Item"))

litrosdiamean        <- svyby(formulaJj, formulaIi,
                         design = designI, 
                         data= litrosdiapersona,
                         FUN=svymean, 
                         vartype = "cvpct",
                         na.rm=TRUE)

litrosdiamedian       <- svyby(formulaJj, formulaIi,
                              design = designI, 
                              data= litrosdiapersona,
                              FUN=svyquantile, 
                              quantile = 0.5,
                              ci = TRUE,
                              vartype = "ci",
                              na.rm=TRUE)

litrosdiap25          <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= litrosdiapersona,
                               FUN=svyquantile, 
                               quantile = 0.25,
                               ci = TRUE,
                               vartype = "ci",
                               na.rm=TRUE)

litrosdiap75          <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= litrosdiapersona,
                               FUN=svyquantile, 
                               quantile = 0.75,
                               ci = TRUE,
                               vartype = "ci",
                               na.rm=TRUE)

### Estimacion consumo de SB Quintiles ----

SSBAnual <- ConGas2 %>% filter(Item == "SSB") %>% ungroup()

SSBAnual <- left_join(SSBAnual,
                      ingreso,
                      by = "DIRECTORIO"
                      )

SSBAnual %<>% mutate(
  quintil = as.factor(as.character(ntile(ITPC, 5))),
  decil = as.factor(as.character(ntile(ITPC, 10)))
) 


designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = SSBAnual)

formulaJj <- as.formula(paste('~', "ConsumoAnual"))
formulaIi <- as.formula(paste('~', "quintil"))


ConsuSSBQuintilMean   <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= SSBAnual,
                               FUN=svymean,
                               vartype = "cvpct",
                               na.rm=TRUE)

ConsuSSBQuintilMedian <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= SSBAnual,
                               FUN=svyquantile,
                               quantile = 0.5,
                               vartype = "ci",
                               ci = TRUE,
                               na.rm=TRUE)

### Estimaciones en el p75 de consumo anual ----
SSBAnual %<>% group_by(quintil) %>% mutate(
  q3_litrosanuales = weighted.quantile(ConsumoAnual, FEX_C, 0.75)
) 

SSBAnualP75 <- SSBAnual %>% filter(ConsumoAnual <= q3_litrosanuales)

designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = SSBAnualP75)

formulaJj <- as.formula(paste('~', "ConsumoAnual"))
formulaIi <- as.formula(paste('~', "quintil"))


ConsuSSB75QuintilMean   <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= SSBAnual,
                               FUN=svymean,
                               vartype = "cvpct",
                               na.rm=TRUE)

ConsuSSB75QuintilMedian <- svyby(formulaJj, formulaIi,
                               design = designI, 
                               data= SSBAnual,
                               FUN=svyquantile,
                               quantile = 0.5,
                               vartype = "ci",
                               ci = TRUE,
                               na.rm=TRUE)


## Estimacion totales macro estimation ----

### Total personas que consumen y hogares ----
Hogares <- ConGas2 %>% 
  filter(row_number()==1) %>% 
  select(DIRECTORIO, 
         Hogares,
         FEX_C, 
         tamanioadj, 
         GastoAnual, 
         ConsumoAnual) %>% 
  mutate(factor = 1)



designI <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = Hogares)

formulaJj <- as.formula(paste('~', "Hogares"))
formulaIi <- as.formula(paste('~', "factor"))

thogares        <- svyby(formulaJj, formulaIi,
                    design = designI, 
                    data= Hogares,
                    FUN=svytotal, 
                    vartype = "cvpct",
                    na.rm=TRUE)

formulaJj <- as.formula(paste('~', "tamanioadj"))
formulaIi <- as.formula(paste('~', "factor"))

tpersonas        <- svyby(formulaJj, formulaIi,
                         design = designI, 
                         data= Hogares,
                         FUN=svytotal, 
                         vartype = "cvpct",
                         na.rm=TRUE)
#Consumo y Gasto Total

ConGas2 %<>% mutate(factor = 1) #esto se crea para poder usar svyby y obtener cvpct

designJ <- svydesign(id =~ DIRECTORIO,
                     weights =~ FEX_C, 
                     data = ConGas2) 
#para consumo y gasto total se hace con ConGas2 sin agrupar por item



formulaJj <- as.formula(paste('~', "ConsumoAnual"))
formulaIi <- as.formula(paste('~', "factor"))
tconsumo <- svyby(formulaJj, formulaIi,
                design = designJ, 
                data= ConGas2,
                FUN=svytotal, 
                vartype = "cvpct",
                na.rm=TRUE)


formulaJj <- as.formula(paste('~', "GastoAnual"))
formulaIi <- as.formula(paste('~', "factor"))
tgasto <- svyby(formulaJj, formulaIi,
                design = designJ, 
                data= ConGas2,
                FUN=svytotal, 
                vartype = "cvpct",
                na.rm=TRUE)




# ---- unitvalues_2 -----

unitvalues_2 <- total_2 %>%
  group_by(DIRECTORIO, Item) %>% summarise(
             litrosmes = sum(LitrosMes, na.rm = T),
             GastoMes = sum(GastoMes, na.rm = T)
           )

unitvalues_2 %<>% mutate(
    unitvalue = GastoMes/litrosmes
  )


unitvalues_2 %<>% filter(!is.nan(unitvalue)) 
unitvalues_2 %<>% filter(unitvalue > 0) 


ConGas2 <- total_2 %>%
  group_by(DIRECTORIO, Item, Texto) %>% summarise(
    litrosmes = sum(LitrosMes),
    gastomes = sum(GastoMes)
  )

ConGas_2_codi <- ConGas2 %>% filter(Texto == 0)
ConGas_2_texto <- ConGas2 %>% filter(Texto == 1)


# PEgar a unit values subset de variables

unitvalues_2 <- left_join(unitvalues_2, 
                          CovariableSubSet, 
                          by = c("DIRECTORIO"))

unitvalues_2 %<>% select(DIRECTORIO,
                         FEX_C,
                         REGION,
                         DOMINIO,
                         TercilIngr,
                         Item,
                         unitvalue) 
unitvalues_region <- unitvalues_2 %>% 
  group_by(REGION,
           TercilIngr,
           Item) %>% summarise(
             unitvalue = weighted.mean(unitvalue, FEX_C)
           )

unitvalues_2 %<>% 
  group_by (REGION, 
            DOMINIO,
           TercilIngr,
           Item) %>%
  summarise(unitvalue = weighted.mean(unitvalue, FEX_C)) 


unitvalues_2 %<>% pivot_wider(names_from = Item,
                                names_prefix = "PrecioM_",
                                values_from = unitvalue)  

unitvalues_region %<>% pivot_wider(names_from = Item,
                              names_prefix = "PrecioMR_",
                              values_from = unitvalue)


unitvalues_2 <- left_join(unitvalues_2, 
                          unitvalues_region, 
                          by = c("REGION",
                                 "TercilIngr"))

unitvalues_2 <- unitvalues_2 %>% mutate(
  PrecioM_Beer = ifelse(is.na(PrecioM_Beer), PrecioMR_Beer, PrecioM_Beer),
  PrecioM_Wine = ifelse(is.na(PrecioM_Wine), PrecioMR_Wine, PrecioM_Wine)
)

unitvalues_2 %<>% select(c(REGION,
                           DOMINIO,
                           TercilIngr,
                           PrecioM_Beer,
                           PrecioM_HotDrinks,
                           PrecioM_MilkDerivatives,
                           PrecioM_SSB,
                           PrecioM_Spirits,
                           PrecioM_Water,
                           PrecioM_Wine
                           )) 


# Base de datos para elasticidades

ConGas2 <- total_2 %>%
  group_by(DIRECTORIO, Item, Texto) %>% summarise(
    litrosmes = sum(LitrosMes),
    gastomes = sum(GastoMes)
  )

ConGas2 <- left_join(ConGas2, 
                     directorio_subset, 
                     by = "DIRECTORIO")
ConGas2 %<>% filter(!is.na(gastomes)) 
ConGas2 %<>% filter(!is.na(indicador)) 

ConGas_2_codi <- ConGas2 %>% filter(Texto == 0)
ConGas_2_texto <- ConGas2 %>% filter(Texto == 1)


ConGas2 %<>% select(DIRECTORIO, 
                    Item,
                    gastomes
                    ) 

ConGas2 %<>% group_by(DIRECTORIO, Item) %>% 
  summarise(
    gastomes = sum(gastomes, na.rm = T)
  )
ConGas2 %<>% pivot_wider(names_from = Item,
                         values_from = gastomes,
                         names_prefix = "Gasto_") 
ConGas2[is.na(ConGas2)] <- 0

ConGas_2_codi %<>% select(DIRECTORIO,
                          Item,
                          gastomes) 

ConGas_2_codi %<>% pivot_wider(names_from = Item,
                         values_from = gastomes,
                         names_prefix = "GastoCodi_") 
ConGas_2_codi[is.na(ConGas_2_codi)] <- 0


ConGas_2_texto %<>% select(DIRECTORIO,
                           Item,
                           gastomes) 

ConGas_2_texto %<>% pivot_wider(names_from = Item,
                               values_from = gastomes,
                               names_prefix = "GastoTexto_") 
ConGas_2_texto[is.na(ConGas_2_texto)] <- 0


ConGas2 <- left_join(ConGas2, ConGas_2_codi, by = "DIRECTORIO")

ConGas2 <- left_join(ConGas2, ConGas_2_texto, by = "DIRECTORIO")

ConGas2[is.na(ConGas2)] <- 0 

ConGas2 <- left_join(ConGas2, CovariableSubSet, by = "DIRECTORIO")

ConGas2 <- left_join(ConGas2, unitvalues_2, 
                     by = c("REGION", "DOMINIO", "TercilIngr"))

write_dta(ConGas2, paste0(wdd_out, "DataEstimates4.dta"))
