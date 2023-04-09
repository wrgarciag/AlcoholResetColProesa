##.....................................................................
## Income data set

# Por consiguiente los ingresos personales corrientes, monetarios y no monetarios, mediante su
# agregacion, son componentes fundamentales para construir los ingresos corrientes de la unidad de gasto.


# Ingresos por trabajo principal de los asalariados
# Ingresos por trabajo principal de los independientes
# Ingresos de capital
# Ingresos por transferencia
# Ingresos ocasionales
# Otros ingresos*

# No. Concepto
# 1 Salario


#En este trabajo ... es: (P6430)
# Antes de descuentos ¿cuánto ganó ... el mes pasado en este empleo? 
# (Incluya propinas y comisiones, y excluya viáticos y pagos en especie) (P6500)

# ¿Cuál fue la ganancia neta o los honorarios netos de ... en esa actividad, negocio, profesión o finca, 
# el mes pasado ? Honorarios o ganancia neta en el mes pasado (P6750)

# ¿A cuántos meses corresponde lo que recibió? (P6760)

# P7070 ¿Cuánto recibió o ganó ... el mes pasado en este otro trabajo o negocio?

# P6510S1
# P6510S2 si 2, sumar
# P1653S2A1
# P1653S2A2 si 2 sumar
# P1653S3A1
# P1653S3A2 si 2 sumar
# P1653S4A1
# P1653S4A2 si 2 sumar


# 2 Alimentos
# P6180S2
# P6590S1

# 3 Vivienda
# P6600S1

# 4 Otros en especie
# P6620S1


# 5 Transporte
# P6610S1

# 6 Subsidio de alimentación
# P6585S1A1
# P6585S1A2  si 2 sumar

# 7 Subsidio de transporte
# P6585S2A1
# P6585S2A2 si 2 sumar

# 8 Subsidio familiar
# P6585S3A1
# P6585S3A2

# 9 Prima tecnica
# P1653S1A1 si 2 sumar
# P1653S1A2

# 10 Prima de servicios
# P6630S1A1 / 12


# 11 Prima de navidad
# P6630S2A1/ 12

# 12 Prima de vacaciones
# P6630S3A1

# 13 Bonificaciones
# P6630S4A1 /12
# P6630S5A1/ 12
# P6779S1 viaticos

# 14 Ganancias

# 15 Ganancias en los ultimos 12 meses

# 16 Otros trabajos
# P7422S1
# P7472S1

# 17 Trabajo
# 18 Pension
# 19 Apoyos de sostenimiento
# 20 Arriendos
# P7500S1A1
# P7500S4A1
# P7500S5A1

# 21 Primas de jubilación
# P7500S2A1
# P7500S3A1

# 22 Ayudas

# P9460S1 subsidio deempleo

# 23 Otros ingresos
# P7510S1A1 /12
# P7510S2A1 /12 remesas
# P7510S4A1 /12
# P7510S5A1 /12
# P7510S6A1 /12
# P7510S10A1 /12
# P7510S9A1 /12
# P1668S1A1 /12 Familias accion
# P1668S1A4/ 12 Familias accion

# P1668S2A2 /12 Adulto mayor
# P1668S2A4 /!2

# P1668S4A2 /12 Jovenes accion
# P1668S4A4 /12 Jovenes accion

# Subsidios educativos
# P8610S1 /12
# P8612S1
# P8612S2

# ?? OJO los otros ingresos los importa en cientifico

# dc_ingr <- dt_pers[,c("DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P","ORDEN","FEX_C",
#                       "P6430","P6500","P6750","P6760","P6070"),with=FALSE]


dc_ingr <- dt_pers[,c("DIRECTORIO",
                      "P6430","P6500","P6750","P6760","P7070","P6510S1","P1653S2A1",
                      "P1653S3A1","P1653S4A1","P6180S2","P6590S1","P6600S1","P6620S1",
                      "P6610S1","P6585S1A1","P6585S2A1","P6585S3A1","P1653S1A1",
                      "P6630S1A1","P6630S2A1","P6630S3A1","P6630S4A1","P6630S5A1","P6779S1",
                      "P7422S1","P7472S1","P7500S1A1","P7500S4A1","P7500S5A1","P7500S2A1",
                      "P7500S3A1","P9460S1","P7510S1A1","P7510S2A1","P7510S4A1","P7510S5A1",
                      "P7510S6A1","P7510S10A1","P7510S9A1","P1668S1A1","P1668S1A4",
                      "P1668S2A2","P1668S2A4","P1668S4A2","P1668S4A4","P8610S1","P8612S1","P8612S2"
),with=FALSE]


# Si el ingreso laboral reportado es < a 100 se asigna NA
dc_ingr[,P6500:=ifelse(P6500<100,NA,P6500)]

# Se dividen ganancias de negocios en unidad de tiempo para llevarla a mes
dc_ingr[,P6750:=P6750/P6760]

# Agregar todas las columnas por hogar
cols <- colnames(dc_ingr)
dc_ingr[, (cols) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = cols]

dc_ingr <- dc_ingr[, lapply(.SD, sum), by=c("DIRECTORIO")]



# P6630S1A1 / 12
# P6630S2A1/ 12
# P6630S4A1 /12
# P6630S5A1/ 12
# P7510S1A1 /12
# P7510S2A1 /12 remesas
# P7510S4A1 /12
# P7510S5A1 /12
# P7510S6A1 /12
# P7510S10A1 /12
# P7510S9A1 /12
# P1668S1A1 /12 Familias accion
# P1668S1A4/ 12 Familias accion
# P1668S2A2 /12 Adulto mayor
# P1668S2A4 /12
# P1668S4A2 /12 Jovenes accion
# P1668S4A4 /12 Jovenes accion
# P8610S1 /12

# Dividir valores anuales a mensualidad
cols <- c("P6630S1A1","P6630S2A1","P6630S4A1","P6630S5A1","P7510S1A1","P7510S2A1","P7510S4A1",
          "P7510S5A1","P7510S6A1","P7510S10A1","P7510S9A1","P1668S1A1","P1668S1A4","P1668S2A2",
          "P1668S2A4","P1668S4A2","P1668S4A4","P8610S1")

dc_ingr[, (cols) := lapply(.SD, function(x){x <- x/12; x}), .SDcols = cols]

# Eliminar columnas que no son ingresos
dc_ingr[, c("P6430","P6760"):=NULL]

# Ingreso total
#dc_ingr[,IT:=P6500+P6750]

dc_ingr_d <- dc_ingr[,c("DIRECTORIO"),with=FALSE]

dc_ingr[, c("DIRECTORIO"):=NULL]

dc_ingr$IT <- rowSums(dc_ingr)

dc_ingr <- cbind(dc_ingr_d,dc_ingr)

dc_ingr <- dc_ingr[,c("DIRECTORIO","IT"),with=FALSE]

Data <- merge(Data,dc_ingr,by=c("DIRECTORIO"),all.x=TRUE)

# Ingreso per capita
Data[,ITPC:=IT/P6008]


# Terciles de ingreso 

perc.rank <- function(x) trunc(rank(x))/length(x)

Data[,percentile:=perc.rank(ITPC)]
Data[,TercilIngr:=1]
Data[,TercilIngr:=ifelse(percentile>=0.33 & percentile <0.66,2,TercilIngr)]
Data[,TercilIngr:=ifelse(percentile>=0.66,3,TercilIngr)]

# Data[ , TercilIngr:= cut(ITPC,
#                       breaks = quantile(ITPC, probs = 0:3/3),
#                       labels = 1:3, right = FALSE)]

Data[ , Quintil:= cut(ITPC,
                          breaks = quantile(ITPC, probs = 0:5/5),
                          labels = 1:5, right = FALSE)]

## Limpiar
rm(dc_ingr,dc_ingr_d)

## Gasto total del hogar
