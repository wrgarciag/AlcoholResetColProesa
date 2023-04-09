##.....................................................................
## Household data set
# P3 Clase
# P8520S1A1 Estrato
# P4040 Acueducto
# P5230 ¿Usted se considera pobre? 
# P6008 Total de personas en el hogar 
# PERIODO

dc_vivi <- dt_vivi[,c("VIVIENDA", "DIRECTORIO","SECUENCIA_ENCUESTA","SECUENCIA_P",
                      "ORDEN","FEX_C","REGION","DOMINIO","P3",
                      "P6008","PERIODO"),with=FALSE]
# Ojo convertir en numerico el factor 

dc_vivi[,P6008:=as.numeric(as.character(P6008))]

table(dc_vivi$P6008)


# P6020 Sexo: 1 - Hombre 2 - Mujer
# P6040 ¿Cuántos años cumplidos tiene ... ?
# P6050	¿Cuál es el parentesco de ... con el ó la jefe del hogar? 
# P6080 De acuerdo con su cultura, pueblo, o rasgos físicos, ... es o se reconoce como:
# ¿Actualmente ... asiste al preescolar, escuela, colegio o universidad? (P6170)
# ¿Cuál es el nivel educativo más alto alcanzado por ... ? (P6210)
# ¿Cuál es el título o diploma de mayor nivel educativo que Usted ha recibido? (P6210S2)

dc_pers <- dt_pers[,c("DIRECTORIO","SECUENCIA_ENCUESTA","ORDEN",
                      "P6020","P6040","P6050","P6080","P6170","P6210","P6210S2"),with=FALSE]


## Solo jefes de hogar
dc_hhead <- dc_pers[P6050==1,]

## Cruzar hoar y caracteristicas de los jefes
Data <- merge(dc_vivi,dc_hhead,by=c("DIRECTORIO","SECUENCIA_ENCUESTA","ORDEN"),all.x = TRUE)

# Seleccionamos unicamente hogares urbanos
#Data <- Data[P3==1,]

Data[,xrural:=as.numeric(P3!=1)]

# Covariables
# As demographic controls we used age, gender, and education of the head of household, 
# and household size, as well as dummy variables indicating the presence of children and single households.

# Hombre= 1
Data[,xsexo:=as.numeric(P6020==1)]

# Edad
Data[,xedad:=P6040]
Data[,xgedad:=cut(xedad,breaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                    labels = c("1","7","12","17","22","27","32","37","42","47","52","57","62","67","72",
                               "77","82","85"),
                    right = FALSE)]
# Education
# 1 » Ninguno
# 2 » Preescolar
# 3 » Basica Primaria
# 4 » Basica Secundaria
# 5 » Media
# 6 » Superior o Universitaria
# 9 » "No sabe, no informa"
Data[,xeduc:="Primaria"]
Data[,xeduc:=ifelse(P6210 %in% c(1,2,19),"Ninguno",xeduc)]
Data[,xeduc:=ifelse(P6210 %in% c(4,5),"Secundaria/Media",xeduc)]
Data[,xeduc:=ifelse(P6210 %in% c(6),"Superior",xeduc)]

# Personas hogar
Data[,xtamanioh:=P6008]

# Unipersonales
Data[,xuniper:=as.numeric(xtamanioh==1)]

# Ninios < 5
dc_ninios <- as.vector(unique(dc_pers[P6040<=5,c("DIRECTORIO"),with=FALSE]))
Data[,xninios:=as.numeric(DIRECTORIO %in% unique(dc_ninios$DIRECTORIO))]

## Limpiar
rm(dc_vivi,dc_pers,dc_hhead,dc_ninios)