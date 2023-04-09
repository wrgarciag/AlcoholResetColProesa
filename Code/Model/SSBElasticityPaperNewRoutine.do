*********************
******************
*
* Bloque3a. Estimacion del AIDS
*
******************
*********************

*log using "C:\Users\William\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Resu\SSBElasticityPaperRevised_hogar_log.log", append

*log using "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - *22-1002-PahoElasSsb\Resu\SSBElasticityPaperrevised_log.log", append

clear all
macro drop _all
set more off

*global path "C:\Users\William\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Data\Output"
global path "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Data\Output"

use "$path\DataEstimates4.dta",clear

drop Gasto*
drop Precio*

*merge m:1 DIRECTORIO using "$path\MeanUnitValuesHousehold_Hogar.dta"
merge m:1 DIRECTORIO using "$path\QualityAdjustedPrices_Hogar.dta"
keep if _merge==3
drop _merge

/*
merge m:1 REGION DOMINIO TercilIngr using "$path\MeanUnitValuesTercil_Hogar.dta"
keep if _merge==3
drop _merge
*/


/*
gen Cluster=substr(VIVIENDA,1,10)
rename PrecioM_SSB PrecioR_SSB
rename PrecioM_HotDrinks PrecioR_HotDrinks
rename PrecioM_MilkDerivatives PrecioR_MilkDerivatives
rename PrecioM_Water PrecioR_Water
rename PrecioM_Beer PrecioR_Beer
rename PrecioM_Spirits PrecioR_Spirits
rename PrecioM_Wine PrecioR_Wine

merge m:1 REGION DOMINIO Cluster using "$path\MeanUnitValuesCluster_Hogar.dta"
keep if _merge==3
drop _merge
*/

merge 1:1 DIRECTORIO using "$path\Gasto_Hogar.dta"

keep if _merge==3
drop _merge

tab xeduc,gen(xeduc_)

* Summary
summarize xsexo xedad xrural xtamanioh xuniper xninios xeduc_1 xeduc_2 xeduc_3 xeduc_4 IT ITPC IT ITPC 


** QUAIDS libreria
egen totalexp = rowtotal(Gasto_HotDrinks Gasto_MilkDerivatives Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits Gasto_Wine)

*egen totalexptext = rowtotal(GastoTexto_SSB GastoTexto_HotDrinks GastoTexto_MilkDerivatives GastoTexto_Beer GastoTexto_Wine GastoTexto_Water GastoTexto_Spirits)

*egen totalexpcodi = rowtotal(GastoCodi_HotDrinks GastoCodi_SSB GastoCodi_MilkDerivatives GastoCodi_Beer GastoCodi_Spirits GastoCodi_Water GastoCodi_Wine)

*generar consumo texto > 0 

*gen consumotexto = 0
*replace consumotexto = 1 if totalexptext > 0

local expe "Gasto_HotDrinks Gasto_MilkDerivatives Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits Gasto_Wine"
 
foreach i of local expe {
        replace `i' =0 if mi(`i')
}
  
gen w1= Gasto_SSB/totalexp
gen w2= Gasto_HotDrinks/totalexp
gen w3= Gasto_MilkDerivatives/totalexp
gen w4= Gasto_Water/totalexp

gen w5= Gasto_Beer/totalexp
gen w6= Gasto_Spirits/totalexp
gen w7= Gasto_Wine/totalexp

rename PrecioM_SSB p1
rename PrecioM_HotDrinks p2
rename PrecioM_MilkDerivatives p3
rename PrecioM_Water p4

rename PrecioM_Beer p5
rename PrecioM_Spirits p6
rename PrecioM_Wine p7

order p1 p2 p3 p4 p5 p6 p7

* dummies de consumo
gen di1= (Gasto_SSB>0)
gen di2= (Gasto_HotDrinks>0)
gen di3= (Gasto_MilkDerivatives>0)
gen di4= (Gasto_Water>0)

gen di5= (Gasto_Beer>0)
gen di6= (Gasto_Spirits>0)
gen di7= (Gasto_Wine>0)

rename xsexo x1
rename xedad x2
rename xrural x3
rename xtamanioh x4
rename xuniper x5
rename xninios x6
rename primaria x7
rename secundaria x8
rename superior x9
rename ningunaed x10


* Logaritmos
* logaritmos de precios

forvalues i=1/7 {
gen lnp`i'=log(p`i')
}

*logaritmo gasto
gen lnexp=log(totalexp)


****************************
* Probit regressions
****************************

global X  x2  x4 x6 x7 x8 x9
global P lnp1 lnp2 lnp3 lnp4 lnp5 lnp6 lnp7

* Se excluye el sexo para no sobreidentificar

*genera variables auxiliares

set more off

sample 10
quaidsce w1-w7, anot(10) prices(p1-p7) expenditure(totalexp) demographics(x1) 

estat uncompensated
matrix unt = r(uncompelas)
matrix list unt

estat expenditure
estat uncompensated if TercilIngr==1, atmeans



global X  x2 x4 x6 x7 x8 x9
global P lnp1 lnp2 lnp3 lnp4 lnp5 lnp6 lnp7

* Se excluye el sexo para no sobreidentificar

*genera variables auxiliares


set more off
forvalues i=1/7 {
probit di`i' $X $P
predict f_d`i', xb
label var f_d`i' "Valor ajustado para el bien `i'"
generate pdf`i'=normalden(f_d`i')
generate cdf`i'=normal(f_d`i')
generate mills`i'=(di`i'*(pdf`i'/cdf`i'))+ ((1-di`i')*(pdf`i'/1-cdf`i'))
}

