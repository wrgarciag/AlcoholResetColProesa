*********************
******************
*
* Bloque3a. Estimacion del AIDS
*
******************
*********************

log using "C:\Users\William\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Resu\SSBElasticityPaperRevised_fuera_log.log", append

*log using "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - *22-1002-PahoElasSsb\Resu\SSBElasticityPaperrevised_log.log", append

clear all
macro drop _all
set more off

global path "C:\Users\William\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Data\Output"
*global path "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - 22-1002-PahoElasSsb\Data\Output"

use "$path\DataEstimates4.dta",clear

drop Gasto*
drop Precio*

merge m:1 REGION DOMINIO TercilIngr using "$path\MeanUnitValuesTercil_Fuera.dta"
keep if _merge==3
drop _merge

merge 1:1 DIRECTORIO using "$path\Gasto_Fuera.dta"

keep if _merge==3
drop _merge

tab xeduc,gen(xeduc_)
* Summary
summarize xsexo xedad xrural xtamanioh xuniper xninios xeduc_1 xeduc_2 xeduc_3 xeduc_4 IT ITPC IT ITPC 


** QUAIDS libreria
egen totalexp = rowtotal(Gasto_HotDrinks Gasto_MilkDerivatives Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits Gasto_Wine)

/*
egen totalexptext = rowtotal(GastoTexto_SSB GastoTexto_HotDrinks GastoTexto_MilkDerivatives GastoTexto_Beer GastoTexto_Wine GastoTexto_Water GastoTexto_Spirits)

egen totalexpcodi = rowtotal(GastoCodi_HotDrinks GastoCodi_SSB GastoCodi_MilkDerivatives GastoCodi_Beer GastoCodi_Spirits GastoCodi_Water GastoCodi_Wine)

*generar consumo texto > 0 

gen consumotexto = 0
replace consumotexto = 1 if totalexptext > 0
*/

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

sum di*
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

global X  x2 x4 x6 x7 x8 x9
global P lnp1 lnp2 lnp3 lnp4 lnp5 lnp6 lnp7

* Se excluye el sexo para no sobreidentificar

*genera variables auxiliares


set more off
forvalues i=1/7 {
probit di`i' $X
predict f_d`i', xb
label var f_d`i' "Valor ajustado para el bien `i'"
generate pdf`i'=normalden(f_d`i')
generate cdf`i'=normal(f_d`i')
generate mills`i'=(di`i'*(pdf`i'/cdf`i'))+ ((1-di`i')*(pdf`i'/1-cdf`i'))
}


forvalues i=1/7 {
rename mills`i' xm`i'
}

save "$path\DataPaperTotal.dta",replace
set more on

set more off
drop FEX_C
forvalues i=1/7 {
replace w`i' = pdf`i' if w`i' == 0
}

set more on


******
** Model 1 Full
*****

set more off
aidsills w1-w7 [fweight = FEX_C_2], prices(p1-p7) expenditure(totalexp) quadratic intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9 xm1 xm2 xm3 xm4 xm5 xm6 xm7) symmetry alpha_0(10)

* Elasticities
aidsills_elas


*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on


**** Cambio de precios







































******
** Model 2 No censoring
*****

set more off
aidsills w1-w7 [fweight = FEX_C_2], prices(p1-p7) expenditure(totalexp) quadratic intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on

******
** Model 3 no quadratic
*****

set more off
aidsills w1-w7 [fweight = FEX_C_2], prices(p1-p7) expenditure(totalexp) intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9 xm1 xm2 xm3 xm4 xm5 xm6 xm7) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on


******
** Model 4 No quadratic - no censoring
*****

set more off
aidsills w1-w7 [fweight = FEX_C_2], prices(p1-p7) expenditure(totalexp) quadratic intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on



******
** Model 5 Full - no weights
*****

set more off
aidsills w1-w7, prices(p1-p7) expenditure(totalexp) quadratic intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9 xm1 xm2 xm3 xm4 xm5 xm6 xm7) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on


******
** Model 6 No censoring - no weights
*****

set more off
aidsills w1-w7, prices(p1-p7) expenditure(totalexp) quadratic intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on

******
** Model 7 no quadratic - no weights
*****

set more off
aidsills w1-w7, prices(p1-p7) expenditure(totalexp) intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9 xm1 xm2 xm3 xm4 xm5 xm6 xm7) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on


******
** Model 8 No quadratic - no censoring - no weights
*******

set more off
aidsills w1-w7, prices(p1-p7) expenditure(totalexp) intercept(x1 x2 x3 x4 x5 x6 x7 x8 x9) symmetry alpha_0(10)

* Elasticities
aidsills_elas
*by income tercile
aidsills_elas if  TercilIngr==1
aidsills_elas if  TercilIngr==2
aidsills_elas if  TercilIngr==3
*by income quintil

aidsills_elas if Quintil==1
aidsills_elas if Quintil==2
aidsills_elas if Quintil==3
aidsills_elas if Quintil==4
aidsills_elas if Quintil==5

set more on



log close
/*
quaidsce w1-w7, anot(10) prices(p1-p7) expenditure(totalexp) demographics(x1) 

estat uncompensated, atmeans
matrix uprural = r(uncompelas)
*/