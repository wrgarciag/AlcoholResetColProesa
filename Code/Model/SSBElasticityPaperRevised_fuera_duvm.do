*********************
******************
*
* Bloque3a. Estimacion del AIDS
*
******************
*********************

*log using "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - *22-1002-PahoElasSsb\Resu\SSBElasticityPaperrevised_log.log", append

use "$path\DataEstimates5.dta",clear

gen Cluster= substr(VIVIENDA,1,8)
encode Cluster, gen(psu)

replace GastoTotal=2.16e+07 if GastoTotal>2.16e+07
rename GastoTotal exptotal
drop Gasto*
drop Precio*

merge m:1 DIRECTORIO using "$path\UnitValuePrices_Fuera.dta"
keep if _merge==3
drop _merge

merge 1:1 DIRECTORIO using "$path\UnitValueGasto_Fuera.dta"
keep if _merge==3
drop _merge

tab xeduc,gen(xeduc_)
* Summary
summarize xsexo xedad xrural xtamanioh xuniper xninios xeduc_1 xeduc_2 xeduc_3 xeduc_4 IT ITPC IT ITPC 


** QUAIDS libreria
local expe "Gasto_HotDrinks Gasto_MilkDerivatives Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits Gasto_Wine"
 
foreach i of local expe {
        replace `i' =0 if mi(`i')
}

egen totalexp = rowtotal(Gasto_HotDrinks Gasto_MilkDerivatives Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits)

gen ei1= Gasto_SSB if Gasto_SSB>0
gen ei2= Gasto_Beer if Gasto_Beer>0
gen ei3= Gasto_Spirits if Gasto_Spirits>0
gen ei4= Gasto_Water if Gasto_Water>0

gen ei5= Gasto_MilkDerivatives if Gasto_MilkDerivatives>0
gen ei6= Gasto_HotDrinks if Gasto_HotDrinks>0
gen ei7= Gasto_Wine if Gasto_Wine>0

  
gen w1= Gasto_SSB/totalexp
gen w2= Gasto_Beer/totalexp
gen w3= Gasto_Spirits/totalexp
gen w4= Gasto_Water/totalexp

gen w5= Gasto_MilkDerivatives/totalexp
gen w6= Gasto_HotDrinks/totalexp
gen w7= Gasto_Wine/totalexp

rename PrecioM_SSB p1
rename PrecioM_Beer p2
rename PrecioM_Spirits p3
rename PrecioM_Water p4
rename PrecioM_MilkDerivative p5
rename PrecioM_HotDrinks p6
rename PrecioM_Wine p7

order p1 p2 p3 p4 p5 p6 p7

* dummies de consumo
gen di1= (Gasto_SSB>0)
gen di2= (Gasto_Beer>0)
gen di3= (Gasto_Spirits>0)
gen di4= (Gasto_Water>0)

gen di5= (Gasto_MilkDerivatives>0)
gen di6= (Gasto_HotDrinks>0)
gen di7= (Gasto_Wine>0)

*Cantidades
rename Cantidades_SSB q1
rename Cantidades_Beer q2
rename Cantidades_Spirits q3
rename Cantidades_Water q4
rename Cantidades_MilkDerivative q5
rename Cantidades_HotDrinks q6
rename Cantidades_Wine q7

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

****************************
* Summarize
****************************

sum di* [aweight=FEX_C]
sum ei* [aweight=FEX_C]
sum w* [aweight=FEX_C]
sum p* [aweight=FEX_C]
sum q* [aweight=FEX_C]

*estpost summarize q* [aweight=FEX_C]   
*esttab using market_hogar.xls, replace cells("count sum_w mean sd min max")

* Logaritmos
* logaritmos de precios

forvalues i=1/7 {
gen lnp`i'=log(p`i')
}


forvalues i=1/7 {
gen luv`i'=log(p`i')
}

rename w1 wssb
rename w2 wbee
rename w3 wspi
rename w4 wwat

rename w5 wmil
rename w6 whot
rename w7 wwin

rename luv1 luvssb 
rename luv2 luvbee
rename luv3 luvspi
rename luv4 luvwat

rename luv5 luvmil
rename luv6 luvhot
rename luv7 luvwin

*drop if totalexp==0

* Modelo 1
duvm ssb bee spi wat mil hot, hhsize(x4) expend(exptotal) hweight(FEX_C) cluster(psu) region(x3) indcat(x1 x5 x6 x7 x8 x9) indcon(x2) csb(1) dregres(1) hgroup(Tercil) boot(100)

* Modelo 2 Sin censura
duvm ssb bee spi wat mil hot, hhsize(x4) expend(exptotal) hweight(FEX_C) cluster(psu) region(x3) indcat(x1 x5 x6 x7 x8 x9) indcon(x2) csb(0) dregres(1) boot(100)

* Modelo 3 Sin Fex
gen fex2=1
duvm ssb bee spi wat mil hot, hhsize(x4) expend(exptotal) hweight(fex2) cluster(psu) region(x3) indcat(x1 x5 x6 x7 x8 x9) indcon(x2) csb(1) dregres(1) boot(100)

* Modelo 4 Solo codificado


log close
