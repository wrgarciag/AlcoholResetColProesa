*********************
******************
*
* Bloque3a. Estimacion del AIDS
*
******************
*********************

*log using "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - *22-1002-PahoElasSsb\Resu\SSBElasticityPaperrevised_log.log", append

*log using "$path\SSBElasticityPaper_hogar_quaids_log.log", append

use "$path\DataEstimates5.dta",clear

gen Cluster= substr(VIVIENDA,1,8)
encode Cluster, gen(psu)

rename GastoTotal exptotal
drop Gasto*
drop Precio*

merge m:1 DIRECTORIO using "$path\QualityAdjustedPrices_Hogar.dta"
keep if _merge==3
drop _merge
merge 1:1 DIRECTORIO using "$path\UnitValueGasto_Hogar.dta"
keep if _merge==3
drop _merge

tab xeduc,gen(xeduc_)
* Summary
summarize xsexo xedad xrural xtamanioh xuniper xninios xeduc_1 xeduc_2 xeduc_3 xeduc_4 IT ITPC IT ITPC 

** QUAIDS libreria
local expe "Gasto_Dairy Gasto_SSB Gasto_Water Gasto_Beer Gasto_Spirits"
 
foreach i of local expe {
        replace `i' =0 if mi(`i')
}

egen totalexp = rowtotal(Gasto_SSB Gasto_Beer Gasto_Spirits Gasto_Water Gasto_Dairy)

gen ei1= Gasto_SSB if Gasto_SSB>0
gen ei2= Gasto_Beer if Gasto_Beer>0
gen ei3= Gasto_Spirits if Gasto_Spirits>0
gen ei4= Gasto_Water if Gasto_Water>0

gen ei5= Gasto_Dairy if Gasto_Dairy>0

gen w1= Gasto_SSB/totalexp
gen w2= Gasto_Beer/totalexp
gen w3= Gasto_Spirits/totalexp
gen w4= Gasto_Water/totalexp

gen w5= Gasto_Dairy/totalexp

rename PrecioM_SSB p1
rename PrecioM_Beer p2
rename PrecioM_Spirits p3
rename PrecioM_Water p4
rename PrecioM_Dairy p5

order p1 p2 p3 p4 p5

* dummies de consumo
gen di1= (Gasto_SSB>0)
gen di2= (Gasto_Beer>0)
gen di3= (Gasto_Spirits>0)
gen di4= (Gasto_Water>0)

gen di5= (Gasto_Dairy>0)

*Cantidades
rename Cantidades_SSB q1
rename Cantidades_Beer q2
rename Cantidades_Spirits q3
rename Cantidades_Water q4
rename Cantidades_Dairy q5

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
*putexcel set "$prtab\market_hogar.xlsx", modify
*summarize q* [aweight=FEX_C]   
*putexcel B2 = `e(count)'

sum di* [aweight=FEX_C]
sum ei* [aweight=FEX_C]
sum w* [aweight=FEX_C]
sum p* [aweight=FEX_C]
sum q* [aweight=FEX_C]


* Logaritmos
* logaritmos de precios

forvalues i=1/5 {
gen lnp`i'=log(p`i')
}


forvalues i=1/5 {
gen luv`i'=log(p`i')
}

******
** Model 1 Full
*****
*drop if totalexp==0
* Robustez QUAIDS con censura
set more off
aidsills w1-w5, prices(p1-p5) expenditure(totalexp) quadratic intercept(x1) alpha_0(10)
aidsills_elas

quaidsce w1-w5, prices(p1-p5) expenditure(totalexp) demographics(x1) anot(10)

estat expenditure

matrix i = r(elas_i)
matrix list i

matrix ie = r(se_elas_i)
matrix list ie

estat uncompensated
matrix u = r(elas_u)
matrix list u

matrix ue = r(se_elas_u)
matrix list ue

estat compensated

matrix c = r(elas_c)
matrix list c

matrix ce = r(se_elas_c)
matrix list ce













