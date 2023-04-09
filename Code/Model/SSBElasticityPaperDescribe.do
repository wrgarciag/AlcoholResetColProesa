use "$path\DataEstimates5.dta",clear

drop Gasto*
drop Precio*
tab xeduc,gen(xeduc_)
* Summary
estpost summarize xsexo xedad xrural xtamanioh xuniper xninios xeduc_1 xeduc_2 xeduc_3 xeduc_4 IT ITPC [aweight=FEX_C]
esttab using "$prtab\demographics.csv", replace cells("count sum_w mean sd min max")


/*
webuse food,clear
drop lnp1 lnp2 lnp3 lnp4 lnexp
set seed 1
generate nkids = int(runiform()*4)
generate rural = (runiform() > 0.7)
aidsills w1-w4, prices(p1-p4) expenditure(expfd) symmetry alpha_0(10)

aidsills_elas
outreg2 using myfile, replace see
/*