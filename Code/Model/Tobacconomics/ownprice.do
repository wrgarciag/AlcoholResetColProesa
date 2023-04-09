/*
************************************
* estimating own price elasticity using Deaton method for one good
************************************

* Estimating within-cluster first stage regressions and measurement error variances

areg luvcig lexp lhsize x1, absorb(clust)
scalar sigma11=$S_E_sse / $S_E_tdf
scalar b1=_coef[lexp]
predict ruvcig, resid
gen y1cig=luvcig-_coef[lexp]*lexp-_coef[lhsize]*lhsize-_coef[x1]*x1

*Repeat for budget shares
areg bscig lexp lhsize x1, absorb(clust)
predict rbscig, resid
scalar sigma22=$S_E_sse/$S_E_tdf
scalar b0=_coef[lexp]
gen y0cig=bscig-_coef[lexp]*lexp-_coef[lhsize]*lhsize-_coef[x1]*x1
qui areg ruvcig rbscig lexp lhsize x1, absorb(clust)
scalar sigma12=_coef[rbscig]*sigma22

*Estimating income or expenditure elasticity

qui sum bscig
scalar Wbar=r(mean)
scalar Expel=1-b1+(b0/Wbar)
scalar list Expel

*Preparing data for between-cluster regression

sort clust
egen y0c= mean(y0cig), by(clust)
egen n0c=count(y0cig), by(clust)
egen y1c= mean(y1cig), by(clust)
egen n1c=count(y1cig), by(clust)
sort clust
qui by clust: keep if _n==1

ameans n0c
scalar n0=r(mean_h)
ameans n1c
scalar n1=r(mean_h)
drop n0c n1c

*Between-cluster regression

qui corr y0c y1c, cov
scalar S=r(Var_2)
scalar R=r(cov_12)
scalar num=scalar(R)-(sigma12/n0)
scalar den=scalar(S)-(sigma11/n1)
cap scalar phi=num/den

* Estimating own-price elasticity

cap scalar zeta= b1/((b0 + Wbar*(1-b1)))
cap scalar theta=phi/(1+(Wbar-phi)*zeta)
cap scalar psi=1-((b1*(Wbar-theta))/(b0+Wbar))
cap return scalar EP=(theta/Wbar)-psi
cap scalar list EP

*Estimating own-price elasticity

cap program drop elast
program elast, rclass
tempname S R num den phi theta psi
qui corr y0c y1c, cov
scalar S=r(Var_2)
scalar R=r(cov_12)
scalar num=scalar(R)-(sigma12/n0)
scalar den=scalar(S)-(sigma11/n1)
cap scalar phi=num/den
cap scalar zeta= b1/((b0 + Wbar*(1-b1)))
cap scalar theta=phi/(1+(Wbar-phi)*zeta)
cap scalar psi=1-((b1*(Wbar-theta))/(b0+Wbar))
return scalar EP=(theta/Wbar)-psi
end
elast
return list
bootstrap EP=r(EP), reps(100) seed(1): elast
*/