cd "F:\PUED\Cursos\Modelos Multinivel\Datos\logistico"
** Instalar rutinas spost13 **
findit spost13

** Ejemplo modelo logísitco y comparación con el probit **
use laborforce.dta
recode age (30/39=1)(40/49=2)(50/60=3), gen(agecat)
label var agecat "Age in categories"
label define agecatl 1 "30-39" 2 "40-49" 3 "50-60"
label values agecat agecatl
order agecat, after(age)

gen k5_1=0
recode k5_1 (0=1) if k5==1
gen k5_2=0
recode k5_2 (0=1) if k5==2
gen k5_3=0
recode k5_3 (0=1) if k5==3
order k5_1 k5_2 k5_3, after(k5)

describe
logit lfp k5 k618 age wc hc lwg inc
estimates store Mlogit

probit lfp k5 k618 age wc hc lwg inc
estimates store Mprobit

estimates table Mlogit Mprobit, b(%9.3f) t varlabel varwidth(30)

logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, or

tab k5
tab lfp k5
logit lfp i.k5

** LRT test ***
logit lfp k5 k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store Mfull

logit lfp k618 i.agecat i.wc i.hc lwg inc, nolog
estimates store Mnok5

lrtest Mfull Mnok5

logit lfp k5 i.agecat i.wc i.hc lwg inc, nolog
estimates store Mnok618

lrtest Mfull Mnok618

estimates restore Mfull
test 1.hc 1.wc
test 1.hc=1.wc
test 2.agecat=3.agecat

logit nqx1 rcschol1 alchdep s0evruse s0fdrglf s0alsc2 s0dlsc2  s1a6sc2 ///   s1d6sc2 s16mouse  s1fdrg6m  s0offhx0 propcrim violcrim drugcrim ///
prntcrim prntalch  s1pfrarr ptsd  gnginv30 pst30 binge1

logit lft 
