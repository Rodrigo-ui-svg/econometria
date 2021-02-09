cd "E:\PUED\Cursos\FE Econometria I\Curso 2020\Datos Multinivel\"
use democracia.dta, clear

*** Modelo nulo  *** 
xtmixed fs_demo || idenpa:, variance mle
estat ic 
display "deviance = " -2*e(ll)
display "ICC=" .1732938/( .1732938+1.016094)*100

** Modelo I Completo **
gen  gdp_pc2=  gdp_pc/100
gen  ppp_c2 = ppp_c/10000
xtmixed fs_demo ppp_c2 voz_pct estab_pct efect_pct cald_pct edo_pct corrp_pct ///
        edad masc p1 p2 p5 p30 p47st s11||idenpa:, variance mle cov(unstructured)
estimates store r1
estat ic 
display "deviance = " -2*e(ll)
display "ICC=" .0435763 /(.0435763 +.8833973 )*100
ICC=4.2043807

*** Modelo Reducido  ***
xtmixed fs_demo  ppp_c2 edad masc p1 p2 p5 p30 p47st s11|| ///
     idenpa:, variance mle cov(unstructured)
estimates store r2	
estat ic 
display "deviance = " -2*e(ll)

lrtest r2 r1

*** Grafica de Gusano **
gllamm fs_demo ppp_c2 edad masc p1 p2 p5 p30 p47st s11, i(idenpa) ip(m) nip(15) adapt 
gllapred reff, u
gllapred murcg, linpred
egen pickone = tag(idenpa)
gsort + reffm1 -pickone
generate rank = sum(pickone)

generate labpos = reffm1 - 3.5*reffs1 - .1
keep if pickone==1
keep idenpa reffm1 reffs1 rank pickone labpos
serrbar reffm1 reffs1 rank if pickone ==1, addplot(scatter labpos rank, ///
        mlabel(idenpa) msymbol(none) mlabpos(0)) scale(1.96)             ///
		xtitle(Rank) ytitle(Prediction) legend(off) ///
		title(Ordenamiento de países usando el modelo multinivel)
