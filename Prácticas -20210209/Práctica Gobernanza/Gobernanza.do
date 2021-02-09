cd "E:\PUED\Cursos\FE Econometria I\Curso 2020\Datos Multinivel\"
import excel "Gobernanza.xlsx", sheet("Data") firstrow
rename PAIS pais
rename PPP ppp
rename V0Z_014 voz_14
rename ESTAB_14 estab_14
rename EFECTIVO_14 efect_14
rename CALIDAD_14 cald_14 
rename ESTADO_14 edrcho_14
rename CORRUP_14 corrp_14

label variable pais "Pais"
label variable voz_14 "Voz y Rendición de Cuentas"
label variable estab_14 "Estabilidad política ausencia de violencia"
label variable efect_14 "Efectividad gubernamental"
label variable cald_14 "Calidad Regulatoria"
label variable edrcho_14 "Estado de derecho"
label variable corrp_14 "Corrupción"
edit

save "Gobernanza.dta", replace
** Estadísticas descriptivas **
tabstat ppp voz_14 estab_14 efect_14 cald_14 edrcho_14 corrp_14, stat(mean sd min max)

* Modelo nulo*
xtreg ppp, i(ID) mle
xtmixed ppp || ID:, var cov(un) mle
estat ic
display "deviance = " -2*e(ll)
* Modelo 1 *
xtreg    ppp voz_14 estab_14 efect_14 cald_14 edrcho_14 corrp_14, ///
         i(ID) mle
xtmixed  ppp voz_14 estab_14 efect_14 cald_14 edrcho_14 corrp_14 || ///
         ID:, var cov(un) mle
estat ic
display "deviance = " -2*e(ll)

* Modelo I reducido (sin inclusión del tiempo) *
xtreg    ppp voz_14 estab_14 , i(ID) mle
xtmixed  ppp voz_14 estab_14 || ID:, var cov(un) mle
estat ic
display "deviance = " -2*e(ll)

* Modelo II (con inclusión del tiempo aleatorio*
xtreg    ppp voz_14 estab_14 efect_14 cald_14 edrcho_14 corrp_14  t, ///
         i(ID) mle
xtmixed  ppp voz_14 estab_14 efect_14 cald_14 edrcho_14 corrp_14  t || ID: t, cov(un) mle
estat ic
display "deviance = " -2*e(ll)

xtmixed  ppp voz_14 cald_14 corrp_14 t || ID: t, cov(un) mle
estat ic
display "deviance = " -2*e(ll)

* modelo II (con interaccion con el tiempo *
gen cald_14xt =  cald_14*t
gen corrp_14xt  = corrp_14*t
xtmixed  ppp cald_14 corrp_14 t cald_14xt corrp_14xt  || ID: t, cov(un) mle
estat ic
display "deviance = " -2*e(ll)

*** graficos ppp contra el tiempo  ***
statsby inter=_b[_cons] slope=_b[t], by(ID) saving(ols):regress ppp t
sort ID
merge m:1 ID using ols
drop _merge
twoway scatter slope inter, mlabel (pais) xtitle(Intercepto) ytitle(Pendiente)

gen pred = inter + slope*t
sort ID
twoway (line pred t, connect(ascending)), ///
   xtitle(time) ytitle("PPP")
drop num inter slope pred
erase ols.dta

save Gobernanza_full, replace
