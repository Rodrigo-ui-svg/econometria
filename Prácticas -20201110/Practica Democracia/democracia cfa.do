cd "E:\PUED\Cursos\FE Econometria I\Curso 2020\Datos Multinivel\Democracia"
use democracia.dta, clear

sem (demo ->  p18st@1  p20stm  p21st_r  p22sta_r ),  ///
	 method(mlmv) latent(demo) standardized

estat gof, stats(all)

predict fs_demo , latent(demo)
** generar variables en escala de 0 a 100 **
gen fs2_demo = (-2.374454-fs_demo)/(-2.374454-2.660388)*100
label variable fs_demo "puntajes factoriales democracia m=0" 
label variable fs2_demo "puntajes factoriales democracia pct"
summ fs_demo fs2_demo

graph hbox fs2_demo, over(idenpa)
order fs2_demo, after( fs_demo)

bysort idenpa: summ fs2_demo
bysort idenpa: summ fs_demo

** promedios por país ** 
tabstat fs2_demo, by(idenpa) stat(mean var n) format(%7.0gc)

** Exportar a MPLUS para CFA de Democracia **
use democracia.dta, clear
stata2mplus ID p18st  p20stm  p21st_r  p22sta_r using CFA_Democracia,replace
