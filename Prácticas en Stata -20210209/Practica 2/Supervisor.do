*** Ejemplo datos del supervisor ***
*** selección del mejor modelo   ***
cd "D:\PUED\FE Econometria I\Curso 2020\Datos RMultiple\"
use Supervisor.dta, clear
regress y x1 x2
regress y x1 x2 x3 x4 x5 x6
regress y x1 x3 

asdoc regress y x1 x2

regress y x1
predict double eyx1, residual

regress x2 x1
predict double ex2x1, residual 

list eyx1 ex2x1
regress eyx1 ex2x1
