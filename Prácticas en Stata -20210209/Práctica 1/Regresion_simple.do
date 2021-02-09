* Directorio de trabajo
cd "C:\Users\THINKPAD T560S\Documents\ECONOMETRIA CLASE\Scripts del curso\Stata\Tema 1"

** Ejemplo de computadoras **
use Computer, clear
reg minutes units
scatter minutes units || lfit minutes units
twoway lfitci minutes units, stdf || scatter minutes units ||


** Ejemplo Anscombe **
use Anscombe.dta, clear
regr y1 x1
scatter y1 x1 || lfit y1 x1

regr y2 x2
scatter y2 x2 || lfit y2 x2

regr y3 x3
scatter y3 x3 || lfit y3 x3

regr y4 x4
scatter y4 x4 || lfit y4 x4


**Ejemplo Bienestar subjetivo**
use "BSubjetivo.dta", clear 
summ satisf2
hist satisf2
tabstat satisf2, stat(n mean sd sk ku min max)
asdoc tab1 trab edad ingreso
reg satisf2 i.trab i.edad i.ingreso
