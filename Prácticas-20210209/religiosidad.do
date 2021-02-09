cd "C:\PUED\Cursos\Modelos Multinivel\Datos\logistico" 
cd "E:\PUED\Cursos\FEconometria 1\Datos Multinivel"
use religiosidad_small.dta


/* First, tabulate the data by country.  
   Note that some of the descriptive information given in 
   the example on page 293 is incorrect.  
   The country with the most respondents is Spain 
   (not the Russian Federation) with 7,745.  
   In the data set as a whole, 23.85% or 23.9% (not 23.8%) 
   of the respondents reported that they attended religious 
   services at least once a week.  
   The country with the lowest proportion of attendance is China 
   (not Denmark) with 0.006. */

tabulate COUNTRY, sort

/* Let's create a pie chart to look at religious attendance
   across all respondents. */

graph pie, over(relatt) plabel(_all percent, size(*1.5) ///
   color(white)) sort(relatt) descending pie(1, color(navy)) ///
   pie(2, color(green)) name(ex17_1_a,replace) ///
   title("Asistencia a servivcios religiososos" "136,611 respuestas en la BD.")

/* Generate a new variable by calculating the percentage 
   of ones (meaning religious attendance at least once a week) 
   for each country subset of the data. Then rereate the 
   histogram is showing how the proportions of religious 
   attendance by country are distributed. */

egen pctra = mean(relatt), by(countryn)
sort pctra
by pctra: gen pctra2 = pctra if _n==1
histogram pctra2, width(0.0001) frequency name(ex17_1_b,replace) ///
   xtitle("Proporcion de asistencia al menos 1 vez a las semana.") ///
   xlabel(0(0.2)1) xsc(r(0 1)) yscale(off) ///
   title("Asistencia a servivcios religiososos por país" "para los 60 países en la BD.") aspect(.07)
/* First, take a look at the income variable.  
   It should be standardized within a country.  
   Compare the box-and-whiskers plot for France, 
   United States, and Nigeria with the same graphic for Turkey. */

graph hbox income if COUNTRY == "France", ///
   title("Distribucion ingresos estadarizados en Francia")
graph hbox income if COUNTRY == "UnitedStates", ///
   title("Distribucion ingresos estadarizados en Estados Unidos")
graph hbox income if COUNTRY == "Mexico", ///
   title("Distribucion ingresos estadarizados en Mexico")
graph hbox income if COUNTRY == "Turkey", ///
   title("Distribucion ingresos estadarizados en Turquía") 
/* Calculate the chi-squared test statistic. */

tabulate countryn relatt, chi2
display _ne(3) " Pearson's Chi-squared test statistic = " ///
    r(chi2) ", p-value: " r(p)
	

/* Calculo de la varianza inter intra, y rho chi2= 2954.3448 */
drop if COUNTRY == "Turkey"
scalar drop _all

scalar chi2stat = r(chi2)
sum relatt
scalar p_hat = r(mean)
bysort countryn : egen nv1 = count(countryn)
by countryn, sort: gen nv2 = nv1*nv1 if _n==1
by countryn, sort: gen nv3 = nv1*pctra*(1-pctra) if _n==1
sum nv2
scalar ssq = r(sum)
sum nv3
scalar nv4 = r(sum)
tab country, nofreq
scalar nco = r(r)
scalar n_tilde = (_N - (ssq/_N))/(nco-1)
drop nv1 nv2 nv3

// between-groups variance
scalar s2b = p_hat*(1-p_hat)*(chi2stat)/(n_tilde*(nco-1))        

// within-groups variance
scalar s2w = nv4/(_N-nco)  

// estimated true variance between the country-dependent proportions
scalar tau_hat_squared = s2b - (s2w/n_tilde)      
display _ne(3) " Estimated variance between the country proportions is " ///
  tau_hat_squared _ne " Estimated between-country standard deviation is " ///
  sqrt(tau_hat_squared)
display "True ICC= " tau_hat_squared/(tau_hat_squared+s2w)*100

/* This completes Example 17.1.  The output above should correspond to the results given in the book. */
/* Click any key to continue. */

scalar list 

*Multilevel logistic regression
*Null model
quietly xtset countryn
xtlogit relatt, intpoints(30)

xtmelogit relatt || countryn:, var
xtmrho

*** Usando xtlogit ***
quietly xtset countryn
xtlogit relatt  educ income unemp female single divorced     ///
        widowed gini educ_c unemp_c divorce_c, intpoints(30)
xtlogit, or

*** Usando xtmelogit ***
xtmelogit relatt  educ income unemp female single divorced   ///
        widowed gini educ_c unemp_c divorce_c || countryn:, intpoints(30)
xtmelogit, or

*** Usando gllamm***
gllamm  relatt  educ income unemp female single divorced     ///
        widowed gini educ_c unemp_c divorce_c,               ///
		i(countryn) link(logit) family(binom) nip(10) adapt  
gllamm, eform
		
