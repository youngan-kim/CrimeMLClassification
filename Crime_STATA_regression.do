
***********************************************************************************
*Temporal stabilty check: crime cluster as dependent variable * Table 4
***********************************************************************************
forval i = 3/9 {
	use data_k`i', clear
	xtset bgidfp10 year
	destring stcou, replace
	xtlogit crimed_1 lg1_crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou, or
	est store m1_`i'		
	cap log close
}

***********************************************************************************
*Export Table 4
***********************************************************************************
estimates dir 
estout m1_* /// 
using "models_crimecluster_dv.txt", /// 
cells(b(star fmt(4)) se(par fmt(3))) style(tab) starlevels(* 0.05 ** 0.01) /// 
legend varlabels(lg1_crimed_1 "High-Crime Cluster (1/0)" robber "Robbery" assaul "Agg. Assualt"  burglr "Burglary" larcen "Larceny" motveh "M.V. Theft" vioevt "Violent Crime" prpevt "Property Crime" crimecount "Total Crime" ///
fdis "Concentrated Disadvantage" gethhet "Racial Hetero." gasian "% Asian" gblack "% Black" glatino "% Latino" gocc "% Occupied units" gowner "% Homeowners" gage29 "% Aged 15 to 29" ///
proresarea "% Residential" proofficearea "% Office" proretailarea "% Retail" profactryarea "% Industrial" ///
_cons "Intercept") ///
varwidth(50) nolz ///
order (lg1_crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea) ///
stats(N pr2) eform label note(Notes: "b = unstandardized coefficient, (SE) = standard error., borough-fixed effects were included but not shown") replace // 
insheet using "models_crimecluster_dv.txt",clear
export excel using "models_crimecluster_dv.xlsx", sheet("Main") sheetreplace

*********************************************************************************************************************************************************
*********************************************************************************************************************************************************

***********************************************************************************
*Negative Binomial Regression models * Table 5 and Tables A4-6
***********************************************************************************
forval i = 3/9 {
	use data_k`i', clear	
	xtset bgidfp10 year
	destring stcou, replace
	foreach q in vioevt prpevt crimecount {
		xtnbreg `q' crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou, exp(gpop)
		est store m2_`q'_`i'
		reg `q' crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou
		vif
	}
}
***********************************************************************************
*Export Table 5 and Tables A4-6 
***********************************************************************************
estimates dir 
estout m2_* /// 
using "models_w_top60.txt", /// 
cells(b(star fmt(4)) se(par fmt(3))) style(tab) starlevels(* 0.05 ** 0.01) /// 
legend varlabels(crimed_1 "High-Crime Cluster (1/0)" robber "Robbery" assaul "Agg. Assualt"  burglr "Burglary" larcen "Larceny" motveh "M.V. Theft" vioevt "Violent Crime" prpevt "Property Crime" crimecount "Total Crime" ///
fdis "Concentrated Disadvantage" gethhet "Racial Hetero." gasian "% Asian" gblack "% Black" glatino "% Latino" gocc "% Occupied units" gowner "% Homeowners" gage29 "% Aged 15 to 29" ///
proresarea "% Residential" proofficearea "% Office" proretailarea "% Retail" profactryarea "% Industrial" ///
_cons "Intercept") ///
varwidth(50) nolz ///
order (crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea) ///
stats(N pr2) label note(Notes: "b = unstandardized coefficient, (SE) = standard error., borough-fixed effects were included but not shown") replace // 
insheet using "models_w_top60.txt",clear
export excel using "models_w_top60.xlsx", sheet("Main") sheetreplace

*********************************************************************************************************************************************************
*********************************************************************************************************************************************************

**************************************************************************************************************
*Negative Binomial regression models with one-year lagged crime cluster * Table 6 and Tables A5-9 
**************************************************************************************************************
forval i = 3/9 {
	use data_k`i', clear		
	xtset bgidfp10 year
	destring stcou, replace
	foreach q in crimecount vioevt prpevt {
		xtnbreg `q' lg1_crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou, exp(gpop)
		est store m3_`q'_`i'
	}
}
***********************************************************************************
*Export Table Table 6 and A5-9
***********************************************************************************
estimates dir 
estout m3_* /// 
using "models_w_top60_lg1.txt", /// 
cells(b(star fmt(4)) se(par fmt(3))) style(tab) starlevels(* 0.05 ** 0.01) /// 
legend varlabels(lg1_crimed_1 "High-Crime Cluster (1/0)" robber "Robbery" assaul "Agg. Assualt"  burglr "Burglary" larcen "Larceny" motveh "M.V. Theft" vioevt "Violent Crime" prpevt "Property Crime" crimecount "Total Crime" ///
fdis "Concentrated Disadvantage" gethhet "Racial Hetero." gasian "% Asian" gblack "% Black" glatino "% Latino" gocc "% Occupied units" gowner "% Homeowners" gage29 "% Aged 15 to 29" ///
proresarea "% Residential" proofficearea "% Office" proretailarea "% Retail" profactryarea "% Industrial" ///
_cons "Intercept") ///
varwidth(50) nolz ///
order (lg1_crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea) ///
stats(N pr2) label note(Notes: "b = unstandardized coefficient, (SE) = standard error., borough-fixed effects were included but not shown") replace // 
insheet using "models_w_top60_lg1.txt",clear
export excel using "models_w_top60_lg1.xlsx", sheet("Main") sheetreplace
	
*********************************************************************************************************************************************************
*********************************************************************************************************************************************************

***********************************************************************************
*Negative Binomial regression models WITHOUT top60 * Table A10-12 and Table 7
***********************************************************************************
forval i = 3/9 {
	use data_k`i', clear	
	xtset bgidfp10 year
	destring stcou, replace
	foreach q in crimecount vioevt prpevt {
		xtnbreg `q' crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou if top60 == 0, exp(gpop)
		est store m4_`q'_`i'
	}
}
***********************************************************************************
*Export Table for the main effect models WITHOUT top60
***********************************************************************************
estimates dir 
estout m4_* /// 
using "models_wo_top60.txt", /// 
cells(b(star fmt(4)) se(par fmt(3))) style(tab) starlevels(* 0.05 ** 0.01) /// 
legend varlabels(crimed_1 "High-Crime Cluster (1/0)" robber "Robbery" assaul "Agg. Assualt"  burglr "Burglary" larcen "Larceny" motveh "M.V. Theft" vioevt "Violent Crime" prpevt "Property Crime" crimecount "Total Crime" ///
fdis "Concentrated Disadvantage" gethhet "Racial Hetero." gasian "% Asian" gblack "% Black" glatino "% Latino" gocc "% Occupied units" gowner "% Homeowners" gage29 "% Aged 15 to 29" ///
proresarea "% Residential" proofficearea "% Office" proretailarea "% Retail" profactryarea "% Industrial" ///
_cons "Intercept") ///
varwidth(50) nolz ///
order (crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea) ///
stats(N pr2) label note(Notes: "b = unstandardized coefficient, (SE) = standard error., borough-fixed effects were included but not shown") replace // 
insheet using "models_wo_top60_drop_parks.txt",clear
export excel using "models_wo_top60.xlsx", sheet("Main") sheetreplace

*********************************************************************************************************************************************************
*********************************************************************************************************************************************************

***********************************************************************************
*Models for RFNN to Correct the Selection Bias * Tables A13-15
***********************************************************************************
forval i = 3/9 {
	use data_k`i'_psm, clear
	xtset bgidfp10 year
	destring stcou, replace
	foreach q in vioevt prpevt crimecount {
		xtnbreg `q' crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea i.stcou, exp(gpop)
		est store m5_`q'_`i'
	}
}
***********************************************************************************
*Export Tables A13-15
***********************************************************************************
estimates dir 
estout m5_* /// 
using "models_w_top60_rfnn.txt", /// 
cells(b(star fmt(4)) se(par fmt(3))) style(tab) starlevels(* 0.05 ** 0.01) /// 
legend varlabels(crimed_1 "High-Crime Cluster (1/0)" robber "Robbery" assaul "Agg. Assualt"  burglr "Burglary" larcen "Larceny" motveh "M.V. Theft" vioevt "Violent Crime" prpevt "Property Crime" crimecount "Total Crime" ///
fdis "Concentrated Disadvantage" gethhet "Racial Hetero." gasian "% Asian" gblack "% Black" glatino "% Latino" gocc "% Occupied units" gowner "% Homeowners" gage29 "% Aged 15 to 29" ///
proresarea "% Residential" proofficearea "% Office" proretailarea "% Retail" profactryarea "% Industrial" ///
_cons "Intercept") ///
varwidth(50) nolz ///
order (crimed_1 fdis gethhet gasian gblack glatino gocc gowner gage29 proresarea proofficearea proretailarea profactryarea) ///
stats(N pr2) label note(Notes: "b = unstandardized coefficient, (SE) = standard error., borough-fixed effects were included but not shown") replace // 
insheet using "models_w_top60_rfnn.txt",clear
export excel using "models_w_top60_rfnn.xlsx", sheet("Main") sheetreplace


	
