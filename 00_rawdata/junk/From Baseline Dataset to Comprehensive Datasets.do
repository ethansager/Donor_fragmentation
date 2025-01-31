// 1. Read in Baseline Dataset and add CPI, CCI.
use "Baseline Dataset SCI.dta", clear

gen strlenregcode = strlen(GDLcode)
keep if strlenregcode == 4
drop strlenregcode

merge 1:1 iso year using "National External\wgidataset.dta"
drop if _merge == 2
drop _merge
merge 1:1 iso year using "National External\CPI.dta"
drop if _merge == 2
drop _merge

// 2. Setup simple prediction models for World Bank Control of Corruption (CCI) and Transparency International Corruption Perceptions Index (CPI) - Note that CPI sample starts from 2012, as CPI is not comparable over time before 2012.
regress CCI c.SCI2##c.SCI2
predict CCIResidual, residuals
generate CCIConstant = _b[_cons]
generate CCIbSCI = _b[SCI2]
generate CCIbSCIsq = _b[c.SCI2#c.SCI2]

regress CPI c.SCI2##c.SCI2 if year >= 2012
predict CPIResidual if year >= 2012, residuals
generate CPIConstant = _b[_cons]
generate CPIbSCI = _b[SCI2]
generate CPIbSCIsq = _b[c.SCI2#c.SCI2]

generate SUBCCI = CCIConstant + CCIbSCI * SCI2 + CCIbSCIsq * SCI2 * SCI2
replace SUBCCI = SUBCCI + CCIResidual if !missing(CCIResidual)

keep iso year CCIResidual-CPIbSCIsq
save "Temp\National Translation SCI to CCI and CPI.dta", replace

// 3. Create subnational file that includes all years for all subnational areas with missings.
use "Baseline Dataset SCI.dta", clear

duplicates drop GDLcode, force
keep GDLcode iso year region scountry
replace year = 1995
save "Temp\Missing1995.dta", replace

forval i = 1996(1)2022 {
	use "Temp\Missing1995.dta"
	replace year = `i'
	save "Temp\Missing`i'.dta", replace
}

use "Temp\Missing1995.dta", clear

forval i = 1996(1)2022 {
	append using "Temp\Missing`i'.dta"
}

merge 1:1 GDLcode year using "Baseline Dataset SCI.dta"
drop _merge

// 4. Translate SCI values to CCI and CPI scale using the regression results of step 2. For all countries with CCI/CPI values, we apply the regression formula including the national-level residual. For countries without CCI/CPI values, we can only predict the CCI/CPI values based on the SCI.

merge m:1 iso year using "Temp\National Translation SCI to CCI and CPI.dta"
drop _merge

generate SUBCCI = CCIConstant + CCIbSCI * SCI2 + CCIbSCIsq * SCI2 * SCI2
replace SUBCCI = SUBCCI + CCIResidual if !missing(CCIResidual)
generate SUBCPI = CPIConstant + CPIbSCI * SCI2 + CPIbSCIsq * SCI2 * SCI2
replace SUBCPI = SUBCPI + CPIResidual if !missing(CPIResidual)
replace SUBCPI = . if year < 2012

// 5. Now, fill up database entirely based on subnational interpolation and national extrapolation using CCI/CPI.
merge m:1 iso year using "National External\CPI.dta"
drop if _merge == 2
drop _merge sdcpi sdcpiall _merge 
merge m:1 iso year using "National External\wgidataset.dta"
drop if _merge == 2
drop _merge

sort GDLcode year

// Interpolation.
by GDLcode: ipolate SUBCCI year, generate(iSUBCCI)
by GDLcode: ipolate SUBCPI year, generate(iSUBCPI)
by GDLcode: ipolate SCI2 year, generate(iSCI2)
replace SUBCCI = iSUBCCI if missing(SUBCCI)
replace SUBCPI = iSUBCPI if missing(SUBCPI)
replace SCI2 = iSCI2 if missing(SCI2)
drop iSUBCCI iSUBCPI iSCI2

// Extrapolation of subnational values based on national CCI/CPI time-trend.
// Make positive only such that rate variable works (rCCI).

regress SCI2 CCI c.CCI#c.CCI
predict CCI_NationalTrend, xb
regress SCI2 CPI c.CPI#c.CPI if year >= 2012
predict CPI_NationalTrend if year >= 2012, xb

replace CPI = . if year < 2012

quietly summ CCI

by GDLcode: generate rCCI = (CCI[_n] - CCI[_n-1])
generate Change = 1
while Change != 0 {
	quietly summ SUBCCI
	local OldObs = `r(N)'
	by GDLcode: replace SUBCCI = SUBCCI[_n+1] - rCCI[_n+1] if missing(SUBCCI)
	quietly summ SUBCCI
	replace Change = `r(N)' - `OldObs'
}
replace Change = 1
while Change != 0 {
	quietly summ SUBCCI
	local OldObs = `r(N)'
	by GDLcode: replace SUBCCI = SUBCCI[_n-1] + rCCI if missing(SUBCCI)
	quietly summ SUBCCI
	replace Change = `r(N)' - `OldObs'
}

by GDLcode: generate rCCIn = (CCI_NationalTrend[_n] - CCI_NationalTrend[_n-1])
replace Change = 1
while Change != 0 {
	quietly summ SCI2
	local OldObs = `r(N)'
	by GDLcode: replace SCI2 = SCI2[_n+1] - rCCIn[_n+1] if missing(SCI2)
	quietly summ SCI2
	replace Change = `r(N)' - `OldObs'
}
replace Change = 1
while Change != 0 {
	quietly summ SCI2
	local OldObs = `r(N)'
	by GDLcode: replace SCI2 = SCI2[_n-1] + rCCIn if missing(SCI2)
	quietly summ SCI2
	replace Change = `r(N)' - `OldObs'
}

replace Change = 1
by GDLcode: generate rCPI = (CPI[_n] - CPI[_n-1])
while Change != 0 {
	quietly summ SUBCPI if year >= 2012
	local OldObs = `r(N)'
	by GDLcode: replace SUBCPI = SUBCPI[_n+1] - rCPI[_n+1] if missing(SUBCPI) & year >= 2012
	quietly summ SUBCPI if year >= 2012
	replace Change = `r(N)' - `OldObs'
}
replace Change = 1
while Change != 0 {
	quietly summ SUBCPI if year >= 2012
	local OldObs = `r(N)'
	by GDLcode: replace SUBCPI = SUBCPI[_n-1] + rCPI if missing(SUBCPI) & year >= 2012
	quietly summ SUBCPI if year >= 2012
	replace Change = `r(N)' - `OldObs'
}

order scountry iso GDLcode region year SUBCCI CCI SUBCPI CPI SCI2
keep scountry iso GDLcode region year SUBCCI CCI SUBCPI CPI SCI2
rename SCI2 SCI
drop if missing(SUBCCI) & missing(SUBCPI)

// 6. Limit all indices to their minimum and maximum.

replace SCI = 0 if SCI < 0
replace SCI = 100 if SCI > 100 & !missing(SCI)
replace SUBCPI = 0 if SUBCPI < 0
replace SUBCPI = 100 if SUBCPI > 100 & !missing(SUBCPI)
replace SUBCCI = -2.5 if SUBCCI < -2.5
replace SUBCCI = 2.5 if SUBCCI > 2.5 & !missing(SUBCCI)

recast double SUBCCI CCI SUBCPI CPI SCI

replace SUBCCI = round(SUBCCI, 0.001)
replace CCI = round(CCI, 0.001)
replace SUBCPI = round(SUBCPI, 0.1)
replace CPI = round(CPI, 0.1)
replace SCI = round(SCI, 0.1)

generate minyear = year*-1
sort iso minyear GDLcode
drop minyear

preserve

keep scountry iso GDLcode region year SCI

save "Comprehensive Dataset SCI.dta", replace
export excel "Comprehensive Dataset SCI.xlsx", replace firstrow(variables)
savespss "Comprehensive Dataset SCI.sav", replace

restore

keep scountry iso GDLcode region year SUBCCI CCI SUBCPI CPI

label variable CCI "World Bank's Control of Corruption"
label variable CPI "Transparency International's Corruption Perceptions Index"
label variable SUBCCI "Subnational Control of Corruption based on SCI"
label variable SUBCPI "Subnational Corruption Perceptions Index based on SCI"

save "SUBCPI and SUBCCI Dataset.dta", replace
export excel "SUBCPI and SUBCCI Dataset.xlsx", replace firstrow(variables)
savespss "SUBCPI and SUBCCI Dataset.sav", replace

// 


