global path "D:\ftr\data"
cd "$path"

**** Exchange rate
clear all
use exchangerate.dta, clear
gen dataend = mdy(month(pubdats)+1,1,year(pubdats))-1
replace dataend = mdy(12,31,year(pubdats)) if month(pubdats)==12
rename curr curcd
sencode curcd, replace
duplicates drop curcd dataend, force
rdecode curcd, replace
save exrate.dta, replace

****North America
clear all
use ncompustat.dta, clear
destring gvkey sic, replace
replace cusip = substr(cusip,1,8)
duplicates drop gvkey fyear, force

gen dataend = mdy(month(datadate)+1,1,year(datadate))-1
replace dataend = mdy(12,31,year(datadate)) if month(datadate)==12

merge m:1 dataend curcd using exrate.dta
keep if _merge==3
drop _merge

tsset gvkey fyear
replace xidoc = 0 if xidoc ==.
gen taas =(ibc-oancf+xidoc)/L.at
gen reas = 1/L.at
gen dsaas =(D.sale-D.rect)/L.at
gen ppeas = ppegt/L.at

gen pras =(cogs+D.invt)/L.at
gen saas = sale/L.at
gen dsa1as = D.L.sale/L.at

gen cfas = (oancf-xidoc)/L.at

gen sa1as = L.sale/L.at
replace xad = 0 if xad ==.
replace xrd = 0 if xrd ==.
gen disxas = (xad+xrd+xsga)/L.at

gen IOA = ni/L.at
keep gvkey cusip fyear datadate at ceq mkvalt dlc dltt ib ni sstk IOA taas reas dsaas ppeas pras saas dsa1as cfas sa1as disxas sic fic loc curcd exchg exrat
keep if loc=="USA"|loc=="CAN"
save ncomp.dta, replace

****Global shareprice
clear all
use gshareprice.dta, clear
gen dataend = mdy(month(datadate)+1,1,year(datadate))-1
replace dataend = mdy(12,31,year(datadate)) if month(datadate)==12
destring gvkey, replace
duplicates drop gvkey dataend, force
save gshprice.dta, replace

****Global
clear all
use gcompustat.dta, clear
destring gvkey sic, replace
gen cusip = substr(sedol,1,6)

gen dataend = mdy(month(datadate)+1,1,year(datadate))-1
replace dataend = mdy(12,31,year(datadate)) if month(datadate)==12

merge m:1 gvkey dataend using gshprice, keepusing(prccd curcdd)
keep if _merge==3
drop _merge
duplicates drop gvkey fyear, force

merge m:1 dataend curcd using exrate.dta
keep if _merge==3
drop _merge

tsset gvkey fyear
replace xidoc = 0 if xidoc ==.
gen taas =(ibc-oancf+xidoc)/L.at
gen reas = 1/L.at
gen dsaas =(D.sale-D.rect)/L.at
gen ppeas = ppegt/L.at

gen pras =(cogs+D.invt)/L.at
gen saas = sale/L.at
gen dsa1as = D.L.sale/L.at

gen cfas = (oancf-xidoc)/L.at

gen sa1as = L.sale/L.at
replace xrd = 0 if xrd ==.
gen disxas = (xrd+xsga)/L.at

gen IOA = ni/L.at
gen mkvalt = cshoi*prccd
keep gvkey cusip fyear datadate at ceq mkvalt dlc dltt ib ni sstk IOA taas reas dsaas ppeas pras saas dsa1as cfas sa1as disxas sic fic loc curcd exchg exrat
save gcomp.dta, replace

**** Combined financial variables
clear all
use ncomp.dta, clear
append using gcomp.dta
duplicates drop gvkey fyear, force
compress

local vem "taas reas dsaas ppeas pras saas dsa1as cfas sa1as disxas"
/*
foreach v of varlist `vem' {
	winsor2 `v', replace cuts(5 95) by(fyear)
}
*/
drop if (sic>=6000 & sic<=6900)
gen indus = int(sic/1000)
egen g = group(indus loc fyear)
qui sum g
global Ng = r(max)

dropvars gobs
bysort g: egen gobs=count(gvkey)
drop if gobs<8

***Acrrual-based earnings management
tsset gvkey fyear
gen aba = .
forvalues i = 1/$Ng {
cap qui reg taas reas dsaas ppeas if g==`i'
cap qui predict aba1 if e(sample), res
cap qui replace aba = aba1 if e(sample)
cap drop aba1
}

***Real earnings management
**prod
gen  abpr = .
forvalues i = 1/$Ng{
cap quietly reg pras reas saas dsaas dsa1as if g==`i'
cap qui predict abpr1 if g==`i', res
cap qui replace abpr = abpr1 if g==`i'
cap drop abpr1
}

**cfo
gen abcf = .
forvalues i = 1/$Ng{
cap quietly reg cfas reas saas dsaas if g==`i'
cap qui predict abcf1 if g==`i', res
cap qui replace abcf = abcf1 if g==`i'
cap drop abcf1
}

**disx
gen abdisx = .
forvalues i = 1/$Ng{
cap quietly reg disxas reas sa1as if g==`i'
cap qui predict abdisx1 if g==`i', res
cap qui replace abdisx = abdisx1 if g==`i'
cap drop abdisx1
}

forvalues i = 1/$Ng {
quiet{
preserve
keep if g==`i'
drop if IOA==.
drop if aba==.
sort IOA
gen TDIOA= abs(IOA[_n]-IOA[_n-1])
gen BDIOA= abs(IOA[_n]-IOA[_n+1])
egen MINDIOA=rowmin(TDIOA BDIOA)
gen AM=aba-aba[_n-1] if MINDIOA==TDIOA
replace AM=aba-aba[_n+1] if MINDIOA==BDIOA
tempfile wcompustat`i'
save "`wcompustat`i''", replace
restore
}
}

use "`wcompustat1'", clear
forvalues i=2/$Ng {
quiet{
append using "`wcompustat`i''"
}
}
gen RM = abpr-abcf-abdisx
save wcompustat.dta, replace

**** Earnings suprise 
use ibesidentification.dta, clear
duplicates drop ticker, force
save ibesid1.dta, replace
use ibessurprise.dta, clear
merge m:1 ticker using ibesid1.dta
keep if _merge==3
drop _merge
gen cusip1 = substr(cusip,-6,6)
replace cusip1 = substr(cusip,1,8) if(country=="NA")|(country=="NC")|(usfirm>0.5)
drop cusip
rename cusip1 cusip
rename pyear fyear
duplicates drop cusip fyear, force
save wsurprise.dta, replace

use wcompustat.dta, clear
duplicates drop cusip fyear, force
merge 1:1 cusip fyear using wsurprise.dta
keep if _merge==3
drop _merge
save firmlevel.dta, replace

**** country-level variables
insheet using "$path\langftr.csv", clear
cap drop if loc ==""
pca strongftr verbr sentencer, comp(1)
estat loadings
predict pcftr
keep loc strongftr verbr sentencer pcftr
rename verbr vr
rename sentencer sr
save languageftr.dta, replace

insheet using "$path\lawculture.csv", clear
drop if loc ==""
rename d08antid invpro
rename d08pubenf pubenf
rename d08smc smi
keep loc countryname invpro pubenf smi pd indiv mas ua lto indul trust* careful* protestant* catholic* yesrelig* norelig*

gen trust=.
gen careful=.
gen protestant=.
gen catholic=.
gen yesrelig=.
gen norelig=.
local vcul "trust careful protestant catholic yesrelig norelig"
foreach v of varlist `vcul'{
gen `v'1981=`v'1
gen `v'1982=`v'1
gen `v'1983=`v'1
gen `v'1984=`v'1

gen `v'1990=`v'2
gen `v'1991=`v'2
gen `v'1992=`v'2
gen `v'1993=`v'2
gen `v'1994=`v'2

gen `v'1995=`v'3
gen `v'1996=`v'3
gen `v'1997=`v'3
gen `v'1998=`v'3
gen `v'1999=`v'3

gen `v'2000=`v'4
gen `v'2001=`v'4
gen `v'2002=`v'4
gen `v'2003=`v'4
gen `v'2004=`v'4

gen `v'2005=`v'5
gen `v'2006=`v'5
gen `v'2007=`v'5
gen `v'2008=`v'5
gen `v'2009=`v'5

gen `v'2010=`v'6
gen `v'2011=`v'6
gen `v'2012=`v'6
gen `v'2013=`v'6
gen `v'2014=`v'6
}
foreach v of varlist `vcul'{
dropvars `v' `v'1 `v'2 `v'3 `v'4 `v'5 `v'6
}
save lawculture.dta, replace

insheet using "$path\countryecon.csv", clear
keep loc ggr* mcap* inf*
drop if loc ==""
save countryecon.dta, replace

use languageftr.dta, clear
merge 1:1 loc using lawculture.dta
keep if _merge==3
drop _merge
merge 1:1 loc using countryecon.dta
keep if _merge==3
drop _merge

keep loc countryname strongftr vr sr pcftr invpro pubenf smi pd indiv mas ua lto indul trust* careful* protestant* catholic* yesrelig* norelig* ggr* mcap* inf*
reshape long trust careful protestant catholic yesrelig norelig ggr mcap inf, i(loc) j(fyear)
gen trustr=trust/(trust+careful)
gen religc= protestant+catholic
gen religd= yesrelig/(yesrelig+norelig)

bysort loc: egen avtrust=mean(trustr)
replace trustr=avtrust
bysort loc: egen avreligc=mean(religc)
replace religc=avreligc
bysort loc: egen avreligd=mean(religd)
replace religd=avreligd

/*
xtile trustrH= trustr, nq(2)
xtile religcH= religc, nq(2)
xtile religdH= religd, nq(2)
*/

save countrylevel.dta, replace

**** Merge firm-level with country-level
cd "$path"
clear all
use firmlevel.dta, clear
merge m:1 loc fyear using countrylevel.dta
keep if _merge==3
drop _merge

tsset gvkey fyear
gen SIZE = ln(at/exrat/(1+inf))
gen BTM = ceq/(mkvalt)
gen LEV = (dlc+dltt)/at
gen ROA = ib/at
gen LOSS = (ni < 0)
gen ISSUE = (sstk > 0)

local vctn "AM RM SIZE BTM LEV ROA"
foreach v of varlist `vctn'{
winsor2 `v', replace cuts(1 99) by(fyear)
}

gen AAM = abs(AM)
gen ARM = abs(RM)
gen SURP=actual-surpmean
gen MEET =(SURP>=0)
gen LA=0 if (actual<0)|(actual>0.01&actual!=.)
replace LA=1 if(actual>=0&actual<=0.01)
gen SPES=0 if (SURP<0)|(SURP>0.01&SURP!=.)
replace SPES=(SURP>=0&SURP<=0.01)

gen sftrinv = strongftr*invpro
gen srinv = sr*invpro
gen vrinv = vr*invpro
gen pftrinv = pcftr*invpro

drop if (fyear<=1993|fyear>=2014)
save ftrdataset.dta, replace

**** summary statistics
cd "D:\ftr"
use "$path\ftrdataset.dta", clear
global v1 "AAM ARM LA SPES"
global vv1 "strongftr"
global vv2 "sr"
global vv3 "vr"
global vv4 "pcftr"
global vv5 "strongftr sftrinv"
global vv6 "sr srinv"
global vv7 "vr vrinv"
global vv8 "pcftr pftrinv"

global vftr "strongftr sr vr pcftr"
global v2 "invpro pd indiv mas ua lto indul ggr SIZE BTM LEV ROA ISSUE MEET LOSS"
global v3 "invpro pd indiv mas ua lto indul ggr SIZE BTM LEV ROA ISSUE MEET"
global v4 "invpro pd indiv mas ua lto indul ggr SIZE BTM LEV ROA ISSUE LOSS"
global v5 "invpro pd indiv mas ua lto indul ggr SIZE BTM LEV ROA ISSUE"
global v6 "invpro pd indiv mas ua lto indul ggr"
global v $vv4 $v2
global vv $vv8 $v2

dropvars miss
egen miss = rmiss($vftr $v1 $v2)
drop if miss>0

dropvars gobs
bysort g: egen gobs=count(gvkey)
drop if gobs<8

logout, save(Descriptive) excel replace: ///
tabstat $v1 $vftr $v2, s(N mean sd p25 p50 p75) /// 
f(%12.6f) c(s)

preserve
sort loc
egen gc = group(gvkey fyear)
by loc: egen cno=count(gc) 
logout, save(countrysum) excel replace: tabstat cno $vftr, by(countryname)
restore

preserve
tabstat $v1, s(mean p50) c(s) f(%10.4f) by(strongftr)
foreach vd of varlist $v1{
ttest `vd', by($vv1)
ranksum `vd', by($vv1)
}
/*
egen psr = pctile(sr), p(50)
gen DSR=1 if sr > psr
replace DSR=0 if sr <= psr

tabstat $v1, s(mean p50) c(s) f(%10.4f) by(DSR)
foreach vd of varlist $v1{
ttest `vd', by(DSR)
ranksum `vd', by(DSR)

egen pvr = pctile(vr), p(50)
gen DVR=1 if vr > pvr
replace DVR=0 if vr <= pvr

tabstat $v1, s(mean p50) c(s) f(%10.4f) by(DVR)
foreach vd of varlist $v1{
ttest `vd', by(DVR)
ranksum `vd', by(DVR)

egen ppcftr = pctile(pcftr), p(50)
gen DPCFTR=1 if pcftr > ppcftr
replace DPCFTR=0 if pcftr <= pcftr

tabstat $v1, s(mean p50) c(s) f(%10.4f) by(DPCFTR)
foreach vd of varlist $v1{
ttest `vd', by(DPCFTR)
ranksum `vd', by(DPCFTR)
*/
restore

**** pairwise correlation
format $v1 $vftr $v2 %6.3f
logout, save(Correlation) excel replace: ///
pwcorr_a $v1 $vftr $v2

**** regression
dropvars yr_dum*
qui tab fyear, gen(yr_dum)
dropvars indus_dum*
qui tab indus, gen(indus_dum)

*** AAM & ARM
preserve
reg AAM $vv1 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg AAM $vv2 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
reg AAM $vv3 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
reg AAM $vv4 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m4

reg ARM $vv1 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv2 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
reg ARM $vv3 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
reg ARM $vv4 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8

local s "using reg1.csv"
local m "m1 m2 m3 m4 m5 m6 m7 m8"
local t "H1-OLS AAM & ARM"
esttab `m' `s', title(`"`t'"') b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** LA & SPES
preserve
logit LA $vv1 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m1
logit LA $vv2 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m2
logit LA $vv3 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m3
logit LA $vv4 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m4

logit SPES $vv1 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m5
logit SPES $vv2 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m6
logit SPES $vv3 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m7
logit SPES $vv4 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m8

local s "using reg2.csv"
local m "m1 m2 m3 m4 m5 m6 m7 m8"
local t "H1-OLS LA & SPES"
esttab `m' `s', title(`"`t'"') b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Interaction between FTR and Investor Protection: AAM & ARM
preserve
reg AAM $vv5 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg AAM $vv6 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
reg AAM $vv7 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
reg AAM $vv8 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m4

reg ARM $vv5 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv6 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
reg ARM $vv7 $v2 yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
reg ARM $vv8 $v2 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8

local s "using reg3.csv"
local m "m1 m2 m3 m4 m5 m6 m7 m8"
local t "H2-Interaction between FTR and Investor Protection: AAM & ARM"
esttab `m' `s', title(`"`t'"') b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Interaction between FTR and Investor Protection: LA & SPES

preserve
logit LA $vv5 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m1
logit LA $vv6 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m2
logit LA $vv7 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m3
logit LA $vv8 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m4

logit SPES $vv5 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m5
logit SPES $vv6 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m6
logit SPES $vv7 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m7
logit SPES $vv8 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
*margins, dydx(*) atmeans
est store m8

local s "using reg4.csv"
local m "m1 m2 m3 m4 m5 m6 m7 m8"
local t "H2-Interaction between FTR and Investor Protection: LA & SPES"
esttab `m' `s', title(`"`t'"') b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore


*** Propensity Score Matching
preserve
cap safedrop u
generate u=uniform()
sort u
psmatch2 $vv1 $v2 yr_dum* indus_dum*, out(AAM) n(1) norepl logit
reg AAM $v yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
est store m1
psmatch2 $vv1 $v2 yr_dum* indus_dum*, out(ARM) n(1) norepl logit 
reg ARM $v yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support 
est store m2
psmatch2 $vv1 $v3 yr_dum* indus_dum*, out(LA) n(1) norepl logit 
logit LA $vv4 $v3 yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
*logit LA _treated
est store m3
psmatch2 $vv1 $v4 yr_dum* indus_dum*, out(SPES) n(1) norepl logit
logit SPES $vv4 $v4 yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
*logit SPES _treated
est store m4

psmatch2 $vv1 $v2 yr_dum* indus_dum*, out(AM) n(1) norepl logit
reg AAM $vv yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
est store m5
psmatch2 $vv1 $v2 yr_dum* indus_dum*, out(RM) n(1) norepl logit
reg ARM $vv yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
est store m6
psmatch2 $vv1 $v3 yr_dum* indus_dum*, out(LA) n(1) norepl logit
logit LA $vv8 $v3 yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
est store m7
psmatch2 $vv1 $v4 yr_dum* indus_dum*, out(SPES) n(1) norepl logit
logit SPES $vv8 $v4 yr_dum* indus_dum* if _support==1, vce(cluster gvkey)
drop _support
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg5.csv, title("PSM") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Additional Country-level controls
preserve
global vac "trustr religc pubenf"
reg AAM $v $vac yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg ARM $v $vac yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
logit LA $vv4 $v3 $vac yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
logit SPES $vv4 $v4 $vac yr_dum* indus_dum*, vce(cluster gvkey)
est store m4
reg AAM $vv $vac yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv $vac yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
logit LA $vv8 $v3 $vac yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
logit SPES $vv8 $v4 $vac yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg6.csv, title("Additional Country-level controls") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Dropping US
preserve
drop if inlist(loc, "USA")
reg AAM $v yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg ARM $v yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
logit LA $vv4 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
logit SPES $vv4 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
est store m4
reg AAM $vv yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
reg LA $vv8 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
reg SPES $vv8 $v4 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg7.csv, title("Dropping US") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Dropping BEL & CHE
/*preserve
tempfile languageftr1
use languageftr.dta, clear
replace strongftr=1 if inlist(loc, "BEL", "CHE")
replace verbr=verbr2 if inlist(loc, "BEL", "CHE")
replace sentencer=sentencer2 if inlist(loc, "BEL", "CHE")
pca strongftr verbr sentencer, comp(1)
estat loadings
predict pcftr1, score
save "`languageftr1'"
restore*/

preserve
/*merge m:1 loc using "`languageftr1'", keepusing(pcftr1)
drop _merge*/
*replace pcftr=pcftr1
drop if inlist(loc, "BEL", "CHE", "NLD", "HKG", "SGP")
reg AAM $v yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg ARM $v yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
logit LA $vv4 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
logit SPES $vv4 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
est store m4
reg AAM $vv yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
reg LA $vv8 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
reg SPES $vv8 $v4 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg8.csv, title("Dropping BEL & CHE") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

*** Country-year analysis
preserve
dropvars yr_dum*
qui tab fyear, gen(yr_dum)
collapse (median) yr_dum* AAM ARM $vv5 $vv6 $vv7 $vv8 $v2 (sum) LA SPES, by(loc fyear)
bysort fyear: egen pla = pctile(LA), p(50)
gen DLA=1 if LA > pla
replace DLA=0 if LA <= pla
bysort fyear: egen pspes = pctile(SPES), p(50)
gen DSPES=1 if SPES > pspes
replace DSPES=0 if SPES <= pspes

reg AAM $vv1 $v6 yr_dum*, robust
est store m1
reg AAM $vv2 $v6 yr_dum*, robust
est store m2
reg AAM $vv3 $v6 yr_dum*, robust
est store m3
reg AAM $vv4 $v6 yr_dum*, robust
est store m4
reg AAM $vv5 $v6 yr_dum*, robust
est store m5
reg AAM $vv6 $v6 yr_dum*, robust
est store m6
reg AAM $vv7 $v6 yr_dum*, robust
est store m7
reg AAM $vv8 $v6 yr_dum*, robust
est store m8

esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg9.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)


reg ARM $vv1 $v6 yr_dum*, robust
est store m1
reg ARM $vv2 $v6 yr_dum*, robust
est store m2
reg ARM $vv3 $v6 yr_dum*, robust
est store m3
reg ARM $vv4 $v6 yr_dum*, robust
est store m4
reg ARM $vv5 $v6 yr_dum*, robust
est store m5
reg ARM $vv6 $v6 yr_dum*, robust
est store m6
reg ARM $vv7 $v6 yr_dum*, robust
est store m7
reg ARM $vv8 $v6 yr_dum*, robust
est store m8

esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg10.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)

logit DLA $vv1 $v6 yr_dum*, robust
est store m1
logit DLA $vv2 $v6 yr_dum*, robust
est store m2
logit DLA $vv3 $v6 yr_dum*, robust
est store m3
logit DLA $vv4 $v6 yr_dum*, robust
est store m4
logit DLA $vv5 $v6 yr_dum*, robust
est store m5
logit DLA $vv6 $v6 yr_dum*, robust
est store m6
logit DLA $vv7 $v6 yr_dum*, robust
est store m7
logit DLA $vv8 $v6 yr_dum*, robust
est store m8


esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg11.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)

logit DSPES $vv1 $v6 yr_dum*, robust
est store m1
logit DSPES $vv2 $v6 yr_dum*, robust
est store m2
logit DSPES $vv3 $v6 yr_dum*, robust
est store m3
logit DSPES $vv4 $v6 yr_dum*, robust
est store m4
logit DSPES $vv5 $v6 yr_dum*, robust
est store m5
logit DSPES $vv6 $v6 yr_dum*, robust
est store m6
logit DSPES $vv7 $v6 yr_dum*, robust
est store m7
logit DSPES $vv8 $v6 yr_dum*, robust
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg12.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)
restore

/*
reg AAM $vv1 $v6 yr_dum*, robust
est store m1
reg ARM $vv1 $v6 yr_dum*, robust
est store m2
logit DLA $vv1 $v6 yr_dum*, robust
est store m3
logit DSPES $vv1 $v6 yr_dum*, robust
est store m4
reg AAM $vv5 $v6 yr_dum*, robust
est store m5
reg ARM $vv5 $v6 yr_dum*, robust
est store m6
logit DLA $vv5 $v6 yr_dum*, robust
est store m7
logit DSPES $vv5 $v6 yr_dum*, robust
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg13.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)

reg AAM $vv2 $v6 yr_dum*, robust
est store m1
reg ARM $vv2 $v6 yr_dum*, robust
est store m2
logit DLA $vv2 $v6 yr_dum*, robust
est store m3
logit DSPES $vv2 $v6 yr_dum*, robust
est store m4
reg AAM $vv6 $v6 yr_dum*, robust
est store m5
reg ARM $vv6 $v6 yr_dum*, robust
est store m6
logit DLA $vv6 $v6 yr_dum*, robust
est store m7
logit DSPES $vv6 $v6 yr_dum*, robust
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg14.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)

reg AAM $vv3 $v6 yr_dum*, robust
est store m1
reg ARM $vv3 $v6 yr_dum*, robust
est store m2
logit DLA $vv3 $v6 yr_dum*, robust
est store m3
logit DSPES $vv3 $v6 yr_dum*, robust
est store m4
reg AAM $vv7 $v6 yr_dum*, robust
est store m5
reg ARM $vv7 $v6 yr_dum*, robust
est store m6
logit DLA $vv7 $v6 yr_dum*, robust
est store m7
logit DSPES $vv7 $v6 yr_dum*, robust
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg11.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)

reg AAM $vv4 $v6 yr_dum*, robust
est store m1
reg ARM $vv4 $v6 yr_dum*, robust
est store m2
logit DLA $vv4 $v6 yr_dum*, robust
est store m3
logit DSPES $vv4 $v6 yr_dum*, robust
est store m4
reg AAM $vv8 $v6 yr_dum*, robust
est store m5
reg ARM $vv8 $v6 yr_dum*, robust
est store m6
logit DLA $vv8 $v6 yr_dum*, robust
est store m7
logit DSPES $vv8 $v6 yr_dum*, robust
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg12.csv, title("Country-Year Regression") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum*)
*/

*** dropping China & Japan
preserve
drop if inlist(loc, "CHN", "JPN")
reg AAM $v yr_dum* indus_dum*, vce(cluster gvkey)
est store m1
reg ARM $v yr_dum* indus_dum*, vce(cluster gvkey) 
est store m2
logit LA $vv4 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m3
logit SPES $vv4 $v4 yr_dum* indus_dum*, vce(cluster gvkey)
est store m4
reg AAM $vv yr_dum* indus_dum*, vce(cluster gvkey)
est store m5
reg ARM $vv yr_dum* indus_dum*, vce(cluster gvkey) 
est store m6
reg LA $vv8 $v3 yr_dum* indus_dum*, vce(cluster gvkey)
est store m7
reg SPES $vv8 $v4 yr_dum* indus_dum*, vce(cluster gvkey) 
est store m8
esttab m1 m2 m3 m4 m5 m6 m7 m8 using reg15.csv, title("Dropping China and Japan") b(%6.3f) replace ///
compress nogaps scalar(N r2_a r2_p) ///
star(* 0.1 ** 0.05 *** 0.01) drop(yr_dum* indus_dum*)
restore

gen family=.
replace family=1 if loc=="AUS" | loc=="BEL" | loc=="BRZ"| loc=="DNK" | loc=="FIN" | loc=="DEU" | loc=="FIN" | loc=="NLD" | loc=="NOR" | loc=="SWE" | loc=="AUT" | loc=="CAN" | loc=="FRA" | loc=="GRC" | loc=="IND" | loc=="IRL" | loc=="ITA" | loc=="MEX" | loc=="PAK" | loc=="POL" | loc=="RUS" | loc=="ESP" | loc=="GBR" | loc=="USA"
replace family=2 if loc=="CHN" | loc=="HKG" | loc=="TWN" | loc=="CHI" | loc=="NZL" | loc=="PHL" | loc=="SGP"
replace family=3 if loc=="JPN"
replace family=4 if loc=="EGY" | loc=="NIG" | loc=="ZAF"

ivregress 2sls AAM $v6 yr_dum1 - yr_dum20  (strongftr = family)

gen continent=.
replace continent=1 if loc=="BEL" | loc=="DNK" | loc=="FIN" | loc=="DEU" | loc=="NLD" | loc=="NOR" | loc=="SWE" | loc=="AUT" | loc=="FRA" | loc=="GRC" | loc=="IND" | loc=="ITA" | loc=="POL" | loc=="RUS" | loc=="ESP" | loc=="GBR"  |  loc=="IRL" 
replace continent=2 if loc=="CHN" | loc=="HKG" | loc=="TWN" | loc=="PHL" | loc=="SGP" | loc=="JPN" | loc=="PAK" 
replace continent=3 if loc=="AUS" | loc=="NZL" 
replace continent=4 if loc=="EGY" | loc=="NIG" | loc=="ZAF"                      
replace continent=5 if loc=="CHI" |	loc=="BRZ" | loc=="MEX"                                                                                  
replace continent=6 if loc=="CAN" | loc=="USA"                                                                 

ivregress 2sls AAM $vv2 $v6 yr_dum*  (strongftr = continent)							                       
ivregress 2sls ARM $vv2 $v6 yr_dum*  (strongftr = continent)									   
