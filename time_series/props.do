clear

use cb2009sc_22_October_2010_nr.dta

keep if COUNTRY == 3

svyset PSU [pweight=WTIND], strata(SUBSTRATUM) fpc(NPSUSS) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(NADHH)

recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup09.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr09.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace



clear


use cb2010sc_04_November_2011_nr.dta

keep if COUNTRY == 3

svyset PSU [pweight=WTIND], strata(SUBSTRATUM) fpc(NPSUSTR) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(HHASIZE)

recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup10.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr10.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


clear


use cb2011sc_07_December_2012_nr.dta

keep if COUNTRY == 3

recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup11.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr11.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode EUSUPP (-9/-3=.)

recode EUSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eusupr)

tabout EUSUPP using "../model/tables/eusup11.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eusupr using "../model/tables/eusupr11.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


clear


use cb2012sc_01_March_2013_or.dta

keep if COUNTRY == 3

svyset PSU [pweight=INDWT], strata(SUBSTRATUM) fpc(NPSUSS) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(NADHH)

recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup12.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr12.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

recode EUSUPP (-9/-3=.)

recode EUSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eusupr)

tabout EUSUPP using "../model/tables/eusup12.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eusupr using "../model/tables/eusupr12.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


clear


use CB_2013_Regional_with_NR_05.02.13.dta

keep if COUNTRY == 3

svyset PSU [pweight=indwt], strata(SUBSTRATUM) fpc(npsuss) singleunit(certainty) || ID, fpc(nhhpsu) || _n, fpc(nadhh)


recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode EUSUPP (-9/-3=.)

recode EUSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eusupr)

tabout EUSUPP using "../model/tables/eusup13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eusupr using "../model/tables/eusupr13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

recode EECSUPP (-9/-3=.)

recode EECSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eecsupr)

tabout EECSUPP using "../model/tables/eecsup13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eecsupr using "../model/tables/eecsupr13.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


clear


use CB_2015_Regional_Only_Responses_140416.dta

keep if COUNTRY == 3

recode NATOSUPP (-9/-3=.)

recode NATOSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout NATOSUPP using "../model/tables/natosup15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode EUSUPP (-9/-3=.)

recode EUSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eusupr)

tabout EUSUPP using "../model/tables/eusup15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eusupr using "../model/tables/eusupr15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode EECSUPP (-9/-3=.)

recode EECSUPP (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eecsupr)

tabout EECSUPP using "../model/tables/eecsup15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eecsupr using "../model/tables/eecsupr15.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

clear


use CB_2017_Georgia_10.11.17.dta

// keep if COUNTRY == 3

recode p22 (-9/-3=.)

recode p22 (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(natosupr)

tabout p22 using "../model/tables/natosup17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout natosupr using "../model/tables/natosupr17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode p25 (-9/-3=.)

recode p25 (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eusupr)

tabout p25 using "../model/tables/eusup17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eusupr using "../model/tables/eusupr17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


recode p28_geo (-9/-3=.)

recode p28_geo (-2/-1=-1 "DK/RA")  (1/2=1 "Do not support") (3=2 "In the middle") (4/5=3 "Support"), gen(eecsupr)

tabout p28_geo using "../model/tables/eecsup17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout eecsupr using "../model/tables/eecsupr17.csv", svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

.
