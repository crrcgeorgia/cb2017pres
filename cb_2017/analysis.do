clear

use "CB_2017_Georgia_10.11.17.dta"

mvdecode p22 p25 p28 rand1 rand2 rand3 rand4 p23 p24 p26 p27 p29 p30 p31 p32, mv(-9=.\-3=.\-7=.)

set more off

svyset


/// Slide 1: Proportions for p22, p25, p28

foreach x in p22 p25 p28 {
recode `x' (-2/-1=-1)
lab def `x' -1 "DK/RA", modify
}


recode p1 (-9/-3=.) (-2/-1=3) (1/2=1) (3=2) (4/5=3)
lab def p1 1 "Wrong" 2 "Does not change at all" 3 "Right", modify

recode p2 (-9/-3=.) (-2/-1=3) (1/2=1) (3/4=2) (5=3)
lab def p2 1 "Situation will never improve" 2 "Everything will be fine" 3 "Agree with neither", modify

recode p12 (-9/-3=.) (-2/-1=3)  (1/2=1) (3/4=2)
lab def p12 1 "Disagree" 2 "Agree" 3 "Neither", modify

recode p13 (-9/-3=.) (-3/-1=3) (1/2=1) (3/4=2) (5=3)
lab def p13 1 "People are like children" 2 "Government is an employee" 3 "Agree with neither", modify

recode m2 (1/2=1) (3=0) (-2/-1=0) (-3=.)
lab def m2 1 "Yes" 0 "No", modify


tabout p22 using tables/p22.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p25 using tables/p25.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p28 using tables/p28.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

/// Slide 2: Time series plot for p22, p25, p28

/// Slide 3: Proportions for rand3 and rand4
foreach x in rand1 rand2 rand3 rand4 {
recode `x' (-2/-1=-1)
lab def `x' -1 "DK/RA", modify
}


tabout rand1 using tables/rand1.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout rand2 using tables/rand2.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout rand3 using tables/rand3.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout rand4 using tables/rand4.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

/// Slide 4: Proportions for p23 and p24

foreach x in p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 {
recode `x' (-2/-1=-1)
lab def `x' -1 "DK/RA", modify
}

tabout p23 using tables/p23.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p24 using tables/p24.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

/// Slide 5: Proportions for p26 and p27

tabout p26 using tables/p26.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p27 using tables/p27.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

/// Slide 6: Proportions for p29 and p30

tabout p29 using tables/p29.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p30 using tables/p30.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

/// Slide 7: Proportions for p31 and p32

tabout p31 using tables/p31.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace

tabout p32 using tables/p32.csv, svy cells(nofreq lb ub) format(4 4 4) clab(Frequency Lower_Bound Upper_Bound) ptotal(none) replace


/// Recode and create variables for the model

foreach x in p22 p25 p28 {
recode `x' (1/2=1 "Do not support") (3=2 "Neutral") (4/5=3 "Support") (-2/-1=2), gen(`x'r)
}

foreach x in rand3 rand4 {
recode `x' (-2/-1=5)
}

lab def rand3 5 "DK/RA", modify
lab def rand4 5 "DK/RA", modify

/// How consistent are people

gen consistent = 0

//// supports EU Supports EEU:
//// replace consistent = 1 if (p25r == 3 & p28r ==3)
//// Supports EU, neutral EEU
//// there was Eu don't support
//// support EEU
//// EU support, don't support EEU
//// Support both
//// DK/RA both
//// and then combinations of DK/RA and either EU or EEU
//// which was relatively small

recode vl1 (-5/-4=1 "Know someone who traveled") (-2/-1= 2 "DK/RA") (0=0 "Don't know anyone who traveled") (1/100=1), gen(visalib)

recode d6 (-2/-1=5 "DK/RA") (1=1 "No foreign language") (2=2 "English") (3=3 "Russian") (4=4 "Other"), gen(mandl)

recode p21 (-2/-1=3 "DK/RA") (1=1 "Democracy is preferable") (2=2 "Sometimes non-democracy preferable") (3=3 "Doesn't matter"), gen(demat)

// m1

mvdecode n14_01 n14_02 n14_03, mv(-9=.\-3=.\-7=.)

egen indx = rowtotal(n14_01 n14_02 n14_03)

xtile soccon=indx, nq(5)

drop indx

mvdecode n6_01 n6_02 n6_03 n6_04 n6_05 n6_06 n6_07 n6_08 n6_09 n6_10 n6_11 n6_12 n6_14 n6_15 n6_16 n6_17, mv(-9=.\-3=.\-7=.)

egen indx = rowtotal(n6_01 n6_02 n6_03 n6_04 n6_05 n6_06 n6_07 n6_08 n6_09 n6_10 n6_11 n6_12 n6_14 n6_15 n6_16 n6_17)

xtile marriage=indx, nq(5)
drop indx 

recode d3 (1/4=1 "Secondary or lower") (5=2 "Vocational") (6/8=3 "Higher"), gen(edu)

gen parhedu = 0

replace parhedu = 1 if (d4 == 6 | d4 == 7 | d4 == 8) | (d5 == 6 | d5 == 7 | d5 == 8) 


mvdecode c2_1-c3_10, mv(-9=.\-3=.\-7=.)

egen indx = rowtotal(c2_1 c2_2 c2_3 c2_4 c2_5 c2_6 c2_8 c2_10)
xtile ses=indx, nq(5)
drop indx 

recode c6 (6/8=1 "<$100") (5=2 "$101-250") (4=3 "$251-400") (1/3=4 ">$400") (-2/-1=5 "DK/RA"), gen(hhincome)

recode c7 (6/8=1 "<$100") (5=2 "$101-250") (4=3 "$251-400") (1/3=4 ">$400") (-2/-1=5 "DK/RA"), gen(hhexpnd)

recode age (18/35=1 "18-35") (36/55=2 "36-55") (56/119=3 "56+"), gen(agegroup)

gen settype = stratum

recode d1 (3=1 "Georgian") (else=0 "Other"), gen(ethn)

recode m1 (-1=2) (1/2=1) (4/5=3)
lab def m1 1 "Bad" 2 "Neutral" 3 "Good", modify


recode d13 (1/4=1 "Yes") (5/7=0 "No"), gen(d13r)

recode d8 (-2/-1=.)
recode d13r (-2/-1=.)
recode p17 (-2/-1=.)
recode d8 (1/3=1) (4/6=0), gen(internet)
mvdecode d16 p22r p25r p28r consistent visalib mandl demat soccon marriage edu parhedu ses hhincome hhexpnd agegroup settype ethn q1_01 q1_02 q1_03 q1_04 q1_05 q1_06 q1_07 q1_08 q1_09 q1_10 q1_11 n1 n2 n3 n4 n5_01 n5_02 n5_03 n5_04 n5_05 n5_06 n5_07 n5_08 n5_09 n5_10 n5_11 n5_12 n5_14 n5_15 n5_16 n5_17 n6_01 n6_02 n6_03 n6_04 n6_05 n6_06 n6_07 n6_08 n6_09 n6_10 n6_11 n6_12 n6_14 n6_15 n6_16 n6_17 p4_01 p4_02 p4_03 p4_04 p4_05 p4_06 p4_07 p4_08 p4_09 p4_10 p4_11 p4_12 p4_13 p4_14 p4_15 p4_16 p4_17 m1 d8 d16 d13r p17, mv(-9=.\-3=.\-7=.\-5=.)

set more off


foreach x in p22r p25r p28r {
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn, vce(cluster psu)
margins, at(edu=(1 2 3)) atmeans saving (margins/`x'_edu, replace)
margins, at(parhedu=(0 1)) atmeans saving (margins/`x'_parhedu, replace)
margins, at(ses=(1 2 3 4 5)) atmeans saving (margins/`x'_ses, replace)
margins, at(hhincome=(1 2 3 4 5)) atmeans saving (margins/`x'_hhincome, replace)
margins, at(age=(18 (1) 100)) atmeans saving (margins/`x'_agegroup, replace)
margins, at(settype=(1 2 3)) atmeans saving (margins/`x'_settype, replace)
margins, at(sex=(1 2)) atmeans saving (margins/`x'_sex, replace)
margins, at(ethn=(0 1)) atmeans saving (margins/`x'_ethn, replace)
}



foreach x in p22r p25r p28r {
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn visalib, vce(cluster psu)
margins, at(visalib=(0 1)) atmeans saving (margins/`x'_visalib, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn b01.mandl, vce(cluster psu)
margins, at(mandl=(1 2 3 4 5)) atmeans saving (margins/`x'_mandl, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn b02.demat, vce(cluster psu)
margins, at(demat=(1 2 3)) atmeans saving (margins/`x'_demat, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn soccon, vce(cluster psu)
margins, at(soccon=(1 2 3 4 5)) atmeans saving (margins/`x'_soccon, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn marriage, vce(cluster psu)
margins, at(marriage=(1 2 3 4 5)) atmeans saving (margins/`x'_marriage, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn m1, vce(cluster psu)
margins, at(m1=(1 2 3)) atmeans saving (margins/`x'_m1, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn m2, vce(cluster psu)
margins, at(m2=(0 1)) atmeans saving (margins/`x'_m2, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn internet, vce(cluster psu)
margins, at(internet=(0 1)) atmeans saving (margins/`x'_internet, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn d16, vce(cluster psu)
margins, at(d16=(0 1)) atmeans saving (margins/`x'_d16, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn i.d13r, vce(cluster psu)
margins, at(d13r=(0 1)) atmeans saving (margins/`x'_d13r, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn i.p1, vce(cluster psu)
margins, at(p1=(1 2 3)) atmeans saving (margins/`x'_p1, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn i.p2, vce(cluster psu)
margins, at(p2=(1 2 3)) atmeans saving (margins/`x'_p2, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn i.p12, vce(cluster psu)
margins, at(p12=(1 2 3 )) atmeans saving (margins/`x'_p12, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn i.p13, vce(cluster psu)
margins, at(p13=(1 2 3)) atmeans saving (margins/`x'_p13, replace)
mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn p17, vce(cluster psu)
margins, at(p17=(0 1)) atmeans saving (margins/`x'_p17, replace)
}

// foreach x in p22r p25r p28r {
// foreach y in visalib b01.mandl b02.demat soccon marriage m1 b05.d8 d16 b05.d13r p17 i.p1 i.p2 i.p12 i.p13 m2 {
// mlogit `x' b01.edu parhedu ses b01.hhincome age b01.settype b01.sex ethn `y', vce(cluster psu)
// }
// }

.
