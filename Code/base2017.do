clear all


use "***\IOp Mexico\Data\ESRU-EMOVI 2017 Entrevistado.dta", clear

*==================================================================================*
* Keep only respondents who were living with their father, mother or both at age of 14
keep if p25==1 |  p25==2 | p25==3
*==================================================================================*

*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*              VARIABLES                 *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*

*Age squared
gen edad2=p05*p05

*Rural or Urban at age of 14
gen rural=0 if p24>0 & p24<5
replace rural=1 if  p24==5

*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*        REGION AT AGE OF 14             *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*

*BC-Coahuila-Chihuahua-Monterrey-Sonora-Tamaulipas
gen region14=1 if p23==2| p23==5| p23==8| p23==19| p23==26| p23==28
*BCsur-Sinaloa-Zacatecas-Nayarit-Durango
replace region14=2 if p23==3| p23==10| p23==18| p23==25| p23==32
*Aguas-Colima-Jalisco-Michoacan-SLP
replace region14=3 if p23==1| p23==6| p23==14| p23==16| p23==24
*Guanajuato-Hidalgo-Mexico-Morelos-Puebla-Queretaro-Tlaxcala
replace region14=4 if p23==11| p23==13| p23==15| p23==17| p23==21| p23==22| p23==29
*CDMX
replace region14=5 if p23==9
*Campeche-Chiapas-Guerrero-Oaxaca-Qroo-Tabasco-Veracruz-Yucatan
replace region14=6 if p23==4| p23==7| p23==12| p23==20| p23==23| p23==27| p23==30| p23==31


*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*             EDUCATION                  *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*


***Education of the father of the respondent
gen niveducp=1 if p42==2
replace niveducp=2 if p43==1
replace niveducp=3 if p43==2
replace niveducp=4 if p43==3 | p43==4
replace niveducp=5 if p43==5 | p43==6 | p43==7 | p43==9
replace niveducp=6 if p43==8 | p43==10 | p43==11 | p43==12

gen  añosescp=.
replace  añosescp=0 if niveducp==1
replace  añosescp=p44 if niveducp==2
replace  añosescp=6+p44 if niveducp==3
replace  añosescp=9+p44 if niveducp==4
replace  añosescp=12+p44 if niveducp==5
replace  añosescp=16+p44 if niveducp==6
label var  añosescp "Years of education of the father"

***Education of the mother of the respondent
gen niveducm=1 if p42m==2
replace niveducm=2 if p43m==1
replace niveducm=3 if p43m==2
replace niveducm=4 if p43m==3 | p43m==4
replace niveducm=5 if p43m==5 | p43m==6 | p43m==7 | p43m==9
replace niveducm=6 if p43m==8 | p43m==10 | p43m==11 | p43m==12

gen añosescm=.
replace añosescm=0 if niveducm==1
replace añosescm=p44m if niveducm==2
replace añosescm=6+p44m if niveducm==3
replace añosescm=9+p44m if niveducm==4
replace añosescm=12+p44m if niveducm==5
replace añosescm=16+p44m if niveducm==6
label var añosescm "Years of education of the mother"

***Average years of education of both parents (if two parents)
egen anesc_padres=rowmean( añosescp añosescm)
label var anesc_padres "Years of education of parents"


*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*        PERCENTILES OF WEALTH           *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*


***Household at age 14

gen piso=.
replace piso=1 if p29==2 | p29==3
replace piso=0 if p29==1

recode p30* p32* p33* p34* (2=0) (8=.)

sum  p30_a  p30_b p30_c p30_e  p33_a p33_b p33_e p33_d   


mca p30_a  p30_b p30_c p30_e  p33_a p33_b p33_e p33_d   , method(burt)
predict i_or if e(sample)
replace i_or=i_or*(-1)
sum i_or[iw=factor]
gen iriq_or=(i_or-r(mean))/r(sd)
label var iriq_or "Índice de riqueza del hogar de origen"
reg iriq_or p05 edad2
predict double resid_or, residuals
xtile percentil_or=resid_or [aw=factor], nq(100)
label var percentil_or "Percentil of wealth at age 14"	

	

***Actual household

recode p125* p126* p128* p129* (2=0)


sum  p125b   p126a p126b p126c  p126e   p126j p126k p126l p126m  p126o p128c  p128d   


mca p125b   p126a p126b p126c  p126e   p126j p126k p126l p126m  p126o p128c  p128d   , method(burt)
predict i_des if e(sample)
replace i_des=i_des*(-1)
sum i_des[iw=factor]
gen iriq_des=(i_des-r(mean))/r(sd)
reg iriq_des p05 edad2
predict double resid_des, residuals
xtile percentil_des=resid_des [aw=factor], nq(100)
label var percentil_des "Percentil of wealth at age 14"	


*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*          Speaking indigenous language        *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*

gen hli=0 if p39==2 | p39m==2
replace hli=1 if p39==1 | p39m==1
label var hli "Parents speak indigenous language" 
label define hli 0 "Parents doesn´t speak indigenous language" 1 "Parents speak indigenous language" 
label values hli hli

*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
*            Skin Tone                   *
*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*
tab p151

*==================================================================================*
* RENAME VARIABLES
*==================================================================================*

rename  p05 edad
rename p06 sexo_inf
rename p151 color_p

*==================================================================================*
*LABELS
*==================================================================================*
label var edad "Age of respondent" 
label var percentil_or "Percentil of wealth at age 14" 
label var percentil_des "Percentil of wealth in 2017" 
label var anesc_padres "Years of education of parents (average)" 
label var sexo_inf "Sex" 
label var hli "Parents speak indigenous language" 
label var color_p "Skin Tone" 
label var factor "Factor" 
label var region14 "Region at 14 years old" 

keep  folio sexo_inf edad  percentil_or percentil_des  hli  rural region14    anesc_padres  color_p  factor                 
order  folio sexo_inf edad  percentil_or percentil_des  hli  rural region14    anesc_padres  color_p  factor                 


save "***\IOp Mexico\Data\Emovi_2017.dta", replace

sum sexo_inf edad  percentil_or percentil_des  hli  rural region14    anesc_padres  color_p 






