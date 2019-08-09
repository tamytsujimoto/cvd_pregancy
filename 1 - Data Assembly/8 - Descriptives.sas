%let path = /folders/myshortcuts/OneDrive/5 - Research/1 - GRA/CVD-Pregnancy-Project---Codes;

* LOADING DATASET *;

proc import datafile = "&path./cvd_final.csv"
	out = cvd
	dbms = csv replace;
	getnames = yes;
run;


*****************************
* UNWEIGHTED MISSING COUNTS *
*****************************;

PROC SORT data = cvd; by cohort; run;

* TOTAL *;
PROC MEANS data = cvd(drop = VAR1 SEQN cycle) stackodsoutput nmiss n; 
where flag_subpop = 1;

 ods output summary=work.mis_tot;
run;

*BY COHORT *;
PROC MEANS data = cvd(drop = VAR1 SEQN cycle) stackodsoutput nmiss n; 
where flag_subpop = 1;
by cohort;

 ods output summary=mis_coh;
run;

* COMBINING OUTPUTS *;	
data mis_tot;
set mis_tot;
cohort = 0;
run;

data out_mis;
set mis_tot mis_coh;
perc_mis = 
run;

proc print data=out_mis;
run;

****************
* SURVEY MEANS *
****************;

*1999-2014*;

*Soc*;
proc surveymeans data=cvd mean stderr min PERCENTILE=(25 50 75) max plots = none;
stratum sdmvstra;
cluster sdmvpsu;
class gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
domain flag_subpop;
var age hh_size fam_size fmpir gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
weight wtmec; 
RUN;

*Preg*;
proc surveymeans data=cvd mean stderr min PERCENTILE=(25 50 75) max plots = none;
stratum sdmvstra;
cluster sdmvpsu;
class flag_cur_preg flag_hst_preg flag_preg_eli flag_inft_wght_9lb flag_age_diab_30;
domain flag_subpop;
var flag_cur_preg flag_hst_preg flag_preg_eli flag_inft_wght_9lb flag_age_diab_30 n_preg n_preg_live n_vgnl_dlvry age_fst_live_brth age_lst_live_brth age_inft_wght_9lb age_diab;
weight wtmec; 
RUN;

*Pregrisk*;
proc surveymeans data=cvd mean stderr min PERCENTILE=(25 50 75) max plots = none;
stratum sdmvstra;
cluster sdmvpsu;
class flag_pretrm_dlvry flag_infnt_sga flag_preg_comp_9906 flag_2preg_comp_9906 flag_gdm flag_hst_brstfd flag_cur_brstfd flag_any_brstfd flag_infnt_brstfd flag_any_preg_comp;
domain flag_subpop;
var flag_pretrm_dlvry flag_infnt_sga flag_preg_comp_9906 flag_2preg_comp_9906 flag_gdm flag_hst_brstfd flag_cur_brstfd flag_any_brstfd flag_infnt_brstfd flag_any_preg_comp
n_pretrm_dlvry n_infnt_sga age_gest_diab n_infnt_brstfd;
weight wtmec; 
RUN;


*1999-2006*;


*Soc*;
proc surveymeans data=cvd mean stderr min PERCENTILE=(25 50 75) max plots = none;
where cohort = 1;
stratum sdmvstra;
cluster sdmvpsu;
class gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
domain flag_subpop;
var age hh_size fam_size fmpir gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
weight wtmec_c1; 
RUN;


