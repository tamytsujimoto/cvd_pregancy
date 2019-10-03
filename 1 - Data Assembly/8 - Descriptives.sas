%let path = /folders/myshortcuts/OneDrive/5 - Research/1 - GRA/CVD-Pregnancy-Project---Codes/1 - Data Assembly;

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


****************
* SURVEY MEANS *
****************;

*1999-2014*;

*Soc*;
proc surveymeans data=cvd mean stderr min PERCENTILE=(25 50 75) max plots = none;
stratum sdmvstra;
cluster sdmvpsu;
class gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
domain flag_subpop_m;
var age hh_size fam_size fmpir gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
weight wtmec_c1; 
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
domain flag_subpop_m;
var age hh_size fam_size fmpir gender race educ_level marit_stat hh_income fam_income flag_milit_stat birth_country ctzn_stat yrs_us;
weight wtmec_c1; 
RUN;

*Trad*;
proc surveymeans data=cvd missing nmiss mean stderr min PERCENTILE=(25 50 75) max plots = none;
where cohort = 1;
stratum sdmvstra;
cluster sdmvpsu;
class flag_cons_30m
        flag_hst_htn
        flag_htn_trt
        flag_stg1_htn
        flag_smkng_cur
        flag_smkng_hst
        flag_smkng_nvr
        flag_smkng_100
        flag_cot_lvl10
        flag_diab_hst
        flag_diab_trt_pill
        flag_diab_trt_ins
        flag_diab_fglu
        flag_diab_a1c
        flag_diab_ogtt
        flag_diab
        flag_hst_chf
        flag_hst_chd
        flag_hst_angn
        flag_hst_hattck
        flag_hst_strk
        flag_hst_cvd
        flag_rhmtd_arth;
domain flag_subpop_w;
var flag_cons_30m
        flag_hst_htn
        flag_htn_trt
        flag_stg1_htn
        flag_smkng_cur
        flag_smkng_hst
        flag_smkng_nvr
        flag_smkng_100
        flag_cot_lvl10
        flag_diab_hst
        flag_diab_trt_pill
        flag_diab_trt_ins
        flag_diab_fglu
        flag_diab_a1c
        flag_diab_ogtt
        flag_diab
        flag_hst_chf
        flag_hst_chd
        flag_hst_angn
        flag_hst_hattck
        flag_hst_strk
        flag_hst_cvd
        flag_rhmtd_arth
        bpxsy_avg
	  	bpxdi_avg
	  	bpxsy_avg_trt
	  	bpxsy_avg_untrt
	  	cho_total
	  	cho_hdl
	  	cot_lvl
	  	fglu
	  	a1c
	  	ogtt;
weight wtmec_c1; 
RUN;


