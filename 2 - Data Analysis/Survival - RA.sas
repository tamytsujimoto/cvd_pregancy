%let path = /folders/myshortcuts/OneDrive/5 - Research/1 - GRA/CVD-Pregnancy-Project---Codes/1 - Data Assembly;

/*********************************************************************
 							LOADING DATASET
 *********************************************************************/


proc import datafile = "&path./cvd_final.csv"
	out = cvd
	dbms = csv replace;
	getnames = yes;
run;

/*********************************************************************
 							PH MODEL
 *********************************************************************/

data cvd;
	set cvd;
	pce_risk = pce_risk*100;
run;

/* all sample */
proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_t;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_t;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth pce_risk /clparm;
	domain flag_subpop_t;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth pce_risk /clparm;
	domain flag_subpop_t;
run;

/* all women */
proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_w;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_w;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth pce_risk/clparm;
	domain flag_subpop_w;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth pce_risk/clparm;
	domain flag_subpop_w;
run;
 
/* all men */
proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_m;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth /clparm;
	domain flag_subpop_m;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome(0) = flag_rhmtd_arth pce_risk/clparm;
	domain flag_subpop_m;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth pce_risk/clparm;
	domain flag_subpop_m;
run;

proc surveyphreg data = cvd;
	class flag_rhmtd_arth(ref='0');
	strata sdmvstra;
	cluster sdmvpsu;
	weight wtmec;
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth pce_risk/clparm;
	domain flag_subpop_m;
run;



proc freq data=cvd;
table cvd_outcome2*flag_rhmtd_arth;
by flag_subpop;
run;

proc sort data=cvd out=cvd2; by flag_subpop_m; run;

proc phreg data = cvd2;
	class flag_rhmtd_arth(ref='0');
	model time_exm*cvd_outcome2(0) = flag_rhmtd_arth pce_risk;
	by flag_subpop_m;
run;
 