libname bs851 '/home/u63575405/Spring2025/BS851';
proc print data=bs851.ist_2025 (obs=10); run;
proc sort data = bs851.ist_2025;
	by trt;
run;

proc plan seed = 10;
	factors site = 10 ordered blocks = 120 ordered trt= 8 random/noprint; 
	output out = rsched trt cvals = ('A' 'A' 'B' 'B' 'C' 'C' 'C' 'C');
run;quit;
* add country-specific subject ids;
data rsched2;
	set rsched;
	by site;
	if first.site then subject=0;
	subject+1;
	if site=1 then
	subid=subject+100;
	if site=2 then
	subid=subject+200;
	if site=3 then
	subid=subject+300;
	if site=4 then
	subid=subject+400;
	if site=5 then
	subid=subject+500;
	if site=6 then
	subid=subject+600;
	if site=7 then
	subid=subject+700;
	if site=8 then
	subid=subject+800;
	if site=9 then
	subid=subject+900;
	if site=10 then
	subid=subject+1000;
run;
* print out first 20 observations of first country;
proc print data=rsched2(obs=20) noobs;
by site;
var subid trt;
run;

* table one;
** frequencies for categorical variables, stratified by treatment group;
proc freq data = bs851.ist_2025;
	tables sex*trt stype*trt;
run;
** means for continuous variables, stratified by treatment group;
proc means data = bs851.ist_2025 mean clm;
	class trt;
	var age rsbp;
run;
proc freq data=bs851.ist_2025;
	table stroke14*trt;
run;

* crude hypothesis testing of treatment(s) v. placebo;
proc logistic data = bs851.ist_2025;	
	class trt (param=ref ref = 'Placebo');
	model stroke14(event='Y')=trt/ risklimits;
run;

ods title 'High Dose Compared to Placebo, ITT';
proc freq data = bs851.ist_2025;
	table trt*stroke14/nocol nopercent chisq riskdiff(column=2 cl=wald norisks) alpha=0.025;
	where trt in ('High Dose', "Placebo");
run;
ods title 'Low Dose Compared to Placebo, ITT';
proc freq data = bs851.ist_2025;
	table trt*stroke14/nocol nopercent chisq riskdiff(column=2 cl=wald norisks) alpha=0.025;
	where trt in ('Low Dose', "Placebo");
run;

* testing treatments v. placebo, adjusting for sex;
proc logistic data = bs851.ist_2025;	
	class trt (ref = 'Placebo') sex (ref = 'M') /param =glm;
	model stroke14(event='Y')=trt sex;
	lsmeans trt/ adjust=bon exp cl diff=control("Placebo");
run;

* drop study participants that were not compliant, rerun analyses;
data compliant;
	set bs851.ist_2025;
	where CMPLASP like "Y" and CMPLHEP like "Y";
run;
** confirm all participants are compliant in both;
proc freq data = compliant;
	tables CMPLASP CMPLHEP;
run;
** rerun analyses to test if crude association significant;
proc logistic data = compliant;	
	class trt (ref = 'Placebo') / param = glm;
	model stroke14(event='Y')=trt/ risklimits;
	lsmeans trt/ adjust=bon exp cl diff=control("Placebo");
run;

ods title 'High Dose Compared to Placebo, PP';
proc freq data = compliant;
	table trt*stroke14/nocol nopercent chisq riskdiff(column=2 cl=wald norisks) alpha=0.025;
	where trt in ('High Dose', "Placebo");
run;
ods title 'Low Dose Compared to Placebo, PP';
proc freq data = compliant;
	table trt*stroke14/nocol nopercent chisq riskdiff(column=2 cl=wald norisks) alpha=0.025;
	where trt in ('Low Dose', "Placebo");
run;

* secondary analysis;
proc lifetest data=bs851.ist_2025;
	time td*id14(0);
	strata trt;
run;

ods select LogNegLogSurvivalPlot;
proc lifetest data=bs851.ist_2025 plots=(loglogs);
	time td*id14(0);
	strata trt;
run;

proc phreg data= bs851.ist_2025;
	class trt /param = glm;
	model td*id14(0) = trt/risklimits;
	lsmeans trt/ adjust=bon exp cl diff=control("Placebo");
run;
