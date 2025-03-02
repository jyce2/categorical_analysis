* 1 - Logistic model;
* Input data bioassay study to compare two treatments;
title ' 1a-b) Bioasay trmt A data sorted by favorable response first';
data bioassay;
	input dose status $ count @@;
	ldose= log10(dose);
	cards;
1 fav 43 1 unfav 57
10 fav 55 10 unfav 45 
100 fav 68 100 unfav 32
;
run;

proc sort data=bioassay;
	by status; *sort response (fav vs. unfav);
run;


* 1a-b);
proc logistic data=bioassay;
	freq count;
	model status = ldose ldose*ldose / 
	/*quadratic term= residual score statistic to determine GOF of intercept+slope only model*/
		scale=none aggregate selection=forward 
		include=1 details covb; *include intercept and ldose term;
run;
title;

* Probit quantal response data analysis;
* Input data for treatment A; 
data pain;
	input dose fav total @@;
	ldose = log10(dose);
	datalines;
1 43 100
10 55 100
100 68 100
;
run;

* 1b/c);
ods graphics on;
title '1b-c Probit logistic model';
proc probit data=pain plot=ippplot;
	model fav/total = ldose / dist=logistic lackfit	
		inversecl (prob= 0.25 0.50 0.75);
run;
ods graphics off;

* 1d);
ods graphics on;
title '1d Probit normal model';
proc probit data=pain plot=ippplot;
	model fav/total = ldose / dist=normal lackfit	
		inversecl (prob= 0.25 0.50 0.75);
run;
ods graphics off;

* 2 - Probit analysis comparing two treatments;
* Input data for treatment A & B;
data pain2;
	input trmt $ dose response $ count;
	int_a=(trmt='A');
	int_b=(trmt='B');
	ldose=log(dose);
	datalines;
A 1 F 43
A 1 U 57
A 10 F 55
A 10 U 45 
A 100 F 68
A 100 U 32 
B 2 F 34
B 2 U 66
B 20 F 52
B 20 U 48 
B 200 F 75
B 200 U 25 
;
run;


* 2a-b) GOF, equal slopes; 
title '2a-b Two intercepts and two slopes (parallel lines test) model';
proc logistic data=pain2;
	freq count;
	model response = int_a int_b
		ldose*int_a ldose*int_b
		ldose*int_a*ldose*int_a
		ldose*int_b*ldose*int_b / noint
	scale=none aggregate include=4 selection=forward details;
	eq_slope: test int_aldose=int_bldose; *dilution assumption;
run;


* 2c) Fit a logistic model with equal slopes;
title '2c - Logistics model equal slopes';
proc logistic data=pain2  covout;
	freq count;
	model response = int_a int_b ldose /
	noint scale=none aggregate covb;
run;

* 3a) Poisson main effects model for count data, exposure case, and categorical factors;
data births;
	input age $ order $ cases total;
	ctotal = log(total/100000);
	ltotal = log(total); *offset in GLM, where link=log(mu);
cards;
20-24 1 120 329472 
20-24 2 151 326736
20-24 3 64 175688
25-29 1 50 114990
25-29 2 120 208690
25-29 3 107 207062
30-34 1 40 39455
30-34 2 85 83225
30-34 3 106 117334
35-39 1 38 14207
35-39 2 89 28490
35-39 3 102 45018
40+ 1 20 3053
40+ 2 44 5389
40+ 3 86 8655
;

* 3a-b main effects Poisson model;
title '3a-b Poisson model';
proc genmod data=births ;
	class order (ref='1') age (ref='20-24') / param=ref;
	model cases = age order 
		/ predicted dist=poisson link=log offset=ctotal;
	estimate 'order2 vs. order1' order 1 0 / exp;
	estimate 'order3 vs. order1' order 0 1 / exp;
run;

* 4a-c Life table test; 
data dental;
	length trmt $ 15;
	input interval trmt $ status count @@;
	datalines;
0 Existing 1 3 0 Existing 0 2
6 Existing 1 5 6 Existing 0 2
12 Existing 1 16 12 Existing 0 5
18 Existing 1 35 18 Existing 0 12
24 Existing 0 50
0 Experimental 1 1 0 Experimental 0 1
6 Experimental 1 3 6 Experimental 0 2
12 Experimental 1 10 12 Experimental 0 15
18 Experimental 1 20 18 Experimental 0 18
24 Experimental 0 60
;
run;



title '4a - life table per trmt';
ods graphics on;
proc lifetest data=dental method=lt
	intervals=(0 to 24 by 6);
	freq count;
	strata trmt;
	time interval*status(0);
run;

* 4d Mantel-Cox test compare two treatments
disregard withdrawals;

data mcdental;
	input time $ treatment $ status $ count @@;
	cards;
0-6 t1 event 3 0-6 t1 not 127
0-6 t2 event 1 0-6 t2 not 129
6-12 t1 event 5 6-12 t1 not 122
6-12 t2 event 3 6-12 t2 not 126
12-18 t1 event 16 12-18 t1 not 106
12-18 t2 event 10 12-18 t2 not 116
18-24 t1 event 35 18-24 t1 not 71
18-24 t2 event 20 18-24 t2 not 96
;
run;

proc freq data=mcdental;
	weight count;
	table time*treatment*status / cmh;
run;

* 5 Piecewise exponential model;
data fit;
	input trmt $ time $ failure totalpmonths;
	smonths = 1000*totalpmonths;
	lmonths = log(totalpmonths);
	cards;
t1 _0-6 3 765 
t1 6-12 5 729
t1 12-18 16 645
t1 18-24 35 441
t2 _0-6 1 774
t2 6-12 3 753
t2 12-18 10 663
t2 18-24 20 474
;

* See p. 420;
* 765 = total person-months = 6*(3*0.5 + 2*0.5 + (130-5))

* 5a;
* piecewise exponential with scaling 1000;
proc logistic data=fit;
	class time trmt(ref='t1') / param=ref;
	model failure / smonths=time trmt time*trmt / 
		scale=none include=2 selection=forward;
run;


* 5a-c equivalent without scaling;
* Fits piecewise exponential via poisson reg;
proc genmod data=fit order=data;
	class time(ref='_0-6') trmt(ref='t1') ;
	model failure = time trmt / dist=poisson link=log offset=lmonths;
run;






