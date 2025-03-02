* 1);

data policy;
	input type $ stress $ response count @@;
	datalines;
urban L 1 50 urban M 1 85 urban H 1 102
urban L 0 15 urban M 0 68 urban H 0 87
rural L 1 35 rural M 1 77 rural H 1 90
rural L 0 20 rural M 0 68 rural H 0 64
;

* Assumptions for using logistic regression 
main effects model.
- stratified simple random sample 
- set of explanatory variables as main effects 
	(categorical or continuous)
- binomial, dichotomous response distribution 
for each housing type*stress (factor-level) combination;
;

* 1a-d, f);
proc logistic data=policy order=data;
	freq count;
	class type(ref='urban') stress(ref='L') / param=ref; * assign ref groups and reference cell-coding;
	model response(event='1')= type stress 
		/scale=none aggregate;
	contrast 'urban vs. rural' type 1 / e estimate=parm; *print C matrix, H0: beta(2) = 0;
run;

* 1e) Provide predicted probabilities for unfavorable response;
proc logistic data=policy order=data noprint;
	freq count;
	class type(ref='urban') stress(ref='L') / param=ref;
	model response(event='0')= type stress;
	output out=predict pred=prob;
run;

* Output predicted probabilities per factor-level;
proc print data=predict;
run;

* 3a) ;
data relief;
	input dose $ response $ count;
	datalines;
1 1 25
1 0 45
10 1 35
10 0 35
100 1 56
100 0 14
;

ods graphics on;
*ods select ParameterEstimates;
proc logistic data=relief order=data plots=all;
	freq count;
	class dose(ref='1') / param=ref;
	model response(event='1')=dose ;
run;
ods graphics off;


*3b;
data reliefc;
	input log10dose response $ count;
	datalines;
0 1 25
0 0 45
1 1 35
1 0 35
2 1 56
2 0 14
;

*ods select ParameterEstimates;
ods graphics on;
proc logistic data=reliefc alpha=0.01;
	freq count;
	model response(event='1')=log10dose / 
		selection=forward details lackfit; *residual Q(rc) chi-square GOF test;
	contrast 'check' log10dose 1  / e estimate=parm; 
run;

