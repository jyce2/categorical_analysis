/*1.a) Assuming the table margins are fixed, 
calculate the probability of each possible 2×2 table 
which could have been observed with these fixed margins. 
Provide a relevant table listing these probabilities. 
Use this table to conduct an appropriate statistical test 
to address whether there is an association between treatment and response.*/

/*1.b) Calculate the odds ratio 
and its 95% confidence interval for the effect of the drug 
compared to the placebo on seizure incidence.

Exact OR = 0.167 
95% CI = (0.003, 2.454) */
data seizure;
	input trmt $ response $ count;
	cards;
drug yes 1
drug no 9
placebo yes 4
placebo no 6
;

* OR Fisher's exact test; 
title 'Question 1b';
proc freq data=seizure order=data;
	weight count;
	table trmt*response / expected;
	exact fisher or; *odds ratio;
run;

* Probability table values;
%macro c(n11=, n12=, n21=, n22=);
data calculate&n11;
	prob&n11 =  divide((fact(10)*fact(10)*fact(5)*fact(15)),(fact(20)*fact(&n11)*fact(&n12)*fact(&n21)*fact(&n22)));
run;

title 'Question 1a';
proc print data=calculate&n11;
run;
%mend c;

%c(n11=0, n12=10, n21=5, n22=5);
%c(n11=1, n12=9,  n21=4, n22=6);
%c(n11=2, n12=8,  n21=3, n22=7);
%c(n11=3, n12=7,  n21=2, n22=8);
%c(n11=4, n12=6,  n21=1, n22=9);
%c(n11=5, n12=5,  n21=0, n22=10);

* Fisher's exact statistical test under exact distribution (hypergeometric);
data pvalue;
	pvalue = sum(0.13545, 0.016254, 0.13545, 0.016254); *2-sided p-value = sum of prob. at or less than observed prob;
run;

title 'Question 1a';
proc print data=pvalue;
run;

*2.
Under a significance level of 0.05 and a target power of at least 80%, 
calculate the sample size needed for each group to compare the proportions of seizures 
between the drug (0.075) and placebo (0.375) groups, assuming equal sample sizes for two groups. 

Sample size needed for each group: 30
What is the power if the sample size is 10 per group, as in Problem 1?
Power: 0.352;

title 'Question 2';
proc power;
	twosamplefreq test=pchi
	groupproportions= (0.075 0.375)
	power = 0.8
	ntotal=.;
run;

proc power;
	twosamplefreq test=pchi
	groupproportions= (0.075 0.375)
	power = .
	groupns = (10 10);
run;
title;

*3. 
Under minimal assumptions, conduct a statistical test to assess the association 
between having positive (vs. negative) response and region, while controlling for age group. 
Briefly justify your method. If statistically significant, state whether those in 
urban area have a more or less positive response as compared to those in rural area.;

data three;
	length age $10 region $6;
	input age $ region $ response $ count @@;
	n_response=(response='yes');
	cards;
<35 urban no 25 
<35 urban yes 65
<35 rural no 41 
<35 rural yes 36
35-44 urban no 32
35-44 urban yes 37
35-44 rural no 41
35-44 rural yes 27
45+ urban no 23 
45+ urban yes 53 
45+ rural no 30 
45+ rural yes 35
;
run;

* 2x2 table;
* MH test Qmh statistic = 16.2763 (p-value <0.0001) 
under chisq df 1  ;

*MH test with Mantel-Fleiss criterion > 5;
*OR, Homogeneity of OR;
title 'Question 3, 4';
proc freq data=three order=data;
	weight count;
	table age*region*response 
	/nocol nopct chisq cmh(mf) measures; 
run;

* 3. Direction estimate = 0.1887 distance measure only d=;
* difference of 1*urban - 1*rural , 
averaged across age group;
* urban have more positive response as compared to those in rural;

title 'Question 3,4';
proc glm data=three;
	class age region;
	freq count;
	model n_response = age region;
	estimate 'proportion' region -1 1;
run; 


*4 Common odds ratio and 95% CI 
0.4543	(0.3090, 0.6680);
* Controlling for age group, rural has 0.4543 times the odds of
positive response than those in urban;
* equivalently take inverse: 
2.2012 (1.497, 3.236)
* Controlling for age group, urban has 2.201 times the odds of
positive response than those in rural.

* Assess Homogeneity of OR for area across age groups: 
Breslow-Day chi square statistic: 1.3414 w/ chi sq df 2 
not sig, fail to reject H0, OR are likely homogeneous across age groups;

 
* 5. Different proportions of positive response 
according to age group, controlling for region.
Treat age group as a nominal variable;
* MH Row Mean Scores Differ statistic =7.9196 df2
p-value=0.0191;
* reject H0;
* There are different proportions of positive response by age group, 
controlling for region;

title 'Question 5';
proc freq data=three order=data;
	weight count;
	table region*age*response / nocol norow nopct cmh;
run;

* 6. Mathematically specify a logistic regression model 
for positive response (vs. negative response) with main effects 
for region and age group (but not their interaction). 
Let “45+” and “Rural” be the reference levels for their respective categories. ;

/* equivalent 
data three1;
	length age $10 region $6;
	input age $ region $ response $ count @@;
	n_response=(response='yes');
	cards;
<35 urban 1 65 <35 rural 1 36
<35 urban 0 25 <35 rural 0 41
35-44 urban 1 37 35-44 rural 1 27
35-44 urban 0 32 35-44 rural 0 41
45+ urban 0 23 45+ rural 0 30  
45+ urban 1 53 45+ rural 1 35
;
run;


proc logistic data=three1 order=data;
	freq count;
	class age(ref='45+') region(ref='rural') / param=ref; * assign ref groups and reference cell-coding;
	model response(event='1')= age region
		/scale=none aggregate;
run;
*/

* Assumptions for using logistic regression 
main effects model.
- stratified simple random sample 
- set of explanatory variables as main effects 
	(categorical or continuous)
- binomial, dichotomous response distribution 
for each housing type*stress (factor-level) combination;
;

title 'Question 6, 7, 8' ;
proc logistic data=three order=data;
	freq count;
	class age(ref='45+') region(ref='rural') / param=ref; * assign ref groups and reference cell-coding;
	model response(event='yes')= age region
		/scale=none aggregate;
	*aggregate- use unique combination groups for GOF;
	contrast 'age group' age 1 0,
									age 0 1 / e estimate=parm; 
	*joint Wald test for overall effect of age group (3 levels);
run;

* Overall effect:
Test statistic= 7.8686	chisq df2
p-value = 0.0196;

* 7. Test GOF of model:
Deviance statistic = 1.3463 chisq df2, p-value= 0.5101
* Model fits data adequately;

* 8. Logistic reg OR and 95% CI;
*CI is based on MLE methods, 
appropriate for large samples to apply asymptotic normality;
* otherwise, does not produce a unique solution/does not converge;
* Rural(ref) vs. Urban OR 	


2.207 (1.500, 3.246) does not contain 1, null value no association;
*Yes, evidence to reject H0 that odds (proportions) of positive opinion
are same between urban vs. rural areas;

* Similar to 4 OR: 
2.201 (1.497, 3.236)
Variance is based on Greenland;

* Age group OR compare to problem 5.;
* Age <35-44 vs. <35: OR = 0.577 with SE(OR)=0.2331869;
* 0.577+/-1.96*0.2331869

* 9
Under minimal assumptions, conduct a statistical test to determine whether there is a trend in the extent of opinion, 
with respect to the four opinion levels (ranging from strongly positive to strongly negative), 
across the age groups (ordered as <35, then 35-44, then 45+), 
while controlling for region.;

* test for trend controlling for region
H0: there is no trend in response across age groups;
* extended Mantel-Haenszel correlation statistic = 
0.0016	chi sq df1 p=value=0.9684
fail to reject H0;


data opinion;
	length age $10 region $6 response $5;
	input age $ region $ response $ count @@;
	cards;
<35 urban stneg 12 
<35 urban soneg 13
<35 urban sopos 40 
<35 urban stpos 25
<35 rural stneg 18 
<35 rural soneg 23
<35 rural sopos 20 
<35 rural stpos 16
35-44 urban stneg 10 
35-44 urban soneg 22
35-44 urban sopos 25 
35-44 urban stpos 12
35-44 rural stneg 14 
35-44 rural soneg 27
35-44 rural sopos 17 
35-44 rural stpos 10
45+ urban stneg 9 
45+ urban soneg 14
45+ urban sopos 33 
45+ urban stpos 20
45+ rural stneg 11 
45+ rural soneg 19
45+ rural sopos 27 
45+ rural stpos 8
;
run;

/*
data opinion2;
	length age $10 region $6 response $5;
	input age $ region $ response $ count @@;
	cards;
<35 urban stpos 25
<35 urban sopos 40 
<35 urban soneg 13
<35 urban stneg 12
<35 rural stpos 16
<35 rural sopos 20
<35 rural soneg 23 
<35 rural stneg 18 
35-44 urban stpos 12
35-44 urban sopos 25 
35-44 urban soneg 22
35-44 urban stneg 10 
35-44 rural stpos 10
35-44 rural sopos 17 
35-44 rural soneg 27
35-44 rural stneg 14 
45+ urban stpos 20
45+ urban sopos 33 
45+ urban soneg 14
45+ urban stneg 9 
45+ rural stpos 8
45+ rural sopos 27 
45+ rural soneg 19
45+ rural stneg 11
;
run;
*/

title 'Question 9';
proc freq data=opinion order=data;
	weight count;
	table region*age*response / 
		nocol norow nopct cmh trend scores=modridit jt;
run;

/*
title 'Question 9';
proc freq data=opinion2 order=data;
	weight count;
	table region*age*response / 
		nocol norow nopct cmh trend scores=rank jt;
run;
*/


* 9 part2. 
Also, conduct a separate statistical test to determine whether there are any overall differences across the age groups with respect to ordered opinion levels, 
while controlling for region.
 Briefly explain the findings from these two types of statistical tests in 1-2 sentences.

* Row Mean Scores Differ CMH 
chisq 2	test statistic 5.2121 p-value=0.0738
fail to reject H0 that ordinal response differs overall across ordinal age group, 
controlling for region;


* 10. 
p.457, 14.6 
For each region, report the gamma rank correlation coefficient
describing the strength of association b/t the age groups and level of opinion, 
along with their 95% CI;
* stratify by region;
* urban coeff: -0.0322;
* rural coeff: 0.0329;

title 'Question 10';
proc freq data=opinion order=data;
	weight count;
	table region*age*response / nocol norow scores=modridit measures cl;
	test gamma;
	
run;



*Also, produce a statistical test for the difference in the gamma rank correlation coefficients 
for urban and rural area, and briefly interpret the corresponding comparison that it provides.
* do by hand 
Fisher's Z transform test;

* 11; 
*Mathematically specify a proportional odds model for the ordinal response 
(ordered as strongly positive, somewhat positive, somewhat negative and strongly negative) 
with main effects for region and age group (but not their interaction).
State the model assumptions, mathematically define all variables in the model, 
and interpret the model parameters. Use “Rural” and “45+” as the reference groups.;


 * Proportional odds model for ordinal opinion response;
proc format;
	value $ resp
	'stpos' = '1-stpos'
	'sopos' = '2-sopos'
	'soneg' = '3-soneg'
	'stneg' = '4-stneg'
;

title 'Question 11';
proc logistic data=opinion order=formatted;
	freq count;
	class region(ref='rural') age(ref='45+') / param=reference;
	model response = region age
	/ scale=none aggregate;
	format response $resp.;
run; 
title;


* 12; 
* GOF of prop odds model, including assessment of prop odds assumption;
* Score test for proportional odds assumption / *Residual Chi-Square;
* H0: Assumption of proportional odds is met;
* H0: Model fit is adequate.;
 title 'Question 12';
proc logistic data=opinion order=formatted plots=effect(polybar x=age*region);
	freq count;
	class region(ref='rural') age(ref='45+') / param=reference;
	model response = region age region*age
	/ scale=none aggregate=(age region);
	format response $resp.;
run; 
title;

*13 Main effects prop.odds model
Odds ratio and 95% CI and pairwise comparisons;
title 'Question 13';
proc logistic data=opinion order=formatted;
	freq count;
	class region(ref='rural') age(ref='45+') / param=reference;
	model response = region age / scale=none;
	format response $resp.;
run; 
title;

* 14;
data chem;
	input age $ exposure $ cases totalpy;
	ctotal = log(totalpy/1000);
cards;
20-29 L 2 88600
20-29 M 3 37800
20-29 H 3 16900
30-39 L 5 94500
30-39 M 4 29500
30-39 H 5 23600
40-49 L 10 69200
40-49 M 8 27600
40-49 H 3 19800
50-59 L 2 41300
50-59 M 4 49500
50-59 H 1 13950
60+ L 6 48500
60+ M 8 18700
60+ H 7 56000
;

* Main effects Poisson piecewise exp PH model;
* wald test overall association;
*10.93 chi sq df 2	p-value=0.0042;
title 'Question 14, 15';
proc genmod data=chem;
	class age (ref='20-29') exposure (ref='L') / param=ref;
	model cases=age exposure /
		dist=poisson link=log offset=ctotal type3 wald; *use joint wald;
	estimate 'L vs. M' exposure 0 1 / e ;
	estimate 'L vs. H' exposure 1 0 / e ;
run;


* 16 Conditional logistic reg model;
libname lib '~/BIOS665/Data';

title 'Question 16 a-d';
proc logistic data=lib.drug alpha=0.0167;
	class drug (ref='B') / param=ref;
	strata patient; *within-subject effects;
	model response(event='F') = drug / waldrl ;
	exact drug / estimate=both;
	estimate 'A vs C' drug 1 -1 / e exp cl;
run;



* 17-18 Piecewise exponential model w/ main effects;
data infe;
	input trmt $ time $ failure totalpyears;
	lmonths = log(totalpyears);
	cards;
t1 yr1 15 62
t1 yr2 13 64
t1 yr3 7 67.5
t2 yr1 12 63.5
t2 yr2 7 69
t2 yr3 10 67
;

* See p. 420;
* total person-yrs = 1*(15*0.5 + 9*0.5 + (74-24))=62;
* total person-yrs = 1*(13*0.5 + 7*0.5 + (74-20));
* total person-yrs = 1*(7*0.5 + 6*0.5 + (74-13));
* total person-yrs = 1*(12*0.5 + 9*0.5 + (74-21));
* total person-yrs = 1*(7*0.5 + 3*0.5 + (74-10));
* total person-yrs = 1*(10*0.5 + 4*0.5 + (74-14));


title 'Question 17, 18ab';
proc genmod data=infe order=data;
	class time(ref='yr1') trmt(ref='t1') ;
	model failure = trmt time / dist=poisson link=log offset=lmonths;
run;

* 18a) GOF: Deviance test statistic: 2.5028	chi sq df 2 p-value=0.2861;
* 18b) HR = exp(-0.2197) 
95% CI:  exp( -0.7119) , exp(0.2725	)

* 19ab Primary repeated measures model;

proc format;
	value order
	1 = '_1'
	2 = '2'
;

proc format;
	value $ ordertrt
	'control' = '_1 control'
	'test' = '2 test'
;

proc format;
	value orderbase
	0 = '_0'
	1 = '1'
;


title 'Question 19ab w/ interaction';
proc genmod data=lib.condition descending order=formatted;
	class studyid time trt center baseline; 
	model favorable = center baseline time trt time*trt center baseline 
		/ link=logit dist=bin type3;
	repeated subject=studyid(center)
		/type=exch corrw;
	format time center order.;
	format trt $ordertrt.;
	format baseline orderbase.;
run;

title 'Question 19ab only main';
proc genmod data=lib.condition descending;
	class studyid time(ref='1') trt(ref='control') center(ref='1') baseline(ref='0'); 
	model favorable = center time trt center baseline 
		/ link=logit dist=bin type3;
	repeated subject=studyid*center 
		/type=exch corrw;
run;


title 'Question 19b all interactions';
proc genmod data=lib.condition descending order=formatted;
	class studyid time trt center baseline; 
	model favorable = center time trt baseline 
			time*trt center*time baseline*time center*trt 
			center*baseline trt*baseline
		/ link=logit dist=bin type3 ;
	repeated subject=studyid*center 
		/type=exch corrw;
	format time center order.;
	format trt $ordertrt.;
	format baseline orderbase.;
run;

title 'Question 19ab final model';
proc genmod data=lib.condition descending;
	class studyid time(ref='1') trt(ref='control') center(ref='1'); 
	model favorable = center time trt center time*trt
		/ link=logit dist=bin type3;
	repeated subject=studyid*center 
		/type=exch corrw;
run;

* 20 ab) with OR of trmt for each follow-up visit;
title 'Question 20ab';
proc genmod data=lib.condition descending;
	class studyid time(ref='1') trt(ref='control') center(ref='1'); 
	model favorable = center time trt center time*trt
		/ link=logit dist=bin type3;
	repeated subject=studyid*center 
		/ type=exch;
	lsmeans time*trt / exp cl;
	slice time*trt / sliceby(time) means exp cl oddsratio;
run;







