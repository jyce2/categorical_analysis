* BIOS 665 hw 1;

* 1a-e);
/* A study of 185 participants was conducted to evaluate whether a test treatment had a better outcome than placebo with the outcome being either favorable or unfavorable response. 51 of 90 participants in the test treatment had a favorable response, 
while 34 of 95 participants in the placebo group had a favorable response.
a) Construct a contingency table summarizing the association described between treatment assignment groups and the response.
b) Calculate an estimate of the proportion of participants with favorable response in each of the treatment groups, along with corresponding two-sided 95% confidence intervals.
c) Calculate an estimate for the difference in proportions of favorable response and a corresponding two-sided 95% confidence interval for comparing the test treatment group and placebo group.
d) Conduct a statistical test for whether any association exists between the treatment group and response. Please list two conditions your data must satisfy for this test to be valid.
e) Interpret and briefly discuss your results from parts (b) through (d).
*/
data one;
	input treat $ outcome $ count;
	cards;
placebo fav 34
placebo un 61
test fav 51
test un 39
;

/* equivalent except one-sided p-value is reversed;
data one_;
	input treat $ outcome $ count @@;
	cards;
test fav 51 test un 39
placebo fav 34 placebo un 61
;

/*
proc print data=one;
sum count;
run;
*/

proc freq data=one order=data;
	weight count;
	table treat*outcome / expected chisq nocol riskdiff (correct) measures;
run;

* 2a-c;
data twoA;
	input dose $ response $ count;
	cards;
high yes 58
high no 17
low yes 25
low no 30
;

*chi square test;
proc freq data=twoA order=data;
	weight count;
	table dose*response / expected chisq nocol nopct measures;
run;

* 2d-e;
data twoB;
	input dose $ response $ count;
	cards;
high yes 6
high no 2
low yes 3
low no 7
;

* fisher's exact test;
proc freq data=twoB order=data;
	weight count;
	table dose*response / expected nocol nopct chisq;
	exact or;
run;
	
* 3a-c;
data three;
	input placebo $ treat $ count;
	cards;
clear clear 129
clear not 22
not clear 55
not not 44
;

* preview table;
proc freq data=three order=data;
	weight count;
	table placebo*treat / nopercent nocol;
run;

* Mcnemar's matched pairs association test;
ods select McnemarsTest;
proc freq data=three order=data;
	weight count;
	table placebo*treat / agree;
	*exact mcnem;
run;

* 4;
data four;
	input disease $ test $ count @@;
	cards;
present + 92 present - 30
absent + 20 absent - 75 
;

* Sensitivity/specificity screening test;
proc freq data=four order=data;
	weight count;
	tables disease*test / riskdiff alpha=0.05;
run;

proc freq data=four order=data;
	weight count;
	tables disease*test / riskdiff alpha=0.10;
run;

* 5;
/*In designing a randomized clinical trial for the evaluation of a treatment for cardiovascular disease 
in terms of a favorable outcome after 6 months of follow-up, 
assume that you expect favorable response proportions of 0.66 for the test treatment 
and 0.42 for an appropriate control after this follow-up period.

* a)
Using a two-sided 0.05 significance level with balanced allocation to these two groups, 
determine the sample size that would be necessary to provide 0.90 power for this planned study.*/
proc power;
	twosamplefreq test=pchi
	groupproportions= (0.66 0.42)
	power = 0.9
	ntotal=.;
run;

* b)
With twice as many patients for the treatment as for the appropriate control, 
determine the sample sizes needed to provide about 0.80 power at the two-sided 0.05 significance level 
for this study with the same expected proportions of favorable response as above.;
proc power;
	twosamplefreq test=pchi
	groupproportions = (0.66 0.42)
	power = 0.8
	groupweights = (2 1)
	ntotal=.;
run;

* c)
If at the end of the study there were 70 patients enrolled in each group, 
what is the power of the study if the expected proportions of favorable response are the same as above, 
under balanced allocation and a two-sided significance level of 0.05?;
proc power;
	twosamplefreq test=pchi 
	groupproportions = (0.66 0.42)
	power = .
	groupns = ( 70 70);
run;






