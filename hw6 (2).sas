
* 1. MH test test association controlling for center;
data diab;
	input center $ trmt $ response $ count @@;
	cards;
A new F 54 A new U 46
A standard F 48 A standard U 52
B new F 30 B new U 20
B standard F 22 B standard U 28
;

title 'Question 1';
ods select CrossTabFreqs CMH MantelFleiss;
proc freq data=diab order=data;
	weight count;
	table center*trmt*response / 
		 nocol nopct cmh(mf); *MH statistic;
run;
title;

* 2. OR in center A;
data A;
	input trmt $ response $ count;
	cards;
new F 54 
new U 46
standard F 48 
standard U 52
;

title 'Question 2';
ods select CrossTabFreqs RelativeRisks;
proc freq data=A order=data;
	weight count;
	table trmt*response/
		nocol nopct expected measures;
run;
title; 

* 3. Prop diff in center B;
data B;
	input trmt $ response $ count;
	cards;
new F 30 
new U 20
standard F 22
standard U 28
;

title 'Question 3';
ods trace on;
*ods select CrossTabFreqs;
proc freq data=B order=data;
	weight count;
	table trmt*response/
		nocol nopct expected riskdiff (correct);
run;
title;

* 4. Determine sample size per group;
* A cohort study is being conducted to compare the incidence of a specific event between two groups: 
Group A and Group B. 
The study is designed with a 1:2 allocation ratio for Group A: Group B. 
The proportions of the event in Group A and Group B are given as 𝜋𝐴 = 0.19 and 𝜋𝐵 = 0.34, respectively. 
With a two-sided significance level of 0.05 and a power of 0.80
determine the required sample size for each group.;
title 'Question 4';
proc power;
	twosamplefreq test=pchi
	groupproportions= (0.19 0.34)
	power = 0.8
	groupweights = (1 2)
	ntotal=.;
run;
title;

* 5. Matched pairs Mcnemar test, 
test if proportions between dependent groups are equal;
data cream;
	input placebo $ new $ count;
	cards;
good good 25 
good poor 15
poor good 30 
poor poor 30 
;
run;
 
title 'Question 5a';
ods select McnemarsTest;
proc freq data=cream order=data;
	weight count;
	table new*placebo / 
		nocol  nopct agree chisq cmh(mf); *MH statistic;
run;
title;

* 5b;
    data ind;
       set cream;
       retain id 0;
       do id=id+1 to id+count;
         factor=new; response='new'; output;
         factor=placebo; response='placebo'; output;
       end;
       keep id factor response;
       run;

title 'Question 5b';      
      proc logistic data=ind;
        strata id;
        class factor (ref='poor') / param=ref;
        model response(event='new') = factor;
        exact factor / estimate=odds;
    run;
title;  

* 10. Proportional odds model for ordinal satisfaction response;
data fit;
	input dept $ level $ response $ count @@;
	cards;
A junior very 72  A junior some 30  A junior not 20
A mid very 119  A mid some 50  A mid not 25
A senior very 62  A senior some 45  A senior not 42  
M junior very 75  M junior some 26  M junior not 28 
M mid very 56 M mid some 34  M mid not 22
M senior very 27  M senior some 52  M senior not 34
;
run;

title 'Question 10';
proc freq data=fit order=data;
	weight count;
	table dept*level*response / nocol nopercent;
run;

proc logistic data=fit order=data; *order as in data, not response first (alpha);
	freq count;
	class dept(ref='A') level(ref='junior') / param=reference;
	model response(ref='very') = dept level 
	/ scale=none aggregate; * unequalslopes=level;
run; 
title;

* 11 Test for trend in proportion of any satisfaction;
data trend;
	input level $ response $ count @@;
	cards;
junior no 20 junior yes 102
mid no 25 mid yes 169
senior no 42 senior yes 107
;
run;

title 'Question 11';
proc freq data=trend order=data;
	weight count;
	table level*response / trend;
run;
title;

* 12 Test for trend controlling for department; 
* MH correlation;
data trend2;
	input dept $ level $ response $ count @@;
	cards;
A junior no 20 A junior yes 102
A mid no 25 A mid yes 169
A senior no 42 A senior yes 107
M junior no 28 M junior yes 101
M mid no 22 M mid yes 90
M senior no 34 M senior yes 79 
;
run;

title 'Question 12';
proc freq data=trend2 order=data;
	weight count;
	table dept*level*response / cmh trend;
run; 
title;


