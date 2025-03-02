* 1a) i.
 rxc contingency table;
* General association test; 

*     ii.
Location shift in response level by education;
data survey;
	length edu $15;
	input edu $ response $ count @@;
	cards;
college no 61 college neutral 67 college yes 129
highschool no 120 highschool neutral 66 highschool yes 70
else no 43 else neutral 50 else yes 76
;
run; 

proc freq data=survey order=data;
	weight count;
	table edu*response / expected 
	plots=freqplot(twoway=cluster) chisq cmh nocol nopct;
run;


*     iii.
Progressive location shift between 
increasing level of education and level of response;
proc freq data=survey order=data;
	weight count;
	table edu*response / scores=rank norow nocol nopct jt;
run;



* 1. b Non-zero correlation association between ordinal vars,
 adjusting for region nominal var (strata);
/*Under minimal assumptions and treating region as a nominal variable, conduct a statistical
test to understand whether ordinal education level is associated with ordinal response, after
controlling for region.*/
data survey2;
	length edu response $15;
	input edu $ region $ response $ count @@;
	cards;
college west no 20 
college west neutral 17
college west yes 50
college midwest no 15 
college midwest neutral 20 
college midwest yes 24
college east no 26 
college east neutral 30 
college east yes 55
highschool west no 46 
highschool west neutral 25 
highschool west yes 25
highschool midwest no 21 
highschool midwest neutral 21 
highschool midwest yes 20
highschool east no 53 
highschool east neutral 20 
highschool east yes 25
else west no 15 
else west neutral 18 
else west yes 32
else midwest no 13 
else midwest neutral 12 
else midwest yes 16
else east no 15 
else east neutral 20 
else east yes 28
;
run;

proc freq data=survey2 order=data;
	weight count;
	tables region*edu*response 
		/ nocol norow nopct cmh; *equally spaced scores;
	*tables region*edu*response 
		/ nocol norow nopct cmh scores=modridit; *unequal spaced scores (modified ridit);
run;


* 2 Exact test of correlation, association 
b/t 2 ordinal variables;
data drug;
	length dose $10	response $10;
	input dose $ response $ count @@;
	cards;
placebo none 8
placebo some 2
placebo marked 0
20mg none 7
20mg some 2
20mg marked 2
40mg none 3
40mg some 3
40mg marked 4
80mg none 0
80mg some 4
80mg marked 6
;
run;

proc freq data=drug order=data;
	weight count;
	table dose*response /
	norow nocol nopercent measures cl;
		exact mhchi scorr;
run;
	

* 3 Test of observer agreement;
* Obsever 1 vs. 2 categorical scores, 
test how much they agree;
data disease;
	input obs1 $ obs2 $ count @@;
	cards;
mild mild 35 
mod mild 20
sev mild 6
vsev mild 3 
mild mod 10
mod mod 32
sev mod 13 
vsev mod 7 
mild sev 1
mod sev 13
sev sev 24 
vsev sev 8 
mild vsev 0
mod vsev 4
sev vsev 6 
vsev vsev 8
;
run; 

proc freq data=disease;
	weight count;
	table obs1*obs2 /norow nocol nopct agree;
	test kappa wtkap;
run;




