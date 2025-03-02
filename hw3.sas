
* 1. MH test and measures of association (OR);
data rash;
	input center $ trmt $ response $ count @@;
	num = (response='y'); *numeric code 1=Y, code 0=N;
	cards;
A active y 52 A active n 26
A placebo y 35 A placebo n 40
B active y 45 B active n 34
B placebo y 40 B placebo n 38
;

proc freq data=rash order=data;
	weight count;
	table center*trmt*response / 
		nocol nopct chisq cmh(mf) measures; *MH statistic, OR; 
	*per center, Mantel-Haenszel (Mantel-Fleiss criterion) test;
run;

* 2. Fit a main effects logistic regression;
* compare homogeneity of odds ratio using GOF H-L chi square test;
proc logistic data=rash order=data;
	freq count;
	class center(ref='B') trmt(ref='placebo') / param=ref; * assign ref groups and reference cell-coding;
	model response(event='y')= center trmt
		/scale=none aggregate lackfit; *residual chi-squared statistic;
	*exact trmt / estimate=both;
run;


*********************************************
* 3. Row means Score test adjusted for stratified variable;
data policy;
	input region $ health $ response $ count @@;
	cards;
west F disagree 26  west F neutral 14  west F agree 50  
west U disagree 58  west U neutral 53  west U agree 45
mid F disagree 15  mid F neutral 15  mid F agree 22
mid U disagree 23  mid U neutral 35  mid U agree 40
east F disagree 33  east F neutral 28  east F agree 37
east U disagree 73  east U neutral 58  east U agree 49
;

proc freq data=policy order=data;
	weight count;
	*table region*health*response / nopercent norow nocol cmh2 scores=rank;
	*table region*health*response / nopercent norow nocol cmh2;
	table region*health*response / nopercent norow nocol cmh2 scores=modridit all;
	table health*response 
		/ cmh chisq measures trend;	
	table region*health*response
		/ cmh chisq measures trend;	
run;

/*
data policy_num;
	input region $ health $ response count @@;
	cards;
west F 0 26  west F 1 14  west F 2 50  
west U 0 58  west U 1 53  west U 2 45
mid F 0 15  mid F 1 15  mid F 2 22
mid U 0 23  mid U 1 35  mid U 2 40
east F 0 33  east F 1 28  east F 2 37
east U 0 73  east U 1 58  east U 2 49
;

* effect size;
proc glm data=policy_num;
	class region health;
	freq count;
	model response = region health;
	estimate 'diff' health -1 1;
run;
*/

* 4. Fit a proportional odds regression model
for ordinal agreement level without Mid region;
data fit;
	input region $ health $ response $ count @@;
	cards;
west F agree 50  west F neutral 14  west F disagree 26
west M agree 30  west M neutral 35  west M disagree 48
west U agree 15  west U neutral 18  west U disagree 10  
east F agree 37  east F neutral 28  east F disagree 33 
east M agree 28  east M neutral 34  east M disagree 53
east U agree 21  east U neutral 24  east U disagree 20
;

proc logistic data=fit order=data;
	freq count;
	class region(ref='east') health(ref='F') / param=reference;
	model response(ref='agree') = region health 
	/ scale=none aggregate; * unequalslopes=health;
run; 






