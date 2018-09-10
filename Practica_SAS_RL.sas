options validvarname=any;

ods listing close;

ods listing gpath='/home/jampol710/my_courses/RafaelLucena/data_input2';

libname lib '/home/jampol710/my_courses/RafaelLucena/data_input2';

/* Visualizacion de los datos para ver missings */
data bank;
	set LIB.BANK_ADDITIONAL_FULL;
    proc means data=bank n nmiss;
run;


/*Analisis exploratorio de las variables*/
/*
proc univariate data=bank normal plot;
 var _numeric_;
 qqplot _numeric_ / NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM /NORMAL(COLOR=MAROON W=4) CFILL = BLUE CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2; 
run;
*/

data bank2;
set LIB.BANK_ADDITIONAL_FULL(rename=('emp.var.rate'n=emp_var_rate 'cons.price.idx'n=cons_price_idx 'cons.conf.idx'n=cons_conf_idx 'nr.employed'n=nr_employed));
run;


data bank2(drop=y campaign duration default age month pdays previous);
set bank2;

/*Hacemos binaria y numerica la variable dependiente*/

	yN = 1;
	if (y='no') then yN=0;


/* Restamos uno a campaign y tramificamos */

	campaignT = campaign - 1;
	if (campaignT>4) then campaignT = 5;


/*Tramificamos age (continua)*/

if (age<25) then ageT = 1;
	if (age>=25) and (age<30) then ageT = 2;
	if (age>=30) and (age<35) then ageT = 3;
	if (age>=35) and (age<40) then ageT = 4;
	if (age>=40) and (age<45) then ageT = 5;
	if (age>=45) and (age<50) then ageT = 6;
	if (age>=50) and (age<60) then ageT = 7;
	if (age>=60) then ageT = 8;


/* tramificamos los meses del año por cuatrimestres */

	if (month='jan') then quarter = 1;
	if (month='feb') then quarter = 1;
	if (month='mar') then quarter = 1;
	if (month='apr') then quarter = 2;
	if (month='may') then quarter = 2;
	if (month='jun') then quarter = 2;
	if (month='jul') then quarter = 3;
	if (month='aug') then quarter = 3;
	if (month='sep') then quarter = 3;
	if (month='oct') then quarter = 4;
	if (month='nov') then quarter = 4;
	if (month='dec') then quarter = 4;
	

/* tramificamos pdays */

	if (pdays<7) then pdaysT = 1;
	if (pdays>=7) and (pdays<14) then pdaysT = 2;
	if (pdays>=14) and (pdays<21) then pdaysT = 3;
	if (pdays>=21) and (pdays<28) then pdaysT = 4;
	if (pdays>=28) then pdaysT = 5;
	
	
/*Hacemos binaria la variable previous*/

	previousT = 1;
	if (previous=0) then previousT = 0;
	
run;



	
/*Dummificacion de variables utilizando el procedimiento glmmod*/
/*
proc glmmod data=bank2;
	class job--previousT;
	model yN= job marital education housing loan contact day_of_week poutcome emp_var_rate cons_conf_idx euribor3m nr_employed campaignT ageT quarter pdaysT previousT;
	ods output designpoints=bankDummy;

run;

data lib.bankDummy;
set bankDummy;
run;

*/

/*Seleccion de variables mediante Miner*/
/*Seleccion de variables mediante arbol de decision*/
/*proc hpsplit data=bank maxbranch=5 maxdepth=10 leafsize=6;
    target y;
    input job marital education housing loan contact day_of_week poutcome ageT campaignT pdaysT previousT quarter / level=nominal;
    input  'cons.price.idx'n 'cons.conf.idx'n euribor3m / level=interval;
    output importance=import;
    prune none;
run;*/




/* Ejecutamos la macro y obtenemos los resultados de la misma*/

/*Macro seleccion modelo: Procedimiento Logistico
t_input  = Tabla Input
vardepen = Variable Dependiente
varindep = Variable(s) Independiente(s)
interaccion  = Variable(s) que interaccionan
semi_ini = Valor Inicial de la semilla
semi_fin = Valor Final de la semilla 
 */

%macro logistic (t_input, vardepen, varindep, interaccion, semi_ini, semi_fin );
ods trace on /listing;
%do semilla=&semi_ini. %to &semi_fin.;

 ods output EffectInModel= efectoslog;/*Test de Wald de efectos en el modelo*/
 ods output FitStatistics= ajustelog; /*"Estadisticos de ajuste", AIC */
 ods output ParameterEstimates= estimalog;/*"Estimadores de parametro"*/
 ods output ModelBuildingSummary=modelolog; /*Resumen modelo, efectos*/
 ods output RSquare=ajusteRlog; /*R-cuadrado y Max-rescalado R-cuadrado*/


proc logistic data=&t_input.  EXACTOPTIONS (seed=&semilla.) ;
	class  &varindep. / param=reference;
	model &vardepen. = &varindep.
						  / selection=stepwise details rsquare NOCHECK;
run;

 data un1; i=12; set efectoslog; set ajustelog; point=i; run;
 data un2; i=12; set un1; set estimalog; point=i; run;
 data un3; i=12; set un2; set modelolog; point=i; run;
 data union&semilla.; i=12; set un3; set ajusteRlog; point=i; run;

 proc append  base=t_models  data=union&semilla.  force; run;
 proc sql; drop table union&semilla.; quit; 

%end;
ods html close; 
proc sql; drop table efectoslog,ajustelog,ajusteRlog,estimalog,modelolog; quit;

%mend;



%logistic (lib.bankDummy, yN, 'job admin.'n--'previousT 1'n , , 12345, 12350);



/*Analisis de los resultados obtenidos de la macro*/
proc freq data=t_models (keep=effect ProbChiSq);  tables effect*ProbChiSq /norow nocol nopercent; run;
proc sql; select distinct * from t_models (keep=effect nvalue1 rename=(nvalue1=RCuadrado)) order by RCuadrado desc; quit;
proc sql; select distinct * from t_models (keep=effect StdErr) order by StdErr; quit;

/*Tabla de clasificacion*/
proc freq data = bank_dummy; tables Yn*emp_var_rate /norow nocol nopercent relrisk; run;
proc freq data = salarial; tables   /norow nocol nopercent relrisk;  run;
proc freq data = salarial; tables  /norow nocol nopercent relrisk;  run;

ods graphics on;

/*Tabla de sensibilidad y especificidad para distintos puntos de corte y Curva ROC*/
proc logistic data=lib.bankDummy desc  PLOTS(MAXPOINTS=NONE); 
 model yN = 'emp_var_rate -3'n--'emp_var_rate -3.4'n 'pdaysT 5'n 'quarter 1'n--'quarter 4'n /ctable pprob = (.05 to 1 by .05)  outroc=roc;
run;


/*Seleccion del 10% de los mejores clientes*/

proc logistic data=lib.bank4 desc  PLOTS(MAXPOINTS=NONE); 
 model yN = 'emp_var_rate'n 'pdaysT'n 'quarter'n /ctable pprob = (.05 to 1 by .05);
 output out=bank_predicted p=contratado_predicted;
run;

proc sort data=bank_predicted out=bank_predicted_ordenado;
by descending contratado_predicted;
run;

proc sql;
create table seleccion10 as select * from bank_predicted_ordenado (OBS=4118);
quit;


/*Seleccion aleatoria del 5% */


proc surveyselect data=lib.bank4 out=seleccion5 seed=12345 
method=srs rate=.05;	
run;


data bankDefinitiva;
set seleccion10 seleccion5;
run;






	





