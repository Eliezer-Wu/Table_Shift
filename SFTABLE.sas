%macro SFTABLE(_SL_DS =, _SL_GROUP =, _DATSRC =, _OUT = SFTABLE, _AVAL =, _BASE =, _PARAM_LIST =, _STATUS_LIST =, 
             _AP =, _BASE_MATCH_VAR =, _PERCENT_NEED = Y, _PERCENT_N = N1, _PERCENT_DEC = 1, _OVERALL_NEED = Y, _OVERALL_EXCLUDE= %str(), 
             _TOTAL_NEED = Y, _MISSING_NEED = Y);
*******************************************************************************************************************************************************;
*******************************************************************************************************************************************************;
*** _SL_DS          : A dataset with subjects and their post-baseline group information of each analysis timepoint combination needed for the table.***;
***                   Please refer to the corresponding table shell to determine which analysis timepoint and grouping variable                     ***;
***                   should be included in the dataset.                                                                                            ***;
***                   e.g.                                                                                                                          ***;
***                   SUBJID APERIOD APERIODC AVISITN AVISIT              TRTA                                                                      ***;
***                   01-001 1       Period 1 14      Visit 4 (Month 5)   DCA                                                                       ***;
***                   01-001 2       Period 2 16      Visit 6 (Month 10)  Placebo                                                                   ***;
***                   01-002 1       Period 1 14      Visit 4 (Month 5)   Placebo                                                                   ***;
***                   01-002 2       Period 2 16      Visit 6 (Month 10)  DCA                                                                       ***;
*** _SL_GROUP       : To indicate which variable contains group information, no synonym allowed (e.g. choose one from TRTA, TRTAN, but not both)    ***;
*** _DATSRC         : A dataset contains shift information, note that:                                                                              ***;
***                   1. _SL_GROUP must be one of its variable.                                                                                     ***;
***                   2. Keep ABLFL in the dataset for macro to identify the base record.                                                           ***;
*** _AVAL           : To indicate which variable in _DATSRC contains status information of each record (e.g. ANRIND, AVALCATz, AVAL, AVALC)         ***;
*** _BASE           : To indicate which variable in _DATSRC contains base status information of each record (e.g. BNRIND, BASECATz, BASE, BASEC)    ***;
*** _PARAM_LIST     : A dataset with list of parameter(s) which using the same status category with _STATUS_LIST                                    ***;
***                   e.g.                                                                                                                          ***;
***                   PARAMN  PARAMCD PARAM                                                                                                         ***;
***                   1       SYSBP   Systolic Blood Pressure (mmHg)                                                                                ***;
***                   2       DIABP   Diastolic Blood Pressure (mmHg)                                                                               ***;
***                   3       HR      Heart Rate (beats/min)                                                                                        ***;
***                   4       RESP    Respiratory Rate (breaths/min)                                                                                ***;
***                   5       TEMP    Temperature (C)                                                                                               ***;
***                   6       HEIGHT  Height (cm)                                                                                                   ***;
***                   7       WEIGHT  Weight (kg)                                                                                                   ***;
*** _STATUS_LIST    : A "|" seperated status list contains all possible status using by parameter(s) in _PARAM_LIST, usually con be found on CRF    ***;
***                   e.g. %str(LOW|NORMAL|HIGH)                                                                                                    ***;
***                   %str(Normal|Abnormal, NCS|Abnormal, CS)                                                                                       ***;
***                   %str(Below normal|Normal|Above normal)                                                                                        ***;
***                   %str(1|2|3|4|5|6|7)                                                                                                           ***;
*** _AP             : Analysis Praramter, should be variable(s) contains in _SL_DS, _DATSRC and their values should be common across all subject.   ***;
***                   e.g. %str(APHASEN APHASE PARAMN PARAMCD PARAM)                                                                                ***;
***                   at least one of PARAMN, PARAM, PARAMCD should exist                                                                           ***;
*** _BASE_MATCH_VAR : List of variable(s) used to merge base status.                                                                                ***;
***                   e.g. If all post-baseline records using the same baseline, %str(PARAM).                                                       ***;
***                        If each APERIOD has its own baseline, %str(APERIOD PARAM).                                                               ***;
*** _PERCENT_NEED   : _PERCENT_NEED takes value as Y or N (default as Y) to indicate whether percentage calulated based on N1,                      ***; 
***                   the number of patients with non-missing values at both baseline and post-baseline visit, is needed.                           ***;
*** _PERCENT_N      : _PERCENT_N takes value as N1 or N (default as N1) to indicate whether percentage is calulated based on                        ***; 
***                    N1 (the number of patients with non-missing values at both baseline and post-baseline visit, i.e.,  count of Total * Total)  ***;
***                    N  (the number of patients expected to have an assessment at the timepoint).                                                 ***;
***                   the number of patients with non-missing values at both baseline and post-baseline visit, is needed.                           ***;
*** _PERCENT_DEC    : indicate decimal of percentage, default as 1.                                                                                 ***;
*** _OVERALL_NEED   : _OVERALL_NEED takes value as Y or N (default as Y) to indicate whether a summary group "Overall" is needed.                   ***;
*** _TOTAL_NEED     : _TOTAL_NEED takes value as Y or N (default as Y) to indicate whether category "Total" is needed.                              ***;
*** _MISSING_NEED   : _MISSING_NEED takes value as Y or N (default as Y) to indicate whether category "Missing" is needed.                          ***;
*******************************************************************************************************************************************************;
*******************************************************************************************************************************************************;

*** input checking ***;
%if %bquote(&_SL_DS.         ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _SL_DS is not given, please provide for listing out subject and their group informarion (usually made from ADSL). Macro stop.;
    %return;
%end;
%if %bquote(&_SL_GROUP.      ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _SL_GROUP is not given, please provide to indicate group variable in SL_DS (usually TRTP/A, TRTSEQP/A). Macro stop.;
    %return;
%end;
%if %bquote(&_DATSRC.      ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _DATSRC is not given, please provide dataset for shift table (usually ADaM BDS dataset such as ADLB, ADVS...etc). Macro stop.;
    %return;
%end;
%if %bquote(&_AVAL.          ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _AVAL is not given, please provide to indicate record status variabe(e.g. ANRIND, AVALCATz, AVAL). Macro stop.;
    %return;
%end;
%if %bquote(&_BASE.          ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _BASE is not given, please provide to indicate base status variable(e.g. BNRIND, BASECATz, BASE). Macro stop.;
    %return;
%end;
%if %bquote(&_PARAM_LIST.    ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _PARAM_LIST is not given, please provide list of parameters. ;
    %put %str(WAR)NING: Note that provided parameters should be using the same category as _STATUS_LIST. Macro stop.;
    %return;
%end;
%if %bquote(&_STATUS_LIST.    ) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _STATUS_LIST is not given, please provide the list of all possible status seperated by '|' ;
    %put %str(WAR)NING: such as %sysfunc(catx(, "%", s, t, r))(Normal|Abnormal, NCS|Abnormal, CS) or %sysfunc(catx(, "%", s, t, r))(Low|Normal|High). ;
    %put %str(WAR)NING: Note that provided status should be the same with parameters listed in _PARAM_LIST used . Macro stop.;
    %return;
%end;

%if %bquote(&_AP.) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _AP is not given, please provide to indicate structure of table (eg. APERIOD AVISIT PARAMCD). Macro stop.;
    %return;
%end;
%if %bquote(&_BASE_MATCH_VAR.) eq %str() %then %do;
    %put %str(WAR)NING: Required parameter _BASE_MATCH_COND is not given, please provide to indicate variable(s);
    %put %str(WAR)NING: from _AP used to merge BASE (e.g. PARAMCD, APERIOD). Macro stop.;
    %return;
%end;

%if %bquote(%upcase(&_PERCENT_NEED.  )) eq %str(Y) %then %do;
%end;
%else %if %bquote(%upcase(&_PERCENT_NEED.  )) eq %str(N) %then %do;
%end;
%else %do;
    %put %str(WAR)NING: _PERCENT_NEED takes value as Y or N (default as Y). Macro stop.;
    %return;
%end;

%if %bquote(%upcase(&_PERCENT_N.  )) eq %str(N1) %then %do;
%end;
%else %if %bquote(%upcase(&_PERCENT_N.  )) eq %str(N) %then %do;
%end;
%else %do;
    %put %str(WAR)NING: _PERCENT_N takes value as N1 or N (default as N1). Macro stop.;
    %return;
%end;

%if %bquote(%upcase(&_OVERALL_NEED.  )) eq %str(Y) %then %do;
    %put If there is/are any group(s) specify in _GROUP you want to exclude from OVERALL, fill in _OVERALL_EXCLUDE, e.g. _OVERALL_EXCLUDE = %str(Placebo);
    %put If multiple, seperated by blank, e.g. _OVERALL_EXCLUDE = %str(Placebo DOSE1);
%end;
%else %if %bquote(%upcase(&_OVERALL_NEED.  )) eq %str(N) %then %do;
%end;
%else %do;
    %put %str(WAR)NING: _OVERALL_NEED takes value as Y or N (default as Y). Macro stop.;
    %return;
%end;

%if %bquote(%upcase(&_TOTAL_NEED.  )) eq %str(Y) %then %do;
%end;
%else %if %bquote(%upcase(&_TOTAL_NEED.  )) eq %str(N) %then %do;
%end;
%else %do;
    %put %str(WAR)NING: _TOTAL_NEED takes value as Y or N (default as Y). Macro stop.;
    %return;
%end;
%if %bquote(%upcase(&_MISSING_NEED.  )) eq %str(Y) %then %do;
%end;
%else %if %bquote(%upcase(&_MISSING_NEED.  )) eq %str(N) %then %do;
%end;
%else %do;
    %put %str(WAR)NING: _MISSING_NEED takes value as Y or N (default as Y). Macro stop.;
    %return;
%end;
*** done ***;

*** define local macro for later ***;

%macro GetVariablesList(_DS =, _MaVaName =, _type =);
%let _DS = %upcase(&_DS);
%if %bquote(&_type.  ) eq %str() %then %do;
proc sql noprint;
    select NAME, NAME into :&_MaVaName._LIST separated by ' ', :&_MaVaName._LIST_SQL separated by ', '
    from dictionary.columns
    where libname = "WORK" and memname = "&_DS"
    ;
quit;
%end;
%else %do;
proc sql noprint;
    select NAME, NAME into :&_MaVaName._LIST_&_TYPE separated by ' ', :&_MaVaName._LIST_&_TYPE._SQL separated by ', '
    from dictionary.columns
    where libname = "WORK" and memname = "&_DS" and type = "&_type"
    ;
quit;
%end;

%mend GetVariablesList;

%macro SQLCond(_VarList =, _deli = %str( ), _left = , _right =, _out =);
%local COND i;
%do i = 1 %to %sysfunc(countw(&_VarList., "&_deli."));

    %let ELEMENT = %upcase(%scan(&_VarList., &i, "&_deli."));

    %if %length(&COND) %then %do;
        %let COND = &COND and &_left..&ELEMENT. = &_right..&ELEMENT.;
    %end;
    %else %do;
        %let COND = &_left..&ELEMENT. =  &_right..&ELEMENT.;
    %end;

%end;
%let &_out = &COND;
%mend SQLCond;

%macro keep(_main =, _keep =, _out =);
%local MAINKEEP i j;
%do i = 1 %to %sysfunc(countw(&_main., " "));

    %let ELEMENT = %upcase(%scan(&_main., &i, " "));

    %do j = 1 %to %sysfunc(countw(&_keep., " "));

        %let ELEMENT2 = %upcase(%scan(&_keep., &j, " ")); 

        %if %str(&ELEMENT) = %str(&ELEMENT2) %then %do;
            %let MAINKEEP = &MAINKEEP %sysfunc(strip(&ELEMENT));
        %end;

    %end;

%end;
%let &_OUT = &MAINKEEP;
%mend keep;

%macro erase(_main =, _erase =, _out =);
%local newWordList wordCount i j currentWord;

%let wordCount = %sysfunc(countw(&_main));
%let wordsToRemoveCount = %sysfunc(countw(&_erase));

%do i = 1 %to &wordCount;

    %let currentWord = %qscan(&_main, &i, %str( ));
    %let removeWord = 0;

    %do j = 1 %to &wordsToRemoveCount;
        %if %upcase(&currentWord) eq %upcase(%qscan(&_erase, &j, %str( ))) %then %let removeWord = 1;
    %end;

    %if &removeWord ne 1 %then %let newWordList = &newWordList &currentWord;

%end;

%let newWordList = %qtrim(&newWordList); 
%let &_out = &newWordList;
%mend erase;

*** done ***;


*** dataset modification ***;
%local _SL_LIST _SL_LIST_SQL;
%GetVariablesList(_DS = &_SL_DS, _MaVaName = _SL);
%put &_SL_LIST;
%put &_SL_LIST_SQL;

data _SL;
    set &_SL_DS;
    %if %sysfunc(findw(&_SL_LIST, SUBJID)) %then %do;
    rename SUBJID = _SUBJ;
    %end;
    %if %sysfunc(findw(&_SL_LIST, USUBJID)) %then %do;
    rename USUBJID = _SUBJ;
    %end;
    rename &_SL_GROUP = _GROUP;
run;

%local _SL_M_LIST _SL_M_LIST_SQL;
%GetVariablesList(_DS = _SL, _MaVaName = _SL_M);
%put &_SL_M_LIST;
%put &_SL_M_LIST_SQL;

%local _SHIFT_LIST _SHIFT_LIST_SQL;
%GetVariablesList(_DS = &_DATSRC, _MaVaName = _SHIFT);
%put &_SHIFT_LIST;
%put &_SHIFT_LIST_SQL;
data _SHIFT01;
    set &_DATSRC;
    %if %sysfunc(findw(&_SHIFT_LIST, ANL01FL)) %then %do;
    if ANL01FL = "Y";
    %end;
    %if %sysfunc(findw(&_SHIFT_LIST, SUBJID)) %then %do;
    rename SUBJID = _SUBJ;
    %end;
    %else %if %sysfunc(findw(&_SHIFT_LIST, USUBJID)) %then %do;
    rename USUBJID = _SUBJ;
    %end;
    _AVAL = strip(&_AVAL);
    _BASE = strip(&_BASE);
    rename &_SL_GROUP = _GROUP;
run;
*** done ***;

*** create parameter-status dummy dataset ***;
data _PARAM_LIST;
    set &_PARAM_LIST;
run;
data _STATUS_LIST01;
    length _AVALN 8. _AVAL $200.;
    %let statusCount = %sysfunc(countw(&_STATUS_LIST, "|"));
    %do i = 1 %to &statusCount;
        %let ELEMENT = %scan(&_STATUS_LIST., &i, "|");
        _AVALN = &i; _AVAL = "&ELEMENT."; output;
    %end;
    _AVALN = &i    ; _AVAL = "Total"    ; output;
    _AVALN = &i + 1; _AVAL = "Missing"  ; output;
run;
proc sql noprint;
    create table _STATUS_LIST02 as
    select a.*, b._AVALN as _BASEN, b._AVAL as _BASE
    from _STATUS_LIST01 a, _STATUS_LIST01 b
    ;
quit;
proc sql noprint;
    create table _ps01 as
    select a.*, b.*
    from _PARAM_LIST a, _STATUS_LIST02 b
    ;
quit;

%local _PS_LIST_NUM _PS_LIST_NUM_SQL;
%GetVariablesList(_DS = _ps01, _MaVaName = _PS, _type = num);
%put &_PS_LIST_NUM;
%put &_PS_LIST_NUM_SQL;

proc sort data = _ps01 out = _ps02;
    by &_PS_LIST_NUM;
run;
*** done ***;

*** shift dataset with dummy visit ***;
data _SHIFT02;
    set _SHIFT01;
    keep _SUBJ &_AP _GROUP _AVAL _BASE ABLFL;
run;

proc sql noprint;
    create table _DUMMY_SHIFT01 as
    select a.*, b.*
    from _SL a, _PARAM_LIST b
    ;
quit;
proc sort data = _DUMMY_SHIFT01 out = _DUMMY_SHIFT02(keep = _SUBJ &_AP _GROUP) nodupkey;
    by _SUBJ &_AP _GROUP;
run;
    *** mrege ***;

%local _AVAL_COND;
%SQLCond(_VarList = %str(_SUBJ &_AP _GROUP), _deli = %str( ), _left = a, _right = b, _out =_AVAL_COND);
%put &_AVAL_COND;

%local _BASE_COND;
%SQLCond(_VarList = %str(_SUBJ &_BASE_MATCH_VAR), _deli = %str( ), _left = a, _right = c, _out =_BASE_COND);
%put &_BASE_COND;

%local _AP_LIST_NUM _AP_LIST_NUM_SQL;
%GetVariablesList(_DS = _DUMMY_SHIFT02, _MaVaName = _AP, _type = num);
%put &_AP_LIST_NUM;
%put &_AP_LIST_NUM_SQL;

proc sql noprint;
    create table _SHIFT03 as
    select a.*, b._AVAL, c._BASE
    from _DUMMY_SHIFT02 a 
    left join _SHIFT02 b                               on &_AVAL_COND
    left join _SHIFT02(where = (not missing(ABLFL))) c on &_BASE_COND
    order by _SUBJ, &_AP_LIST_NUM_SQL
    ;
quit;
    *** done ***;
*** done ***;

*** Total, Missing ***;
data _SHIFT04;
    set _SHIFT03;
    _AVAL_ORG = _AVAL;
    _BASE_ORG = _BASE;
    _AVAL = coalescec(_AVAL, "Missing");
    _BASE = coalescec(_BASE, "Missing");
    output;

    if cmiss(_AVAL_ORG, _BASE_ORG) = 0 then do;
        _AVAL = "Total"   ; _BASE = "Total"   ; output;
        _AVAL = _AVAL_ORG ; _BASE = "Total"   ; output;
        _AVAL = "Total"   ; _BASE = _BASE_ORG ; output;
    end;
    else if cmiss(_AVAL_ORG, _BASE_ORG) = 2 then do;

    end;
    else if missing(_AVAL_ORG) then do;
        _AVAL = "Missing" ; _BASE = "Total"   ; output;
    end;
    else if missing(_BASE_ORG) then do;
        _AVAL = "Total"   ; _BASE = "Missing" ; output;
    end;
run;
*** done ***;

*** count ***;
%let _FREQ_BY = %sysfunc(tranwrd(&_AP, %str( ), %str( * ))) * _GROUP * _AVAL * _BASE;
%put &_FREQ_BY;

proc freq data = _SHIFT04 noprint;
    table &_FREQ_BY / out = _SHIFT_COUNT01(drop  = PERCENT);
run;
    *** dummy ***;
proc sql noprint;
    create table _DUMMY_TABLE as
    select distinct a.*, b.*
    from _SHIFT_COUNT01(drop = _AVAL _BASE COUNT) a, _STATUS_LIST02 b
    order by &_AP_LIST_NUM_SQL, _AVALN, _BASEN
    ;
quit;

%local _COUNT_COND;
%SQLCond(_VarList = %str(&_AP _GROUP _AVAL _BASE), _deli = %str( ), _left = a, _right = b, _out =_COUNT_COND);
%put &_COUNT_COND;

proc sql noprint;
    create table _SHIFT_COUNT02 as
    select a.*, coalesce(b.COUNT, 0) as COUNT
    from _DUMMY_TABLE a left join _SHIFT_COUNT01 b
    on &_COUNT_COND
    order by &_AP_LIST_NUM_SQL, _AVALN, _BASEN
    ;
quit;
    *** done ***;
*** done ***;

*** overall ***;
%let _OVERALL_BY = %sysfunc(tranwrd(&_AP, %str( ), %str(, ))), _AVALN, _AVAL, _BASEN, _BASE;
%put &_OVERALL_BY;

%local _OVERALL_EXCLUDE_LIST;
%do i = 1 %to %sysfunc(countw(&_OVERALL_EXCLUDE., "|"));
    %let ELEMENT = %upcase(%scan(&_OVERALL_EXCLUDE., &i, "|"));
        %if %length(&_OVERALL_EXCLUDE_LIST) %then %do;
            %let _OVERALL_EXCLUDE_LIST = &_OVERALL_EXCLUDE_LIST, "&ELEMENT";
        %end;
        %else %do;
            %let _OVERALL_EXCLUDE_LIST = "&ELEMENT";
        %end;
%end;

%put &_OVERALL_EXCLUDE_LIST;

proc sql noprint;
    create table _SHIFT_COUNT02_OVERALL as
    select &_OVERALL_BY, "Overall" as _GROUP, sum(COUNT) as COUNT
%if %bquote(&_OVERALL_EXCLUDE.) eq %str() %then %do;
    from _SHIFT_COUNT02
%end;
%if %bquote(&_OVERALL_EXCLUDE.) ne %str() %then %do;
    from _SHIFT_COUNT02(where = (upcase(_GROUP) not in (&_OVERALL_EXCLUDE_LIST.)))
%end;
    group by &_OVERALL_BY
    ;
quit;
data _SHIFT_COUNT03;
    set _SHIFT_COUNT02 _SHIFT_COUNT02_OVERALL;

run;
*** done ***;

*** percent ***;
%if %bquote(%upcase(&_PERCENT_N.  )) eq %str(N1) %then %do;
data _SHIFT_DENO;
    set _SHIFT_COUNT03;
    where _AVAL = "Total" and _BASE = "Total";
run;
%end;
%if %bquote(%upcase(&_PERCENT_N.  )) eq %str(N)  %then %do;
data _SHIFT_N;
    set _SHIFT_COUNT03;
    where _AVAL in ("Total", "Missing") and _BASE in ("Total", "Missing");
run;

%let _AP_LIST_SQL = %sysfunc(tranwrd(&_AP, %str( ), %str(, )));
%put &_AP_LIST_SQL;

proc sql noprint;
    create table _SHIFT_DENO as
    select &_AP_LIST_SQL, _GROUP, sum(COUNT) as COUNT
    from _SHIFT_N
    group by &_AP_LIST_SQL, _GROUP
    ;
quit;
%end;

%local _DENO_COND;
%SQLCond(_VarList = %str(&_AP _GROUP), _deli = %str( ), _left = a, _right = b, _out =_DENO_COND);
%put &_DENO_COND;

proc sql noprint;
    create table _SHIFT_COUNT04 as
    select a.*, b.COUNT as DENO
    from _SHIFT_COUNT03 a left join _SHIFT_DENO b
    on &_DENO_COND
    order by &_AP_LIST_NUM_SQL, _GROUP, _AVALN, _BASEN
    ;
quit;
proc sql noprint;
    select max(int(log10(ifn(COUNT = 0, 1, COUNT))) + 1) into: _MAX_COUNT_DEC
    from _SHIFT_COUNT04
    ;
quit;
%put &_MAX_COUNT_DEC;
%local _DEC;
%let _DEC = %sysevalf(&_PERCENT_DEC. + 4 + 0.1 * &_PERCENT_DEC.);
%put &_DEC;
data _SHIFT_COUNT05;
    set _SHIFT_COUNT04;
    _MAX_COUNT_DEC = &_MAX_COUNT_DEC.;
    if DENO > 0 then do;
        if _BASE = "Missing" then COUNTX = putn(COUNT, _MAX_COUNT_DEC);
        else if _AVAL = "Missing" or  COUNT = 0 then COUNTX = putn(COUNT, _MAX_COUNT_DEC)||repeat(" ", &_PERCENT_DEC. + 6);
        else COUNTX = putn(COUNT, _MAX_COUNT_DEC)||" ("||put(COUNT / DENO * 100, &_DEC.)||")";
    end;
    if DENO = 0 then do;
    COUNTX = putn(COUNT, _MAX_COUNT_DEC);
    end;
run;

data _SHIFT_COUNT06;
    set _SHIFT_COUNT05;
    %if %bquote(%upcase(&_PERCENT_NEED.  )) eq %str(Y) %then %do;
    RESULT = COUNTX;
    %end;
    %if %bquote(%upcase(&_PERCENT_NEED.  )) eq %str(N) %then %do;
    RESULT = COUNT;
    %end;
    %if %bquote(%upcase(&_OVERALL_NEED.  )) eq %str(N) %then %do;
    if _GROUP = "Overall" then delete;
    %end;
    %if %bquote(%upcase(&_TOTAL_NEED.    )) eq %str(N) %then %do;
    if _AVAL = "Total" or _BASE = "Total" then delete;
    %end;
    %if %bquote(%upcase(&_MISSING_NEED.  )) eq %str(N) %then %do;
    if _AVAL = "Missing" or _BASE = "Missing" then delete;
    %end;
    drop COUNT COUNTX;
    rename DENO = &_PERCENT_N.;
run;
*** done ***;


*** transpose ***;
proc sort data = _SHIFT_COUNT06;
    by &_AP _GROUP &_PERCENT_N. _AVALN _AVAL _BASEN;
proc transpose out = &_OUT.(drop = _NAME_ rename = (_GROUP = &_SL_GROUP.));
    by &_AP _GROUP &_PERCENT_N. _AVALN _AVAL;
    id _BASE;
    var RESULT;
run;
*** done ***;

data &_OUT._RAW;
    set _SHIFT_COUNT04;
    rename DENO = &_PERCENT_N.;
run;

proc datasets library=work memtype=data nolist;
    delete _SHIFT: _STATUS: _SL _PS0: _PARAM_LIST _DUMMY_:;
run; quit;

%put output datasets: &_OUT. and &_OUT._RAW, check table shell for your next step.;

%mend SFTABLE;
