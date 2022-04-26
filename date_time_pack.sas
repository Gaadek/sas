/***********************************************************************************************************
Program Name			: date_time_pack.sas
Description				: Pack of useful functions to manage dates, times and datetimes
Instructions			: Refer to macro functions definitions
***********************************************************************************************************/

*-- Macro to extract a date value from a char and convert it to a real SAS date --*;
%macro conv_date(cvar, dvar, rule, nowarn);
/*
	Purpose:	Convert an input value (cvar) provided as text to a SAS date variable (dvar).
	
	cvar:		input variable, text value representing a date9 date (e.g. "01JAN2022")
				valid missing values for day are UN or UK
				valid missing value for month is UNK
	dvar:		output variable, conversion of cvar into a numeric value with a date format
	rule:		input parameter, either "min" or "max", to define how to handle partial dates
				* min: UNUNK2020 => 01JAN2021
				* max: UNUNK2020 => 31DEC2021
				Default is "min"
	nowarn:		disable warnings in case of invalid input value (i.e. text not missing and not representing a valid date)

	notes:		if cvar is missing (i.e. variable exists but content is null), then dvar will be missing as well
				the macro parses the cvar value to exclude anything that looks like a time value and accept any delimiter
				between day, month and year.
*/

	*-- Ensure CVAR is provided --*;
	%if %sysevalf(%superq(cvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter CVAR when calling conv_date;
		%let syscc = 99.;
		%return;
	%end;

	*-- Ensure DVAR is provided --*;
	%if %sysevalf(%superq(dvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter DVAR when calling conv_date;
		%let syscc = 99.;
		%return;
	%end;

	*-- Derivation rule for partial dates --*;
	%if %sysevalf(%superq(rule)=,boolean) %then %do;
		%let rule = min;
	%end;

	%if %lowcase(&rule.) ne min and  %lowcase(&rule.) ne max %then %do;
		%put ERROR: invalid value "&rule." for parameter RULE when calling conv_date;
		%let syscc = 99.;
		%return;
	%end;

	*-- Define level of log message --*;
	%if %sysevalf(%superq(nowarn)=,boolean) %then %do;
		%let logtyp = WARNING;
	%end;
	%else %do;
		%let logtyp = INFO;
	%end;

	*-- Avoid uninitialized var message --*;
	&dvar = .;

	if not missing(&cvar.) then do;
		attrib _tmp format=$50.;

		_tmp = upcase(&cvar.);

		*-- Keep only what looks like a date (i.e. remove time and spacers) --*;
		_tmp = prxchange("s/(\d{1,2}|[A-Za-z]{2})[^a-zA-Z0-9.]?([A-Za-z]{3})[^a-zA-Z0-9.]?(\d{4}).*/$1$2$3/oi", -1, _tmp);

		*-- Add leading 0 if needed --*;
		_tmp = prxchange("s/^(\d{1}[A-Za-z]{3}\d{4})$/0$1/oi", -1, _tmp);

		*-- Apply partial date rule --*;
		%if %lowcase(&rule.) = min %then %do;
			*-- Replace UNK by JAN for missing month value --*;
			_tmp = prxchange("s/UNK/JAN/oi", -1, strip(_tmp));

			*-- Replace UN/UNK by 01 for missing day value --*;
			_tmp = prxchange("s/^U[NK]/01/oi", -1, strip(_tmp));

			*-- Convert to date --*;
			&dvar. = input(_tmp, ?? date9.);
		%end;
		%else %do;
			*-- Replace UNK by DEC for missing month value --*;
			_tmp = prxchange("s/UNK/DEC/oi", -1, strip(_tmp));

			*-- Replace UN/UNK by last day of the month for missing day value --*;
			if prxmatch("/^U[NK].*/oi", strip(_tmp)) then do;
				*-- Replace UN/UK by 01 for missing day value --*;
				_tmp = prxchange("s/^U[NK]/01/oi", -1, strip(_tmp));

				*-- Convert to date --*;
				&dvar. = input(_tmp, ?? date9.);

				*-- Get the last day of the given month --*;
				if not missing(&dvar.) then do;
					&dvar. = intnx ('month', &dvar., 0, 'E');
				end;
			end;
			else do;
				*-- Convert to date --*;
				&dvar. = input(_tmp, ?? date9.);
			end;
		%end;

		if missing(&dvar.) then do;
			_tmp = strip("&logtyp.") || ": Input date is not valid: " || strip(&cvar.);
			put _tmp;
		end;

		drop _tmp;
	end;
%mend conv_date;


*-- Macro to extract a time value from a char and convert it to a real SAS time --*;
%macro conv_time(cvar, tvar, nowarn);
/*
	Purpose:	Convert an input value (cvar) provided as text to a SAS time variable (tvar).
	
	cvar:		input variable, text value representing a time (e.g. "01:25")
	tvar:		output variable, conversion of cvar into a numeric value with a time format
	nowarn:		disable warnings in case of invalid input value (i.e. text not missing and not representing a valid time)

	notes:		if cvar is missing (i.e. variable exists but content is null), then tvar will be missing as well
*/
	*-- Ensure CVAR is provided --*;
	%if %sysevalf(%superq(cvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter CVAR when calling conv_time;
		%let syscc = 99.;
		%return;
	%end;

	*-- Ensure TVAR is provided --*;
	%if %sysevalf(%superq(tvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter TVAR when calling conv_time;
		%let syscc = 99.;
		%return;
	%end;

	*-- Define level of log message --*;
	%if %sysevalf(%superq(nowarn)=,boolean) %then %do;
		%let logtyp = WARNING;
	%end;
	%else %do;
		%let logtyp = INFO;
	%end;

	*-- Avoid uninitialized var message --*;
	&tvar. = .;

	if not missing(&cvar.) then do;
		attrib _tmp format=$50.;

		_tmp = compress(scan(&cvar., -1, ' '));

		*-- Keep only what looks like a time --*;
		_tmp = prxchange("s/(\d+\:\d{2})(\:\d{2})?.*/$1$2/oi", -1, _tmp);

		*-- Add leading 0 if needed --*;
		_tmp = prxchange("s/^(\d{1}\:)/0$1/oi", -1, _tmp);

		&tvar. = input(_tmp, ?? time.);

		*-- Check is SAS rounded invalid times (e.g. 01:62 => 02:03) --*;
		if not missing(&tvar.) then do;
			if hour(&tvar.) ne input(scan(_tmp, 1, ':'), best.) then &tvar. = .;
			else if minute(&tvar.) ne input(scan(_tmp, 2, ':'), best.) then &tvar. = .;
		end;

		if missing(&tvar.) then do;
			_tmp = strip("&logtyp.") || ": Input time is not valid: " || strip(&cvar.);
			put _tmp;
		end;

		drop _tmp;
	end;
%mend conv_time;

*-- Macro to build a date time (dtvar) from a date var (dvar) and a time var (tvar) --*;
%macro build_datetime(dvar, tvar, dtvar, missing_time_allowed=yes);
/*
	Purpose:				Build a datetime variable from given date (dvar) and time (tvar).
	
	dvar:					input variable, numeric value representing a date (e.g. 01JAN2022)
	tvar:					input variable, numeric value representing a time (e.g. 10:22:45)
	dtvar:					output variable, conversion of dvar and tvar into a numeric value with a datetime format
	missing_time_allowed	input parameter to define how to handle missing time:
							if dvar is missing (i.e. variable exists but content is null), then dtvar will set to null in all cases
							if tvar is missing (i.e. variable exists but content is null) AND missing_time_allowed = yes, then time will be defaulted to 00:00:00
							if tvar is missing (i.e. variable exists but content is null) AND missing_time_allowed <> yes, then dtvar will be set to null
*/
	*-- Ensure DVAR is provided --*;
	%if %sysevalf(%superq(dvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter DVAR when calling build_datetime;
		%let syscc = 99.;
		%return;
	%end;

	*-- Ensure DTVAR is provided --*;
	%if %sysevalf(%superq(dtvar)=,boolean) %then %do;
		%put ERROR: missing mandatory parameter DTVAR when calling build_datetime;
		%let syscc = 99.;
		%return;
	%end;

	*-- Avoid uninitialized var message --*;
	&dtvar. = .;

	*-- Date must be provided in all cases --*;
	if not missing(&dvar.) then do;
		*-- In case of time variable not provided --*;
		%if %sysevalf(%superq(tvar)=,boolean) %then %do;
			%if %lowcase(&missing_time_allowed.) = yes %then %do;
				*-- If missing time is allowed, replace missing time --*;
				&dtvar. = dhms(&dvar., 0, 0, 0);
			%end;
			%else %do;
				*-- If missing time is not allowed, set datetime value to missing --*;
				&dtvar. = .;
			%end;
		%end;
		%else %do;
			if not missing(&tvar.) then	&dtvar. = dhms(&dvar., 0, 0, &tvar.);

			%if %lowcase(&missing_time_allowed.) = yes %then %do;
				else						&dtvar. = dhms(&dvar., 0, 0, 0);
			%end;
			%else %do;
				else						&dtvar. = .;
			%end;
		%end;
	end;
%mend build_datetime;
