data ds_in_01;
	attrib	subject format=$10. 
			subjectid format=8.
	;
	
	*-- Sample data --*;
	infile datalines delimiter=',';
	input subject $ subjectid;
	datalines;
		0001,1
		0002,2
	;
run;
