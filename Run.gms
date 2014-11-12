*Convert input data from excel spredsheet to Gdx file --> ignore this if GDX already up-to-date
*$call "gdxxrw i=Inputs_2013.xlsm o=AUFLS_2013.gdx  index=index!a1"
*$Stop

Files
temp
Setting      / "Setting.inc" /;
Setting.lw = 0 ;
Setting.sw = 0 ;

put Setting;
put "$setglobal  SolveType    'Flexible'" /;
put "$setglobal  InputFile    'AUFLS_2012'" /;
put "$setglobal  InputFileX   'Result_Clean_2011'" /;
put "$setglobal  OutputFile   'Result_Flexible_2012'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveFlexible2012.lst'


$ontext
put Setting;
put "$setglobal  SolveType    'Flexible'" /;
put "$setglobal  InputFile    'AUFLS_2013'" /;
put "$setglobal  InputFileX   'Result_Flexible_2012'" /;
put "$setglobal  OutputFile   'Result_Flexible_2013'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveFlexible2013.lst'


put Setting;
put "$setglobal  SolveType    'Resolve'" /;
put "$setglobal  InputFile    'AUFLS_2012'" /;
put "$setglobal  InputFileX   'Result_Clean_2011'" /;
put "$setglobal  OutputFile   'Result_Resolve_2012'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveResolve2012.lst'


put Setting;
put "$setglobal  SolveType    'Resolve'" /;
put "$setglobal  InputFile    'AUFLS_2013'" /;
put "$setglobal  InputFileX   'Result_Resolve_2012'" /;
put "$setglobal  OutputFile   'Result_Resolve_2013'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveResolve.lst'


put Setting;
put "$setglobal  SolveType    'Clean'" /;
put "$setglobal  InputFile    'AUFLS_2012'" /;
put "$setglobal  OutputFile   'Result_Clean_2012'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveClean2012.lst'


put Setting;
put "$setglobal  SolveType    'Clean'" /;
put "$setglobal  InputFile    'AUFLS_2013'" /;
put "$setglobal  OutputFile   'Result_Clean_2013'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveClean2013.lst'
$offtext


put Setting;
put "$setglobal  SolveType    'Nosolve'" /;
put "$setglobal  InputFile    'AUFLS_2012'" /;
put "$setglobal  InputFileX   'Result_Clean_2011'" /;
put "$setglobal  OutputFile   'Performace_2012_2011Base'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveFlexible2012.lst'


put Setting;
put "$setglobal  SolveType    'Nosolve'" /;
put "$setglobal  InputFile    'AUFLS_2013'" /;
put "$setglobal  InputFileX   'Result_Clean_2011'" /;
put "$setglobal  OutputFile   'Performace_2013_2011Base'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveFlexible2013.lst'


put Setting;
put "$setglobal  SolveType    'Nosolve'" /;
put "$setglobal  InputFile    'AUFLS_2013'" /;
put "$setglobal  InputFileX   'Result_Flexible_2012'" /;
put "$setglobal  OutputFile   'Performace_2013_2012Base'" /;
putclose;
* Solve the model for the current input file
put_utility temp 'exec' / 'gams RunSolve' ;
* Copy the vSPDsolve.lst file to i_fileName.lst in ..\Programs\lst\
put_utility temp 'shell' / 'copy Solve.lst SolveFlexible2013.lst'


