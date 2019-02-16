//Rubbish bytecode parser. Currently only for the human readable stuff.

//Example rubbish bytecode file
/*
#topfunction "main" //function that starts execution
#function "main"
pushimm 5
pushimm 6
add
call "print" //print is a stdlib function
#endfunction
*/
/* Grammar
Line = TopFunctionDirective | FunctionDirective | Operation
TopFunctionDirective =  #topfunction String
FunctionDirective = #function String | #endfunction
Operation = pushimm Value | pop | call String | ret | add | sub | mul | div | nop | storeval String | loadval String
Value = Integer | Double | String
*/

#include <cstring>