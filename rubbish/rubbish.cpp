// rubbish.cpp : Defines the entry point for the application.
//

#include "rubbish.h"
#include "defs.h"
#include "library.h"

using namespace std;
using namespace rubbish;

/*
//Program to compute sum of 1..n for some n
pushimm "Enter a number:"
call "printLn"
pop
call "getOneInt"
storevar "n"
loadvar "n"
loadvar "n"
pushimm 1
add
mul
pushimm 2
div
call "printLn"
pop
ret

*/

int main()
{
	Value v0;
	v0.type = ValueType::VAL_STRING;
	v0.value.stringVal = "Enter a number!";

	Value v1;
	v1.type = ValueType::VAL_STRING;
	v1.value.stringVal = "printLn";

	Value v2;
	v2.type = ValueType::VAL_STRING;
	v2.value.stringVal = "getOneInt";

	Value v3;
	v3.type = ValueType::VAL_STRING;
	v3.value.stringVal = "n";

	Value v4;
	v4.type = ValueType::VAL_INTEGER;
	v4.value.integerVal = 2;

	Value v5;
	v5.type = ValueType::VAL_INTEGER;
	v5.value.integerVal = 1;


	Instruction i1(InstructionType::INST_PUSHIMM, v0);
	Instruction i2(InstructionType::INST_CALL, v1);
	Instruction i3(InstructionType::INST_POP);
	Instruction i4(InstructionType::INST_CALL, v2);
	Instruction i5(InstructionType::INST_STOREVAR, v3);
	Instruction i6(InstructionType::INST_LOADVAR, v3);
	Instruction i7(InstructionType::INST_LOADVAR, v3);
	Instruction i8(InstructionType::INST_PUSHIMM, v5);
	Instruction i9(InstructionType::INST_ADD);
	Instruction i10(InstructionType::INST_MUL);
	Instruction i11(InstructionType::INST_PUSHIMM, v4);
	Instruction i12(InstructionType::INST_DIV);
	Instruction i13(InstructionType::INST_CALL, v1);
	Instruction i14(InstructionType::INST_POP);
	Instruction i15(InstructionType::INST_RET);





	FunctionInfo info;
	info.name = "main";
	info.arity = 0;
	info.instructions.push_back(i1);
	info.instructions.push_back(i2);
	info.instructions.push_back(i3);
	info.instructions.push_back(i4);
	info.instructions.push_back(i5);
	info.instructions.push_back(i6);
	info.instructions.push_back(i7);
	info.instructions.push_back(i8);
	info.instructions.push_back(i9);
	info.instructions.push_back(i10);
	info.instructions.push_back(i11);
	info.instructions.push_back(i12);
	info.instructions.push_back(i13);
	info.instructions.push_back(i14);
	info.instructions.push_back(i15);

	info.type = FunctionType::FUNC_RUBBISH;

	Context context;
	context.RegisterFunction(&info);
	LoadLibrary(context);
	context.LoadFunctionForExecution("main");
	context.ExecuteAll();

	int x;
	cin >> x;
	return 0;
}
