// rubbish.cpp : Defines the entry point for the application.
//

#include "rubbish.h"
#include "defs.h"
using namespace std;
using namespace rubbish;

int main()
{
	ValueValue v1;
	v1.integerVal = 5;
	
	ValueValue v2;
	v2.integerVal = 6;

	ValueValue v0;
	v0.stringVal = "main";

	Value val1(ValueType::VAL_INTEGER, v1);
	Value val2(ValueType::VAL_INTEGER, v2);
	Value val0(ValueType::VAL_STRING, v0);

	//Instruction i0(InstructionType::INST_CALL, val0);
	Instruction i1(InstructionType::INST_PUSHIMM, val1);
	Instruction i2(InstructionType::INST_PUSHIMM, val2);
	Instruction i3(InstructionType::INST_ADD, val2);
	Instruction i4(InstructionType::INST_RET, val2);

	
	FunctionInfo info;
	info.name = "main";
	info.arity = 0;
	info.instructions.push_back(i1);
	info.instructions.push_back(i2);
	info.instructions.push_back(i3);
	info.instructions.push_back(i4);

	info.type = FunctionType::FUNC_RUBBISH;

	Context context;
	context.RegisterFunction(&info);
	context.LoadFunctionForExecution("main");
	context.ExecuteAll();

	std::cout << context.dataStack.top().value.integerVal;
	int x;
	cin >> x;
	return 0;
}
