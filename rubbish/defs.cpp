#include "defs.h"

rubbish::Value::Value() : type(ValueType::VAL_INVALID) {
}

rubbish::Value::Value(ValueType valueType, const ValueValue& value) 
	: type(valueType), value(value) {
}

rubbish::Instruction::Instruction() : type(InstructionType::INST_NOP) {
}

rubbish::Instruction::Instruction(InstructionType instructionType) 
	: type(instructionType) {
}

rubbish::Instruction::Instruction(InstructionType instructionType, const Value & value) 
	: type(instructionType), value(value) {
}

void rubbish::Context::RegisterFunction(FunctionInfo * functionInfo) {
	functionStore[functionInfo->name] = functionInfo;
}

void rubbish::Context::RegisterNativeFunction(std::string name, int arity, std::function<bool(std::stack<Value>&)> func) {
	FunctionInfo* funcInfo = new FunctionInfo;
	funcInfo->name = name;
	funcInfo->arity = arity;
	funcInfo->type = FunctionType::FUNC_NATIVE;
	funcInfo->nativeFunction = func;
	RegisterFunction(funcInfo);
}

void rubbish::Context::ExecuteInstruction(const Instruction & instr) {
	switch (instr.type) {

	case InstructionType::INST_STOREVAR:
		ExecuteStoreVar(instr);
		break;

	case InstructionType::INST_LOADVAR:
		ExecuteLoadVar(instr);
		break;

	case InstructionType::INST_CALL:
		ExecuteCall(instr);
		break;

	case InstructionType::INST_RET:
		ExecuteRet(instr);
		break;

	case InstructionType::INST_PUSH:
		ExecutePush(instr);
		break;

	case InstructionType::INST_POP:
		ExecutePop(instr);
		break;

	case InstructionType::INST_ADD:
		ExecuteAdd(instr);
		break;

	case InstructionType::INST_SUB:
		ExecuteSub(instr);
		break;

	case InstructionType::INST_MUL:
		ExecuteMul(instr);
		break;

	case InstructionType::INST_DIV:
		ExecuteDiv(instr);
		break;

	case InstructionType::INST_NOP:
		ExecuteNop(instr);
		break;

	case InstructionType::INST_JNZ:
		ExecuteJnz(instr);
		break;

	case InstructionType::INST_JZ:
		ExecuteJz(instr);
		break;

	case InstructionType::INST_JMP:
		ExecuteJmp(instr);
		break;
	}
}

void rubbish::Context::LoadFunctionForExecution(std::string funcName) {
	auto found = functionStore.find(funcName);
	if (found != functionStore.end()) {
		auto fInfo = found->second;

		switch (fInfo->type) {
		case FunctionType::FUNC_NATIVE:
			fInfo->nativeFunction(dataStack);
			break;

		case FunctionType::FUNC_RUBBISH:
			auto framePtr = new Frame;
			framePtr->functionInfo = fInfo;
			framePtr->instPtr = 0;
			callStack.push(framePtr);
		}
	}
	else {
		throw new std::exception;
	}
}

void rubbish::Context::LoadInternalTopLevelFunction() {
	LoadFunctionForExecution(topFunctionName);
}

void rubbish::Context::ExecuteStoreVar(const Instruction & instr) {
	auto frame = callStack.top();
	auto varVal = dataStack.top();
	frame->dataStore[instr.value.value.stringVal] = varVal;
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteLoadVar(const Instruction & instr) {
	auto frame = callStack.top();
	auto found = frame->dataStore.find(instr.value.value.stringVal);
	if (found != frame->dataStore.end()) {
		dataStack.push(found->second);
	}
	else {
		throw new std::exception;
	}
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteCall(const Instruction & instr) {
	auto frame = callStack.top();
	auto found = functionStore.find(instr.value.value.stringVal);
	if (found != functionStore.end()) {
		auto fInfo = found->second;
		
		switch (fInfo->type) {
		case FunctionType::FUNC_NATIVE:
			fInfo->nativeFunction(dataStack);
			break;

		case FunctionType::FUNC_RUBBISH:
			auto framePtr = new Frame;
			framePtr->functionInfo = fInfo;
			framePtr->instPtr = 0;
			callStack.push(framePtr);
		}
	}
	else {
		throw new std::exception;
	}
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteRet(const Instruction & instr) {
	auto frame = callStack.top();
	delete frame;
	callStack.pop();
}

void rubbish::Context::ExecutePush(const Instruction & instr) {
	auto frame = callStack.top();
	dataStack.push(instr.value);
	(frame->instPtr)++;
}

void rubbish::Context::ExecutePop(const Instruction & instr) {
	auto frame = callStack.top();
	dataStack.pop();
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteAdd(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	
	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		val1.value.integerVal = val1.value.integerVal + val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		val1.value.doubleVal = val1.value.doubleVal + val2.value.doubleVal;
	}
	dataStack.push(val1);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteSub(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		val1.value.integerVal = val1.value.integerVal - val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		val1.value.doubleVal = val1.value.doubleVal - val2.value.doubleVal;
	}
	dataStack.push(val1);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteMul(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		val1.value.integerVal = val1.value.integerVal * val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		val1.value.doubleVal = val1.value.doubleVal * val2.value.doubleVal;
	}
	dataStack.push(val1);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteDiv(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		val1.value.integerVal = val1.value.integerVal / val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		val1.value.doubleVal = val1.value.doubleVal / val2.value.doubleVal;
	}
	dataStack.push(val1);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteNop(const Instruction & instr) {
	auto frame = callStack.top();
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteJnz(const Instruction & instr)
{
	auto frame = callStack.top();
	checkTypeThrow(instr.value, ValueType::VAL_INTEGER);
	int offset = instr.value.value.integerVal;
	auto top = dataStack.top();
	checkTypeThrow(top, ValueType::VAL_INTEGER);
	if (top.value.integerVal != 0) {
		(frame->instPtr) += offset;
	}
	else {
		(frame->instPtr)++;
	}
}

void rubbish::Context::ExecuteJz(const Instruction & instr)
{
	auto frame = callStack.top();
	checkTypeThrow(instr.value, ValueType::VAL_INTEGER);
	int offset = instr.value.value.integerVal;
	auto top = dataStack.top();
	checkTypeThrow(top, ValueType::VAL_INTEGER);
	if (top.value.integerVal == 0) {
		(frame->instPtr) += offset;
	}
	else {
		(frame->instPtr)++;
	}
}

void rubbish::Context::ExecuteJmp(const Instruction & instr)
{
	auto frame = callStack.top();
	checkTypeThrow(instr.value, ValueType::VAL_INTEGER);
	int offset = instr.value.value.integerVal;
	auto top = dataStack.top();
	checkTypeThrow(top, ValueType::VAL_INTEGER);
	(frame->instPtr) += offset;
}

void rubbish::Context::ExecuteAll() {
	while (!callStack.empty()) {
		auto frame = callStack.top();
		ExecuteInstruction(frame->functionInfo->instructions[frame->instPtr]);
	}
}