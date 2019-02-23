#include "defs.h"

rubbish::Value::Value() : type(ValueType::VAL_INVALID) {
}

rubbish::Value::Value(const Value & v) : type(v.type) {
	if (v.type == ValueType::VAL_STRING) {
		int len = strlen(v.value.stringVal);
		value.stringVal = new char[len + 1];
		value.stringVal[len] = 0;
		strcpy(value.stringVal, v.value.stringVal);
	}
	else {
		value = v.value;
	}
}

rubbish::Value::Value(ValueType valueType, const ValueValue& value) 
	: type(valueType), value(value) {
}

//rubbish::Value::~Value() {
//	if (type == ValueType::VAL_STRING) {
//		delete value.stringVal;
//	}
//}

rubbish::Instruction::Instruction() : type(InstructionType::INST_NOP) {
}

rubbish::Instruction::Instruction(InstructionType instructionType) 
	: type(instructionType) {
}

rubbish::Instruction::Instruction(InstructionType instructionType, const Value & value) 
	: type(instructionType), value(value) {
}

bool rubbish::Context::IsDoneExecuting() {
	return callStack.empty();
}

void rubbish::Context::RegisterFunction(FunctionInfo * functionInfo) {
	functionStore[functionInfo->name] = functionInfo;
}

void rubbish::Context::RegisterNativeFunction(std::string name, int arity, std::function<bool(Context&)> func) {
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
	
	case InstructionType::INST_RETCALL:
		ExecuteRetcall(instr);
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

	case InstructionType::INST_JTRUE:
		ExecuteJtrue(instr);
		break;

	case InstructionType::INST_JMP:
		ExecuteJmp(instr);
		break;

	case InstructionType::INST_GT:
		ExecuteGt(instr);
		break;

	case InstructionType::INST_LT:
		ExecuteLt(instr);
		break;

	case InstructionType::INST_EQ:
		ExecuteEq(instr);
		break;

	case InstructionType::INST_NOT:
		ExecuteNot(instr);
		break;

	case InstructionType::INST_AND:
		ExecuteAnd(instr);
		break;

	case InstructionType::INST_OR:
		ExecuteOr(instr);
		break;
	}
}

void rubbish::Context::LoadFunctionForExecution(std::string funcName) {
	auto found = functionStore.find(funcName);
	if (found != functionStore.end()) {
		auto fInfo = found->second;

		switch (fInfo->type) {
		case FunctionType::FUNC_NATIVE:
			fInfo->nativeFunction(*this);
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
			fInfo->nativeFunction(*this);
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

void rubbish::Context::ExecuteRetcall(const Instruction & instr) {
	auto frame = callStack.top();
	delete frame;
	callStack.pop();

	auto found = functionStore.find(instr.value.value.stringVal);
	if (found != functionStore.end()) {
		auto fInfo = found->second;

		switch (fInfo->type) {
		case FunctionType::FUNC_NATIVE:
			fInfo->nativeFunction(*this);
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

void rubbish::Context::ExecuteJtrue(const Instruction & instr) {
	auto frame = callStack.top();
	checkTypeThrow(instr.value, ValueType::VAL_INTEGER);
	int offset = instr.value.value.integerVal;
	auto top = dataStack.top();
	dataStack.pop();
	checkTypeThrow(top, ValueType::VAL_BOOL);
	if (top.value.boolVal == true) {
		(frame->instPtr) += offset;
	}
	else {
		(frame->instPtr)++;
	}
}

void rubbish::Context::ExecuteJmp(const Instruction & instr) {
	auto frame = callStack.top();
	checkTypeThrow(instr.value, ValueType::VAL_INTEGER);
	int offset = instr.value.value.integerVal;
	(frame->instPtr) += offset;
}

void rubbish::Context::ExecuteGt(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		v.value.boolVal = val1.value.integerVal > val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		v.value.boolVal = val1.value.doubleVal > val2.value.doubleVal;
	}

	dataStack.push(v);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteLt(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		v.value.boolVal = val1.value.integerVal < val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		v.value.boolVal = val1.value.doubleVal < val2.value.doubleVal;
	}

	dataStack.push(v);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteEq(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_INTEGER) {
		checkTypeThrow(val2, ValueType::VAL_INTEGER);
		v.value.boolVal = val1.value.integerVal == val2.value.integerVal;
	}
	else if (val1.type == ValueType::VAL_DOUBLE) {
		checkTypeThrow(val2, ValueType::VAL_DOUBLE);
		v.value.boolVal = val1.value.doubleVal == val2.value.doubleVal;
	}

	dataStack.push(v);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteNot(const Instruction & instr) {
	auto frame = callStack.top();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_BOOL) {
		v.value.boolVal = !val1.value.boolVal;
	}
	else {
		throw new std::exception;
	}

	dataStack.push(v);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteAnd(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_BOOL) {
		checkTypeThrow(val2, ValueType::VAL_BOOL);
		v.value.boolVal = val1.value.boolVal && val2.value.boolVal;
	}
	else {
		throw new std::exception;
	}
	
	dataStack.push(v);
	(frame->instPtr)++;
}

void rubbish::Context::ExecuteOr(const Instruction & instr) {
	auto frame = callStack.top();
	auto val2 = dataStack.top();
	dataStack.pop();
	auto val1 = dataStack.top();
	dataStack.pop();
	Value v;
	v.type = ValueType::VAL_BOOL;

	if (val1.type == ValueType::VAL_BOOL) {
		checkTypeThrow(val2, ValueType::VAL_BOOL);
		v.value.boolVal = val1.value.boolVal || val2.value.boolVal;
	}
	else {
		throw new std::exception;
	}

	dataStack.push(v);
	(frame->instPtr)++;
}

int rubbish::Context::ExecuteNInstructions(int n) {
	int executed = 0;
	
	for (int i = 0; i < n; i++) {
		if (!IsDoneExecuting()) {
			auto frame = callStack.top();
			ExecuteInstruction(frame->functionInfo->instructions[frame->instPtr]);
			executed++;
		}
		else {
			break;
		}
	}

	return executed;
}

void rubbish::Context::ExecuteAll() {
	while (!callStack.empty()) {
		auto frame = callStack.top();
		ExecuteInstruction(frame->functionInfo->instructions[frame->instPtr]);
	}
}

int rubbish::Scheduler::AddContext(std::unique_ptr<Context> context) {
	int pid = currentUID;
	currentUID++;

	context->scheduler = this;	
	context->pid = pid;

	std::shared_ptr<Context> shared = std::move(context);
	processStore[pid] = shared;
	runQueue.push_back(std::move(shared));

	return pid;
}

void rubbish::Scheduler::ExecuteAll() {
	while (!runQueue.empty()) {
		auto currentContext = runQueue.front();
		runQueue.pop_front();
		
		currentContext->ExecuteNInstructions(numInstructionsPerRound);

		if (!currentContext->IsDoneExecuting()) {
			runQueue.push_back(std::move(currentContext));
		}
	}
}
