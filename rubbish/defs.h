//Rubbish language definitions. Opcodes and such.
#pragma once

#include <vector>
#include <map>
#include <memory>
#include <stack>
#include <functional>
#include <exception>

namespace rubbish {

	enum class ValueType {
		VAL_INTEGER,
		VAL_DOUBLE,
		VAL_STRING,
		VAL_BOOL,
		VAL_POINTER,
		VAL_INVALID
	};

	union ValueValue {
		int integerVal;
		double doubleVal;
		char* stringVal;
		bool boolVal;
		void* pointerVal;
	};

	struct Value {
		ValueType type;
		ValueValue value;
		
		Value();
		Value(ValueType valueType, const ValueValue& value);
	};

	enum class InstructionType {
		INST_STOREVAR,
		INST_LOADVAR,
		INST_CALL,
		INST_RET,
		INST_RETCALL,
		INST_PUSH,
		INST_POP,
		INST_ADD,
		INST_SUB,
		INST_MUL,
		INST_DIV,
		INST_JTRUE,
		INST_JMP,
		INST_GT,
		INST_LT,
		INST_EQ,
		INST_NOT,
		INST_AND,
		INST_OR,
		INST_NOP
	};

	struct Instruction {
		InstructionType type;
		Value value;

		Instruction();
		Instruction(InstructionType instructionType);
		Instruction(InstructionType instructionType, const Value& value);
	};

	template <typename T, typename U>
	void checkTypeThrow(T val, U type) {
		if (val.type != type) {
			throw new std::exception;
		}
	}

	enum class FunctionType {
		FUNC_NATIVE,
		FUNC_RUBBISH
	};

	struct FunctionInfo {
		FunctionType type;
		std::string name;
		int arity;
		std::vector<Instruction> instructions;
		std::function<bool(std::stack<Value>&)> nativeFunction;
	};

	struct Frame {
		FunctionInfo* functionInfo;
		std::map<std::string, Value> dataStore;
		int instPtr = 0;
	};

	struct Context {
		std::map<std::string, FunctionInfo*> functionStore;
		std::stack<Value> dataStack;
		std::stack<Frame*> callStack;

		std::string topFunctionName;

		void RegisterFunction(FunctionInfo* functionInfo);
		void RegisterNativeFunction(std::string name, int arity, std::function<bool(std::stack<Value>&)> func);
		void RegisterRubbishFunction(std::string name, int arity, const std::vector<Instruction>& instructions);
		
		void ExecuteInstruction(const Instruction& instr);
		void LoadFunctionForExecution(std::string funcName);
		void LoadInternalTopLevelFunction();

		void ExecuteStoreVar(const Instruction& instr);
		void ExecuteLoadVar(const Instruction& instr);
		void ExecuteCall(const Instruction& instr);
		void ExecuteRet(const Instruction& instr);
		void ExecuteRetcall(const Instruction& instr);
		void ExecutePush(const Instruction& instr);
		void ExecutePop(const Instruction& instr);
		void ExecuteAdd(const Instruction& instr);
		void ExecuteSub(const Instruction& instr);
		void ExecuteMul(const Instruction& instr);
		void ExecuteDiv(const Instruction& instr);
		void ExecuteNop(const Instruction& instr);
		void ExecuteJtrue(const Instruction& instr);
		void ExecuteJmp(const Instruction& instr);
		void ExecuteGt(const Instruction& instr);
		void ExecuteLt(const Instruction& instr);
		void ExecuteEq(const Instruction& instr);
		void ExecuteNot(const Instruction& instr);
		void ExecuteAnd(const Instruction& instr);
		void ExecuteOr(const Instruction& instr);

		void ExecuteAll();
	};
};