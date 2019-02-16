//Rubbish language definitions. Opcodes and such.
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
		INST_PUSHIMM,
		INST_POP,
		INST_ADD,
		INST_SUB,
		INST_MUL,
		INST_DIV,
		INST_NOP
	};

	struct Instruction {
		InstructionType type;
		Value value;

		Instruction();
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

		void RegisterFunction(FunctionInfo* functionInfo);
		
		void ExecuteInstruction(const Instruction& instr);
		void LoadFunctionForExecution(std::string funcName);

		void ExecuteStoreVar(const Instruction& instr);
		void ExecuteLoadVar(const Instruction& instr);
		void ExecuteCall(const Instruction& instr);
		void ExecuteRet(const Instruction& instr);
		void ExecutePushImm(const Instruction& instr);
		void ExecutePop(const Instruction& instr);
		void ExecuteAdd(const Instruction& instr);
		void ExecuteSub(const Instruction& instr);
		void ExecuteMul(const Instruction& instr);
		void ExecuteDiv(const Instruction& instr);
		void ExecuteNop(const Instruction& instr);

		void ExecuteAll();
	};
};