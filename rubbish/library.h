#include "defs.h"
#include <iostream>

namespace rubbish {
	bool PrintLnFunc(std::stack<Value> & dataStack) {
		auto val = dataStack.top();
		switch (val.type) {
		case ValueType::VAL_BOOL:
			std::cout << "[Bool] " << val.value.boolVal << std::endl;
			break;

		case ValueType::VAL_DOUBLE:
			std::cout << "[Double] " << val.value.doubleVal << std::endl;
			break;

		case ValueType::VAL_INTEGER:
			std::cout << "[Integer] " << val.value.integerVal << std::endl;
			break;

		case ValueType::VAL_INVALID:
			std::cout << "[Invalid]" << std::endl;
			break;

		case ValueType::VAL_POINTER:
			std::cout << "[" << val.value.pointerVal << "]" << std::endl;
			break;

		case ValueType::VAL_STRING:
			std::cout << "[String] " << val.value.stringVal << std::endl;
			break;
		}

		return true;
	}

	bool GetOneIntFunc(std::stack<Value> & dataStack) {
		int x;
		std::cin >> x;
		Value v;
		v.type = ValueType::VAL_INTEGER;
		v.value.integerVal = x;

		dataStack.push(v);

		return true;
	}

	void LoadLibrary(Context& context) {
		FunctionInfo* printLnInfo = new FunctionInfo;
		printLnInfo->name = "printLn";
		printLnInfo->arity = 0;
		printLnInfo->type = FunctionType::FUNC_NATIVE;
		printLnInfo->nativeFunction = PrintLnFunc;
		context.RegisterFunction(printLnInfo);

		FunctionInfo* getOneIntInfo = new FunctionInfo;
		getOneIntInfo->name = "getOneInt";
		getOneIntInfo->arity = 0;
		getOneIntInfo->type = FunctionType::FUNC_NATIVE;
		getOneIntInfo->nativeFunction = GetOneIntFunc;
		context.RegisterFunction(getOneIntInfo);
	}
};