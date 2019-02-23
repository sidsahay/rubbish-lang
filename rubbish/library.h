#pragma once

#include "defs.h"
#include <iostream>
#include <memory>
#include <vector>

namespace rubbish {
	namespace libdefs {
		bool PrintLnDebugFunc(Context& context) {
			auto val = context.dataStack.top();
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

		bool PrintFunc(Context& context) {
			auto val = context.dataStack.top();
			switch (val.type) {
			case ValueType::VAL_BOOL:
				std::cout << val.value.boolVal;
				break;

			case ValueType::VAL_DOUBLE:
				std::cout << val.value.doubleVal;
				break;

			case ValueType::VAL_INTEGER:
				std::cout << val.value.integerVal;
				break;

			case ValueType::VAL_INVALID:
				std::cout << "[Invalid]";
				break;

			case ValueType::VAL_POINTER:
				std::cout << "[" << val.value.pointerVal << "]";
				break;

			case ValueType::VAL_STRING:
				std::cout << val.value.stringVal;
				break;
			}

			return true;
		}

		bool PrintLnFunc(Context& context) {
			PrintFunc(context);
			std::cout << std::endl;

			return true;
		}

		bool GetOneIntFunc(Context& context) {
			int x;
			std::cin >> x;
			Value v;
			v.type = ValueType::VAL_INTEGER;
			v.value.integerVal = x;

			context.dataStack.push(v);

			return true;
		}

		bool SpawnFunc(Context& context) {
			//number of args to function, because variadic function
			auto numArgs = context.dataStack.top();
			checkTypeThrow(numArgs, ValueType::VAL_INTEGER);
			int numArgsInt = numArgs.value.integerVal;
			context.dataStack.pop();

			std::vector<Value> args;
			
			for (int i = 0; i < numArgsInt; i++) {
				auto topVal = context.dataStack.top();
				args.push_back(topVal);
				context.dataStack.pop();
			}

			auto funcName = context.dataStack.top();
			checkTypeThrow(funcName, ValueType::VAL_STRING);
			context.dataStack.pop();

			std::unique_ptr<Context> spawnedContext(new Context);
			spawnedContext->functionStore = context.functionStore;
			
			//Insert args in proper order i.e. reverse of popped order because the stack reverses it anyway
			for (auto it = args.rbegin(); it != args.rend(); it++) {
				spawnedContext->dataStack.push(*it);
			}

			spawnedContext->LoadFunctionForExecution(funcName.value.stringVal);
			
			int pid = context.scheduler->AddContext(std::move(spawnedContext));

			Value pidVal;
			pidVal.type = ValueType::VAL_INTEGER;
			pidVal.value.integerVal = pid;

			context.dataStack.push(pidVal);

			return true;
		}

		bool SendFunc(Context& context) {
			auto val = context.dataStack.top();
			context.dataStack.pop();

			auto pidVal = context.dataStack.top();
			context.dataStack.pop();
			checkTypeThrow(pidVal, ValueType::VAL_INTEGER);
			int pid = pidVal.value.integerVal;

			auto found = context.scheduler->processStore.find(pid);

			if (found != context.scheduler->processStore.end()) {
				found->second->mailbox.push_back(val);
				Value returnVal;
				returnVal.type = ValueType::VAL_BOOL;
				returnVal.value.boolVal = true;
				context.dataStack.push(returnVal);
			}
			else {
				throw new std::exception;
			}

			return true;
		}

		bool ReceiveFunc(Context& context) {
			if (context.mailbox.empty()) {
				//do nothing i.e. move the frame pointer back by one so receive() is executed again
				auto frame = context.callStack.top();
				(frame->instPtr)--;
			}
			else {
				auto val = context.mailbox.front();
				context.mailbox.pop_front();

				context.dataStack.push(val);
			}

			return true;
		}

		bool GetPidFunc(Context& context) {
			Value returnVal;
			returnVal.type = ValueType::VAL_INTEGER;
			returnVal.value.integerVal = context.pid;
			context.dataStack.push(returnVal);
			
			return true;
		}
	}
	

	void LoadLibrary(Context& context) {
		using namespace libdefs;
		
		context.RegisterNativeFunction("printLnDebug", 0, PrintLnDebugFunc);
		context.RegisterNativeFunction("print", 0, PrintFunc);
		context.RegisterNativeFunction("printLn", 0, PrintLnFunc);
		context.RegisterNativeFunction("getOneInt", 0, GetOneIntFunc);
		context.RegisterNativeFunction("spawn", 0, SpawnFunc);
		context.RegisterNativeFunction("send", 0, SendFunc);
		context.RegisterNativeFunction("receive", 0, ReceiveFunc);
		context.RegisterNativeFunction("getPid", 0, GetPidFunc);
	}
};