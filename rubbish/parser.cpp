#include "parser.h"
//THIS IS A TERRIBLE PARSER PLEASE DO NOT USE IT UNLESS YOU HAVE TO

using namespace rubbish;

namespace rubbish {
	namespace parser {
		static constexpr size_t BUF_SIZE = 256;

#define MAKE_ZERO(buf) memset(buf, 0, sizeof(char) * BUF_SIZE)
#define EQ(str1, str2) (!(strcmp(str1, str2)))

		char* RecoverString(char* str) {
			int len = strlen(str);
			for (int i = 0; i < len; i++) {
				if (str[i] == '"') {
					str[i] = '\0';
				}
				else if (str[i] == '_') {
					str[i] = ' ';
				}
			}

			return str + 1;
		}

		enum ParserState {
			TOP_LEVEL,
			IN_FUNCTION
		};
	};
};

std::unique_ptr<Context> rubbish::parser::ParseFile(std::string path) {
	auto context = std::make_unique<Context>();
	FunctionInfo* funcInfo = nullptr;

	char* buf = new char[BUF_SIZE];
	int spaceLoc = 0;
	std::ifstream in(path);
	std::vector<std::string> input;

	ParserState state = TOP_LEVEL;

	while (!in.eof()) {
		MAKE_ZERO(buf);
		in.getline(buf, BUF_SIZE);
		char* token = strtok(buf, " ");
		if ((token == NULL) || EQ(token, "")) continue;

		if (state == TOP_LEVEL) {
			if (EQ(token, "#topfunction")) {
				token = strtok(NULL, " ");
				char* name = RecoverString(token);
				context->topFunctionName = std::string(name);
			}
			else if (EQ(token, "#function")) {
				funcInfo = new FunctionInfo;
				funcInfo->type = FunctionType::FUNC_RUBBISH;
				
				token = strtok(NULL, " ");
				char* name = RecoverString(token);
				funcInfo->name = std::string(name);

				/*token = strtok(NULL, " ");
				int arity = atoi(token);
				funcInfo->arity = arity;
				*/
				state = IN_FUNCTION;
			}
			else {
				std::cout << "[Error] Expected a top-level construct like #topfunction or #function at token: " << token << "\n";
			}
		}
		else if (state == IN_FUNCTION) {
			if (EQ(token, "#endfunction")) {
				context->RegisterFunction(funcInfo);
				funcInfo = nullptr;
				state = TOP_LEVEL;
			}
			else if (EQ(token, "storevar")) {
				Instruction i;
				i.type = InstructionType::INST_STOREVAR;
				token = strtok(NULL, " ");
				i.value = ParseValue(token);
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "loadvar")) {
				Instruction i;
				i.type = InstructionType::INST_LOADVAR;
				token = strtok(NULL, " ");
				i.value = ParseValue(token);
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "call")) {
				Instruction i;
				i.type = InstructionType::INST_CALL;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "ret")) {
				Instruction i;
				i.type = InstructionType::INST_RET;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "retcall")) {
				Instruction i;
				i.type = InstructionType::INST_RETCALL;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "push")) {
				Instruction i;
				i.type = InstructionType::INST_PUSH;
				token = strtok(NULL, " ");
				i.value = ParseValue(token);
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "pop")) {
				Instruction i;
				i.type = InstructionType::INST_POP;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "add")) {
				Instruction i;
				i.type = InstructionType::INST_ADD;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "sub")) {
				Instruction i;
				i.type = InstructionType::INST_SUB;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "mul")) {
				Instruction i;
				i.type = InstructionType::INST_MUL;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "div")) {
				Instruction i;
				i.type = InstructionType::INST_DIV;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "nop")) {
				Instruction i;
				i.type = InstructionType::INST_NOP;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "jtrue")) {
				Instruction i;
				i.type = InstructionType::INST_JTRUE;
				token = strtok(NULL, " ");
				i.value = ParseValue(token);
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "jmp")) {
				Instruction i;
				i.type = InstructionType::INST_JMP;
				token = strtok(NULL, " ");
				i.value = ParseValue(token);
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "gt")) {
				Instruction i;
				i.type = InstructionType::INST_GT;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "lt")) {
				Instruction i;
				i.type = InstructionType::INST_LT;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "eq")) {
				Instruction i;
				i.type = InstructionType::INST_EQ;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "not")) {
				Instruction i;
				i.type = InstructionType::INST_NOT;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "and")) {
				Instruction i;
				i.type = InstructionType::INST_AND;
				funcInfo->instructions.push_back(i);
			}
			else if (EQ(token, "or")) {
				Instruction i;
				i.type = InstructionType::INST_NOP;
				funcInfo->instructions.push_back(i);
			}
			else {
				std::cout << "[Error] expected function body or #endfunction at token: " << token << "," << std::endl;
			}
		}
	}
	in.close();

	return context;
}

Value rubbish::parser::ParseValue(char * buf) {
	Value v;
	
	if (buf[0] == '"') {
		buf = RecoverString(buf);
		char* internBuf = new char[strlen(buf) + 1];
		memset(internBuf, 0, (strlen(buf) + 1) * sizeof(char));

		strcpy(internBuf, buf);
		v.type = ValueType::VAL_STRING;
		v.value.stringVal = internBuf;
	}
	else if (buf[0] == '%') {
		if (EQ(buf, "%true")) {
			v.type = ValueType::VAL_BOOL;
			v.value.boolVal = true;
		}
		else {
			v.type = ValueType::VAL_BOOL;
			v.value.boolVal = false;
		}
	}
	else {
		v.type = ValueType::VAL_INTEGER;
		v.value.integerVal = atoi(buf);
	}

	return v;
}


