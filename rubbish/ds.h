//TODO: implement runtime typechecking

//Generic value. Strings are POD right now.
//TODO: implement strings in terms of the List object.
typedef union {
	void* generic_data;
	int   integer_data;
	char* string_data;
} ISH_Value;

//TODO: figure out proper types and sizes for these
typedef enum {
	ISH_INST_STOREVAR,
	ISH_INST_LOADVAR,
	ISH_INST_CALL,
	ISH_INST_RET,
	ISH_INST_RETCALL,
	ISH_INST_PUSH,
	ISH_INST_POP,
	ISH_INST_ADD,
	ISH_INST_SUB,
	ISH_INST_MUL,
	ISH_INST_DIV,
	ISH_INST_JTRUE,
	ISH_INST_JMP,
	ISH_INST_GT,
	ISH_INST_LT,
	ISH_INST_EQ,
	ISH_INST_NOT,
	ISH_INST_AND,
	ISH_INST_OR,
	ISH_INST_NOP
} ISH_Opcode;

typedef struct {
	ISH_Opcode opcode;
	ISH_Value  operand;
} ISH_Instruction;

//Execution context of a Rubbish thread
typedef struct {
	ISH_Instruction* instructions;
	ISH_ValueStack*  stack;
} ISH_Context;

//Implements scheduling policies, currently round robin only.
//TODO: implement a better scheduler
typedef struct {
	ISH_Context* context;
} ISH_Scheduler;
