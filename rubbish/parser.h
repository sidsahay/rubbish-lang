//Rubbish bytecode parser. Currently only for the human readable stuff.
//THIS IS A TERRIBLE PARSER PLEASE DO NOT USE IT UNLESS YOU HAVE TO
#pragma once

#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

#include "defs.h"

namespace rubbish {
	namespace parser {
		Context* ParseFile(std::string path);
		Value ParseValue(char* buf);
	};
};