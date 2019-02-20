// rubbish.cpp : Defines the entry point for the application.
//

#include "rubbish.h"
#include "defs.h"
#include "library.h"
#include "parser.h"

using namespace std;
using namespace rubbish;

int main(int argc, char** argv) {
	std::string path;
	if (argc > 1) {
		path = argv[1];
	}
	else {
		path = "C:\\Users\\Siddharth\\Source\\Repos\\sidsahay\\rubbish-lang\\examples\\factorial.ishc";
	}

	auto context = parser::ParseFile(path);
	context->LoadInternalTopLevelFunction();
	LoadLibrary(*context);
	context->ExecuteAll();
	int x;
	cin >> x;
	return 0;
}