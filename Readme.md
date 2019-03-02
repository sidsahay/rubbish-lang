# The Rubbish Programming Language
Rubbish is a language that is optimized to be terrible at everything in general. Rubbish's planned features include:
* Actor model, lightweight processes based concurrency (Erlang-style) (currently no support for SMP)
* Immutable only values. This allows language implementation and GC to be easier. It also allows programming to be more difficult in most cases, which is a design goal(TM).
* Impure functional programming.
* A terrible standard library with no support for doing most common tasks.
* A database engine called Dump that you should definitely use for real work, if nothing else then just for the jokes.

## How to build
Rubbish uses the CMake build system. To build you must have the following installed
* CMake 3.8 or so
* Haskell's Stack build tool (search for OS-specific instructions on how to install stack)
* A working C/C++ compiler toolchain (Visual Studio's C/C++ on Windows, GCC/G++ or clang on Linux)
* A C++ build system like make (for Linux) or Visual Studio etc. for Windows

To build using CMake
* `cd {BUILD_DIRECTORY}`
* `cmake {SOURCE_DIRECTORY} -DCMAKE_BUILD_TYPE=Release` (you can build in debug mode too (just ignore that last parameter) but then it won't put the executables in one place)
* Invoke your build tool. On linux it's usually just `make`, on Windows go to the build directory, open `rubbish.sln` in Visual Studio, and build the targets (on Windows you have to set Release mode inside Visual Studio to trigger the Release config)
* In Release config, the built executables (`rubbish` and `rubbishc`) are put in the build directory under a `build_output` folder

## Compiling and running Rubbish programs
Rubbish programs have the extension `.ish` and the compiled files have the extension `.ishc`. Suppose you have a `program.ish` file. To compile and run, do
* `rubbishc program.ish program.ishc`
* `rubbish program.ishc`

It's like Java, `rubbish` is the interpreter and `rubbishc` is the compiler.