CXX = clang++

LLVM_PREFIX = /usr/local

LLVM_BIN_PATH = $(LLVM_PREFIX)/bin
LLVM_CONFIG_COMMAND = $(LLVM_BIN_PATH)/llvm-config --cxxflags --ldflags --libs
CLANG_BUILD_FLAGS = -I$(LLVM_PREFIX)/include

CLANGLIBS = \
	-lclangTooling -lclangFrontend -lclangSerialization \
	-lclangDriver -lclangRewriteCore -lclangRewriteFrontend -lclangParse \
	-lclangSema -lclangAnalysis -lclangAST \
	-lclangASTMatchers -lclangEdit -lclangLex \
	-lclangBasic

all: complex_convert

complex_convert: complex_convert.cpp
	$(CXX) complex_convert.cpp $(CFLAGS) -o complex_convert \
    $(CLANG_BUILD_FLAGS) $(CLANGLIBS) `$(LLVM_CONFIG_COMMAND)`

clean:
	rm -rf *.o *.ll complex_convert
