SUFFIX=.so
OBJ=OSRCreator.o
LIB_NAME = OSRCreator$(SUFFIX)
LLVM_PATH=/usr/lib/llvm-9/bin/
LDFLAGS = $(shell llvm-config-9 --ldflags)
LDFLAGS +=-Wl,-dead_strip -Wl,-flat_namespace -Wl,-undefined -Wl,suppress -shared
#LDFLAGS += -Wl,-dead_strip -Wl,-undefined,dynamic_lookup
CXXFLAGS = -g -Wall -fno-rtti -fPIC -std=c++11 $(shell llvm-config-9  --cxxflags --system-libs)
COMPILER = clang++-9
all: $(LIB_NAME)
$(LIB_NAME): $(OBJ)
	$(COMPILER) $(CXXFLAGS) $(LDFLAGS) $^ -o $@
$(OBJ): OSRCreator.cpp 
	$(COMPILER) $(CXXFLAGS) -c $^ -o $@
clean:
	rm *.o *.so