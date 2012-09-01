CXX	:= g++
CXXFLAGS := -Wall -g


libGL.so.1: libglfork.cpp
	$(CXX) $(CXXFLAGS) -fPIC -shared -o $@ $< -lX11 -lpthread
