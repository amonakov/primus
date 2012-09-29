CXX      ?= g++
CXXFLAGS ?= -Wall -g
LIBDIR   ?= lib

BUMBLEBEE_SOCKET   := '"/var/run/bumblebee.socket"'
PRIMUS_DISPLAY     := '":8"'
PRIMUS_LOAD_GLOBAL := '"/usr/$$LIB/libglapi.so"'
PRIMUS_libGLa      := '"/usr/$$LIB/nvidia-bumblebee/libGL.so.1"'
PRIMUS_libGLd      := '"/usr/$$LIB/libGL.so.1"'

CXXFLAGS += -DBUMBLEBEE_SOCKET=$(BUMBLEBEE_SOCKET)
CXXFLAGS += -DPRIMUS_DISPLAY=$(PRIMUS_DISPLAY)
CXXFLAGS += -DPRIMUS_LOAD_GLOBAL=$(PRIMUS_LOAD_GLOBAL)
CXXFLAGS += -DPRIMUS_libGLa=$(PRIMUS_libGLa)
CXXFLAGS += -DPRIMUS_libGLd=$(PRIMUS_libGLd)

$(LIBDIR)/libGL.so.1: libglfork.cpp
	mkdir -p $(LIBDIR)
	$(CXX) $(CXXFLAGS) -fPIC -shared -o $@ $< -lX11 -lpthread -lrt
