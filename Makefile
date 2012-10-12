CXX      ?= g++
CXXFLAGS ?= -Wall -g

# On multilib systems, this needs to point to distribution-specific library
# subdir like in /usr (lib or lib64 for 64-bit, lib32 or lib for 32-bit)
LIBDIR   ?= lib

BUMBLEBEE_SOCKET   := '"/var/run/bumblebee.socket"'
PRIMUS_DISPLAY     := '":8"'
PRIMUS_LOAD_GLOBAL := '"/usr/$$LIB/libglapi.so.0"'
PRIMUS_libGLa      := '"/usr/$$LIB/nvidia-bumblebee/libGL.so.1"'
PRIMUS_libGLd      := '"/usr/$$LIB/libGL.so.1"'

CXXFLAGS += -DBUMBLEBEE_SOCKET=$(BUMBLEBEE_SOCKET)
CXXFLAGS += -DPRIMUS_DISPLAY=$(PRIMUS_DISPLAY)
CXXFLAGS += -DPRIMUS_LOAD_GLOBAL=$(PRIMUS_LOAD_GLOBAL)
CXXFLAGS += -DPRIMUS_libGLa=$(PRIMUS_libGLa)
CXXFLAGS += -DPRIMUS_libGLd=$(PRIMUS_libGLd)

$(LIBDIR)/libGL.so.1: libglfork.cpp
	mkdir -p $(LIBDIR)
	$(CXX) $(CXXFLAGS) -fvisibility=hidden -fPIC -shared -o $@ $< -lX11 -lpthread -lrt
