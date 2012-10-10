primus
======

Primus is a shared library that provides OpenGL and GLX APIs and
implements low-overhead
local-only client-side OpenGL offloading via GLX forking, similar to
VirtualGL.  It intercepts GLX calls and redirects GL rendering to a
secondary X display, presumably driven by a faster GPU. On swapping
buffers, rendered contents are read back using a PBO and copied onto
the drawable it was supposed to be rendered on in the first place.

Both original and shadow GL contexts can use direct rendering.

A simple shell script, `primusrun`, is provided to help with testing.

At the moment primus does not advertise any GLX extensions to the client.

Building for multilib (32-bit + 64-bit) systems
-----------------------------------------------

    LIBDIR=lib make && CXX=g++\ -m32 LIBDIR=lib32 make

Adjust `LIBDIR` variables above as appropriate for your distribution
(reflecting how `/usr/lib*` are named):

* Arch needs `lib` and `lib32` as above
* Gentoo needs `lib64` and `lib32`
* RPM-based may need `lib64` and `lib`
* Debian (with multiarch) needs `lib/x86_64-linux-gnu` and `lib/i386-linux-gnu`

On benchmarking
---------------

Please note that VirtualGL does not really display all frames it
gets from the offload slave.  By default, primus does render all frames
it gets from the secondary server, except when the display thread
locks up for longer than 20 milliseconds.

For an interesting comparison, play around with `ipers` from mesa-demos.
