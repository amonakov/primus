primus
======

Primus  is a preloadable shared library that implements low-overhead
local-only client-side OpenGL offloading via GLX forking, similar to
VirtualGL.  It intercepts GLX calls and redirects GL rendering to a
secondary X display, presumably driven by a faster GPU. On swapping
buffers, rendered contents are read back using a PBO and copied onto
the drawable it was supposed to be rendered on in the first place.

Both original and shadow GL contexts can use direct rendering.

A simple shell script, `primusrun`, is provided to help with testing.

At the moment, it's pretty much proof-of-concept.  Known to work tests
are glxinfo, glxgears, glxgears_fbconfig, glxspheres and Osmos.

Anything that uses GLX extensions probably does not work.

On benchmarking
---------------

Please note that on some systems performance of glDrawPixels on Intel
cards is so low that it's becoming the bottleneck (check with e.g.
`glreadtest` from VirtualGL sources -- you should be getting 500+ MPix/s
on glDrawPixels operations with Mesa git).

Please also note that VirtualGL does not really display all frames it
gets from the offload slave.  By default, primus does render all frames
it gets from the secondary server, but that can be overridden with
`PRIMUS_DROPFRAMES` (see primusrun).

For an interesting comparison, play around with `ipers` from mesa-demos.

On contributing
---------------

A good way to contribute would be to run mesa-demos under primus, it
provides many simple tests.  Analyze failures or file bugreports.
