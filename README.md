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
