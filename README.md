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

Some applications, e.g. Bastion, need PRIMUS_DROPFRAMES=1 in environment
to avoid a deadlock.

At the moment primus does not advertise any GLX extensions to the client.

On benchmarking
---------------

Please note that VirtualGL does not really display all frames it
gets from the offload slave.  By default, primus does render all frames
it gets from the secondary server, but that can be overridden with
`PRIMUS_DROPFRAMES` (see primusrun).

For an interesting comparison, play around with `ipers` from mesa-demos.
