Notes on primus implementation
==============================

primus: quick'n'dirty OpenGL offloading

The purpose is to redirect OpenGL rendering to another X display, and copy
the rendered frames to the original (as if rendering was not redirected),
similar to VirtualGL, but with different assumptions and design goals.

primus makes the following assumptions:

* both X servers are local, and direct rendering is available on both
* the application is "well-behaved" (uses double buffering, does not use
  color index rendering, does not draw on top of the OpenGL drawable, etc.)

In contrast, VirtualGL:

* assumes that the application's X display is remote
* supports obscure OpenGL functionality, making it more versatile

The design goals are:

* simplicity/minimalism:
    - only GLX functions are overridden
    - only two Xlib functions are used
* efficiency:
    - minimal amount of framebuffer data copying achievable from
      application-level offloading (unfortunately OpenGL does
      not allow to reduce beyond 2 copies, readback+display)
    - pipelining of rendering, readback and display

Put another way, VirtualGL is optimized for running arbitrary OpenGL
applications over a network, with correctness and bandwidth being primary
concerns, while primus is optimized for running modern games on hybrid
graphics hardware setups, with simplicity and performance in mind.  It is only
needed until DMA-BUF/PRIME offloading is implemented and mature.

This document collects various interesting issues encountered while
implementing primus.

Assuming that a secondary X server driving the rendering slave card is
present, OpenGL offloading can be achieved by intercepting GLX calls to
reroute application's rendering to a pbuffer on the secondary X server, and
then reading back the framebuffer and displaying it on the original drawable.

Intercepting GLX calls
----------------------

The usual way to perform symbol interposition in Linux is to use LD_PRELOAD,
and that is what VirtualGL does.  However, that is not enough when an
application uses dlopen+dlsym to access library functions. To support that,
VirtualGL overrides dlopen and dlsym via LD_PRELOAD as well, and it even
intercepts `dlopen("libdl")`. Perhaps a cleaner approach is to make the
wrapper provide the complete API (GLX+OpenGL in this case) and add the wrapper
into LD_LIBRARY_PATH instead, which is what primus implements. One problem
with this approach is that although primus overrides only GLX functions, it
has to implement trivial forwarding for OpenGL functions.

If the underlying OpenGL library is compiled without -Bsymbolic and calls
GLX functions itself, those calls will invoke functions from the wrapper,
which will cause failures unless the wrapper is prepared for that by keeping a
per-thread boolean value indicating whether a wrapper function was entered but
not returned yet (primus does not do that yet).

Implementing GLX redirection
----------------------------

For each rendering context created by the application, primus creates an
additional slave-side context for readback and master-side context for display
(the latter would not be necessary if displaying the framebuffer was performed
by some other means than OpenGL, but it's useful for vblank synchronization).

For each application thread that calls glXMakeCurrent, primus additionally
spawns a readback thread and a display thread. Rendering, readback and display
are pipelined: application thread regains control as soon as readback thread
issued an asynchronous glReadPixels into a PBO, and readback thread uses two
PBOs to perform readback of a new frame into one while another is used by the
display thread.

Threads signal data availability/release via posix semaphores. Additionally, a
GL sync object is required so that readback thread does not read an incomplete
frame.

The application sees slave-side FBConfig and GLXContext IDs, but master-side X
Visuals. 

One GLX function that is not easily redirected is glXUseXFont, as it internally
calls OpenGL functions on bitmap data from the X server. We need bitmap data
from one server, but OpenGL functions need to be called in the other server's
context.

Direct Rendering on Both Servers
--------------------------------

The worker threads each get a direct rendering context. One small issue that
was encountered is that symbols from libglapi.so need to be loaded by primus
in a global namespace (`dlopen("libglapi.so.0", RTLD_GLOBAL)`), otherwise DRI
modules would fail to load (they need symbols from libglapi, but don't link
against it). Starting with Mesa 9.0, DRI modules do link against libglapi, so
this is not a problem.

However, this means that without shared libglapi, it is not possible to obtain
direct rendering context when loading Mesa with `dlopen("libGL.so.1",
RTLD_LOCAL)`.    

Xlib and threads
----------------

primus spawns additional threads for each application thread that calls
glXMakeCurrent: a readback thread and a display thread. The latter makes calls
to glXSwapBuffers after drawing a full-screen quad asynchronously with the
application. Initially, primus called XInitThreads to enable that, as
suggested by the Xlib error message. However, if an applications loads both
Xlib and libGL dynamically via dlopen, primus may not be able to call
XInitThreads early enough. It turned out that opening separate X connections
for primus' needs is a simpler and more robust solution.

Forwarding OpenGL calls
-----------------------

To provide OpenGL API functions, primus contains trivial forwarding functions
(VirtualGL overrides some of OpenGL functions, e.g. glFinish to support
single-buffered applications; primus does not support those). However, it
would be better to rely on a dynamic linker mechanism to avoid the need to
provide forwarder implementations, and instead make the dynamic linker resolve
OpenGL functions to definitions found in a slave libGL. On Solaris, that would
be possible with per-symbol DT_FILTER tagging; unfortunately, this is not
supported in binutils and glibc.

Another question is what functions need to be forwarded. Per OpenGL ABI
document, libraries need to export only up to OpenGL 1.2 and ARB_multitexture,
and anything above that the applications must obtain via glXGetProcAddress. In
reality, Mesa's libGL and nVidia's libGL export a significant (but different)
amount of functions implementing extensions, and there are applications either
linking against those (fortunately not many, Braid and Trine 2 have been found
guilty so far) or even trying to obtain pointers via dlsym (Enemy Territory).

Compositing
-----------

There had been reports that using primus under a compositing WM causes
flickering or slow/uneven update rate.  When worker threads are synchronized
in a way that the application's thread calling glXSwapBuffers does not regain
control until the display worker completed blitting and called glXSwapBuffers
itself, those issues are not observed. It's unclear what mechanism is causing
the problem.

Waiting for the display worker to finish completely serializes
render-readback-display, so may cause a big degradation in performance. The
recommended mode is to make the display worker display the previously rendered
frame: this should achieve better performance at the expense of latency of
presentation (and black first frame unless special care is given).


Window Resizing 
---------------

It is not obvious how to receive notifications of window resizing in primus.
At the moment, it simply calls XGetGeometry every frame (without that, the
only Xlib function called in primus would be XOpenDisplay).  VirtualGL
intercepts several resizing-related functions from Xlib, and additionally
tries to deduce drawable size from glViewport calls.

Window Destruction 
------------------

Since display worker runs asynchronously by default, there is a problem that
the window it is drawing to may be destroyed after application regained
control after calling glXSwapBuffers, but before the display worker completed
drawing and called glXSwapBuffers itself. The worker then encounters an X
error and causes the application to terminate. This problem does not arise
when worker threads are synchronized as described above.  How to avoid this
problem while still running the display worker asynchronously?

Multilib
--------

Linux dynamic linker expands $LIB in paths according to current architecture
(since 2002).  This nice feature is used both in dlopen() calls in primus and
the wrapper script.
