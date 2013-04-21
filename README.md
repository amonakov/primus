primus
======

Primus is a shared library that provides OpenGL and GLX APIs and
implements low-overhead
local-only client-side OpenGL offloading via GLX forking, similar to
VirtualGL.  It intercepts GLX calls and redirects GL rendering to a
secondary X display, presumably driven by a faster GPU. On swapping
buffers, rendered contents are read back using a PBO and copied onto
the drawable it was supposed to be rendered on in the first place.
For more information, refer to [technotes.md]
(https://github.com/amonakov/primus/blob/master/technotes.md).

To use, install or build from source and use `primusrun` wrapper script.

In distributions
----------------

* Arch: [primus-git](https://aur.archlinux.org/packages.php?ID=63239)
  and [lib32-primus-git](https://aur.archlinux.org/packages.php?ID=63240)
  in AUR
* Gentoo: `primus-9999.ebuild` in the `bumblebee` overlay
* Ubuntu: in the [Bumblebee PPA](https://launchpad.net/~bumblebee/+archive/stable)

Building for multilib (32-bit + 64-bit) systems
-----------------------------------------------

    LIBDIR=lib make && CXX=g++\ -m32 LIBDIR=lib32 make

Adjust `LIBDIR` variables above as appropriate for your distribution
(reflecting how `/usr/lib*` are named):

* Arch needs `lib` and `lib32` as above
* Gentoo needs `lib64` and `lib32`
* RPM-based may need `lib64` and `lib`
* Debian (with multiarch) needs `lib/x86_64-linux-gnu` and `lib/i386-linux-gnu`
* Ubuntu (with multiarch) seems rather inconsistent.  The dynamic linker
  expands `$LIB` to `x86_64-linux-gnu`/`i386-linux-gnu` (without `lib/`), but
  Nvidia drivers are installed into `/usr/lib{,32}/nvidia-current`. Something
  like the following is needed:

        export PRIMUS_libGLd='/usr/lib/$$LIB/mesa/libGL.so.1'
        LIBDIR=x86_64-linux-gnu make
        LIBDIR=i386-linux-gnu CXX=g++\ -m32 make
        unset PRIMUS_libGLd

  Starting from 13.04, Ubuntu needs the same `LIBDIR` paths as Debian (with
  leading `lib/`); consequently, `lib/` in `PRIMUS_libGLd` should be omitted.

  Furthermore, `libnvidia-tls.so` is not present in default shared library
  search directories.  Uncomment the corresponding line in `primusrun`.

Issues under compositing WMs
----------------------------

Since compositing hurts performance, invoking primus when a compositing WM is
active is not recommended.  If you need to use primus with compositing and see
flickering or bad performance, synchronizing primus' display thread with the
application's rendering thread may help (can anyone investigate why?):

    PRIMUS_SYNC=1 primusrun ...

This makes primus display the previously rendered frame. Alternatively,
with `PRIMUS_SYNC=2` primus will display the latest rendered frame, trading
frame rate for reduced visual latency.

FAQ
---

Q: Performance does not exceed 60 fps, I was getting more with optirun/VirtualGL.  
A: This is the effect of vblank synchronisation. For benchmarking, you can use
`vblank_mode=0 primusrun ...`, but in practice this will probably only waste power,
as your LCD panel does not display more than 60 frames per second anyway.
