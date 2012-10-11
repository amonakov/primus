// primus: quick'n'dirty OpenGL offloading
//
// The purpose is to redirect OpenGL rendering to another X display, and copy
// the rendered frames to the original (as if rendering was not redirected),
// similar to VirtualGL, but with different assumptions and design goals.
//
// primus makes the following assumptions:
//
// * both X servers are local, and direct rendering is available on both
// * the application is "well-behaved" (uses double buffering, does not use
//   color index rendering, does not draw on top of the OpenGL drawable, etc.)
//
// In contrast, VirtualGL:
// * assumes that the primary X display is remote
// * supports obscure OpenGL functionality, making it more versatile
//
// The design goals are:
//
// * simplicity/minimalism:
//   - only GLX functions are overridden
//   - only two Xlib functions are used
// * efficiency:
//   - minimal amount of framebuffer data copying (unfortunately OpenGL does
//     not allow to reduce beyond 2 copies (readback+display))
//   - pipelining of rendering, readback and display
//
// Put another way, VirtualGL is optimized for running arbitrary OpenGL
// applications over a network, with correctness and bandwidth being primary
// concerns, while primus is optimized for running modern games on hybrid
// graphics hardware setups, with simplicity and performance in mind.
//
// Unlike VirtualGL, primus does not rely on LD_PRELOAD mechanism, but instead
// implements a replacement OpenGL library.  This is generally a cleaner
// approach, as it avoids the need to intercept dlopen+dlsym to support
// applications loading libGL.so via dlopen (most SDL apps). The downside is
// that OpenGL functions need to be explicitely forwarded, and some
// applications expecting GL extensions functions to be exported may fail to
// load.

#include <dlfcn.h>
#include <pthread.h>
#include <time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cassert>
#include <map>
#include <sstream>
#include <iomanip>
#include <GL/glx.h>

#define primus_trace(...) do { if (1) fprintf(stderr, "primus: " __VA_ARGS__); } while(0)

// Pointers to implemented/forwarded GLX and OpenGL functions
struct CapturedFns {
  void *handle;
  // Declare functions as fields of the struct
#define DEF_GLX_PROTO(ret, name, args, ...) ret (*name) args;
#include "glx-reimpl.def"
#include "glx-dpyredir.def"
#include "glxext-reimpl.def"
#include "gl-passthru.def"
#include "gl-needed.def"
#undef DEF_GLX_PROTO
  CapturedFns(const char *lib)
  {
    assert(lib && lib[0] == '/');
    handle = dlopen(lib, RTLD_LAZY);
    primus_trace("loading %s\n", lib);
    assert(handle);
#define DEF_GLX_PROTO(ret, name, args, ...) name = (ret (*) args)dlsym(handle, #name);
#include "glx-reimpl.def"
#include "glx-dpyredir.def"
#undef DEF_GLX_PROTO
#define DEF_GLX_PROTO(ret, name, args, ...) name = (ret (*) args)this->glXGetProcAddress((GLubyte*)#name);
#include "glxext-reimpl.def"
#include "gl-passthru.def"
#include "gl-needed.def"
#undef DEF_GLX_PROTO
  }
  ~CapturedFns()
  {
    dlclose(handle);
  }
};

// Drawable tracking info
struct DrawableInfo {
  // Only XWindow is not explicitely created via GLX
  enum {XWindow, Window, Pixmap, Pbuffer} kind;
  GLXFBConfig fbconfig;
  GLXPbuffer  pbuffer;
  int width, height;
};

struct DrawablesInfo: public std::map<GLXDrawable, DrawableInfo> {
  bool known(GLXDrawable draw)
  {
    return this->find(draw) != this->end();
  }
};

// Profiler
class Profiler {
public:
  Profiler(const char *name, const char **state_names) : name(name), state_names(state_names), nstates(0), state(0), nframes(0)
  {
    while (state_names[nstates]) ++nstates; // count number of states
    state_time = new double[nstates];
    memset(state_time, 0, sizeof(double)*nstates);
    // reset time data
    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC, &tp);
    double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
    prev_timestamp = print_timestamp = timestamp;
  }
  ~Profiler() {
    delete [] state_time;
  }
  void tick(bool state_reset = false)
  {
    // update times
    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC, &tp);
    double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
    if (state_reset)
      state = 0;
    state_time[state] += timestamp - prev_timestamp;
    state = (state + 1) % nstates;
    prev_timestamp = timestamp;
    nframes += !!(state == 0);
    // check if it's time to print again
    double period = timestamp - print_timestamp; // time since we printed
    if (state != 0 || period < 5)
      return;
    // construct output
    std::stringstream stream;
    for (int i = 0; i < nstates; ++i) {
      stream << ", " << std::fixed << std::setprecision(1) << (100 * state_time[i] / period) << "% " << state_names[i];
    }
    primus_trace("profiling: %s: %.1f fps%s\n", name, nframes / period, stream.str().c_str());
    // start counting again
    print_timestamp = timestamp;
    nframes = 0;
    memset(state_time, 0, sizeof(double)*nstates);
  }
private:
  const char *name;
  const char **state_names;
  int nstates;

  int state;
  double *state_time;
  double prev_timestamp, print_timestamp;
  int nframes;
};

// Runs before all other initialization takes place
struct EarlyInitializer {
  EarlyInitializer()
  {
#ifdef BUMBLEBEE_SOCKET
    // Signal the Bumblebee daemon to bring up secondary X
    errno = 0;
    int sock = socket(PF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, BUMBLEBEE_SOCKET);
    connect(sock, (struct sockaddr *)&addr, sizeof(addr));
    if (errno)
      perror("connect");
    char c = 'C';
    send(sock, &c, 1, 0);
    recv(sock, &c, 1, 0);
    assert(c == 'Y' && "bumblebeed failed");
    // the socket will be closed when the application quits, then bumblebee will shut down the secondary X
#else
#warning Building without Bumblebee daemon support
#endif
  }
};

// Shorthand for obtaining compile-time configurable value that can be
// overridden by environment
#define getconf(V) (getenv(#V) ? getenv(#V) : V)

// Process-wide data
static struct PrimusInfo {
  EarlyInitializer ei;
  // The "accelerating" X display
  Display *adpy;
  // The "displaying" X display. The same as the application is using, but
  // primus opens its own connection.
  // FIXME: worker threads need to open their own connections as well?
  Display *ddpy;
  // An artifact: primus needs to make symbols from libglapi.so globally
  // visible before loading Mesa
  const void *needed_global;
  CapturedFns afns;
  CapturedFns dfns;
  DrawablesInfo drawables;
  // FIXME: there are race conditions in accesses to these
  std::map<GLXContext, GLXFBConfig> actx2fbconfig;
  std::map<GLXContext, GLXContext> actx2dctx;
  std::map<GLXContext, GLXContext> actx2rctx;

  PrimusInfo():
    adpy(XOpenDisplay(getconf(PRIMUS_DISPLAY))),
    ddpy(XOpenDisplay(NULL)),
    needed_global(dlopen(getconf(PRIMUS_LOAD_GLOBAL), RTLD_LAZY | RTLD_GLOBAL)),
    afns(getconf(PRIMUS_libGLa)),
    dfns(getconf(PRIMUS_libGLd))
  {
    assert(adpy && "failed to open secondary X display");
    assert(needed_global && "failed to load PRIMUS_LOAD_GLOBAL");
    if (afns.handle == dfns.handle && strcmp(getconf(PRIMUS_libGLa), getconf(PRIMUS_libGLd)))
      primus_trace("warning: unexpectedly got same libGL for rendering and display\n");
  }
} primus;

// Thread-specific data
// For each thread that called glXMakeCurrent, primus spawns two additional
// threads: the readback task, and the display task.
static __thread struct TSPrimusInfo {

  static void* dwork(void *vd);

  // Pertaining to the display task
  struct D {
    pthread_t worker;
    pthread_mutex_t dmutex, rmutex;
    int width, height;
    GLvoid *buf;
    Display *dpy;
    GLXDrawable drawable, read_drawable;
    GLXContext context;
    bool reinit;

    void spawn_worker()
    {
      pthread_mutex_init(&dmutex, NULL);
      pthread_mutex_lock(&dmutex);
      pthread_mutex_init(&rmutex, NULL);
      pthread_create(&worker, NULL, dwork, (void*)this);
    }

    void make_current(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext actx)
    {
      if (draw && primus.drawables[draw].kind == DrawableInfo::Pbuffer)
	return;
      if (!worker)
	spawn_worker();
      this->dpy = dpy;
      this->drawable = draw;
      this->read_drawable = read;
      this->context = actx ? primus.actx2dctx[actx] : NULL;
      this->width  = draw ? primus.drawables[draw].width  : 0;
      this->height = draw ? primus.drawables[draw].height : 0;
    }
  } d;

  static void* rwork(void *vr);

  // Pertaining to the readback task
  struct R {
    pthread_t worker;
    pthread_mutex_t amutex, rmutex;
    int width, height;
    GLXDrawable pbuffer;
    GLXContext context;
    // Pointer to the corresponding display task descriptor
    D *pd;
    bool reinit;
    GLsync sync;

    void spawn_worker()
    {
      pthread_mutex_init(&amutex, NULL);
      pthread_mutex_init(&rmutex, NULL);
      pthread_mutex_lock(&rmutex);
      pthread_mutex_lock(&amutex);
      pthread_create(&worker, NULL, rwork, (void*)this);
    }

    void make_current(GLXDrawable draw, GLXContext actx, D *pd)
    {
      if (draw && primus.drawables[draw].kind == DrawableInfo::Pbuffer)
	return;
      if (!worker)
	spawn_worker();
      this->pbuffer = draw ? primus.drawables[draw].pbuffer : 0;
      this->context = actx ? primus.actx2rctx[actx] : NULL;
      this->width   = draw ? primus.drawables[draw].width  : 0;
      this->height  = draw ? primus.drawables[draw].height : 0;
      this->pd      = pd;
      this->reinit  = true;
    }
  } r;
  void make_current(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext actx)
  {
    d.make_current(dpy, draw, read, actx);
    r.make_current(draw, actx, &d);
    pthread_mutex_unlock(&r.rmutex);
    pthread_mutex_lock(&r.amutex);
    if (!draw || !actx)
    {
      assert(!draw && !actx);
      pthread_join(d.worker, NULL);
      pthread_join(r.worker, NULL);
      r.worker = d.worker = 0;
    }
  }
} tsprimus;

void* TSPrimusInfo::dwork(void *vd)
{
  struct D &d = *(D *)vd;
  int width, height;
  static const float quad_vertex_coords[]  = {-1, -1, -1, 1, 1, 1, 1, -1};
  static const float quad_texture_coords[] = { 0,  0,  0, 1, 1, 1, 1,  0};
  static const char *state_names[] = {"wait", "upload", "draw+swap", NULL};
  Profiler profiler("display", state_names);
  for (;;)
  {
    pthread_mutex_lock(&d.dmutex);
    profiler.tick(true);
    if (d.reinit)
    {
      d.reinit = false;
      width = d.width; height = d.height;
      primus.dfns.glXMakeCurrent(primus.ddpy, d.drawable, d.context);
      if (!d.drawable)
      {
	pthread_mutex_unlock(&d.rmutex);
	return NULL;
      }
      primus.dfns.glViewport(0, 0, d.width, d.height);
      primus.dfns.glVertexPointer  (2, GL_FLOAT, 0, quad_vertex_coords);
      primus.dfns.glTexCoordPointer(2, GL_FLOAT, 0, quad_texture_coords);
      primus.dfns.glEnableClientState(GL_VERTEX_ARRAY);
      primus.dfns.glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      GLuint quad_texture;
      primus.dfns.glGenTextures(1, &quad_texture);
      primus.dfns.glBindTexture(GL_TEXTURE_2D, quad_texture);
      primus.dfns.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      primus.dfns.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
      primus.dfns.glEnable(GL_TEXTURE_2D);
      pthread_mutex_unlock(&d.rmutex);
      continue;
    }
    primus.dfns.glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, d.buf);
    profiler.tick();
    primus.dfns.glDrawArrays(GL_QUADS, 0, 4);
    pthread_mutex_unlock(&d.rmutex);
    primus.dfns.glXSwapBuffers(primus.ddpy, d.drawable);
    profiler.tick();
  }
  return NULL;
}

void* TSPrimusInfo::rwork(void *vr)
{
  GLuint pbos[2] = {0};
  int cbuf = 0;
  struct R &r = *(R *)vr;
  static const char *state_names[] = {"app", "map", "wait", NULL};
  Profiler profiler("readback", state_names);
  for (;;)
  {
    pthread_mutex_lock(&r.rmutex);
    profiler.tick(true);
    if (r.reinit)
    {
      r.reinit = false;
      if (pbos[0])
      {
	pthread_mutex_lock(&r.pd->rmutex);
	pthread_mutex_unlock(&r.pd->rmutex);
	primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
	primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
      }
      r.pd->reinit = true;
      pthread_mutex_unlock(&r.pd->dmutex);
      pthread_mutex_lock(&r.pd->rmutex);
      primus.afns.glXMakeCurrent(primus.adpy, r.pbuffer, r.context);
      if (!r.pbuffer)
      {
	pthread_mutex_unlock(&r.amutex);
	return NULL;
      }
      if (!pbos[0])
	primus.afns.glGenBuffers(2, &pbos[0]);
      primus.afns.glReadBuffer(GL_BACK);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[1]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, r.width*r.height*4, NULL, GL_STREAM_READ);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[0]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, r.width*r.height*4, NULL, GL_STREAM_READ);
      pthread_mutex_unlock(&r.amutex);
      continue;
    }
    primus.afns.glWaitSync(r.sync, 0, GL_TIMEOUT_IGNORED);
    primus.afns.glReadPixels(0, 0, r.width, r.height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
    pthread_mutex_unlock(&r.amutex);
    GLvoid *pixeldata = primus.afns.glMapBuffer(GL_PIXEL_PACK_BUFFER_EXT, GL_READ_ONLY);
    profiler.tick();
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    tp.tv_nsec += 20000000;
    tp.tv_sec  += tp.tv_nsec / 1000000000;
    tp.tv_nsec %= 1000000000;
    if (pthread_mutex_timedlock(&r.pd->rmutex, &tp))
    {
      primus_trace("warning: dropping a frame to avoid deadlock\n");
    }
    else
    {
      r.pd->buf = pixeldata;
      pthread_mutex_unlock(&r.pd->dmutex);
      cbuf ^= 1;
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf]);
    }
    primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
    profiler.tick();
  }
  return NULL;
}

// Find appropriate FBConfigs on both adpy and ddpy for a given Visual
static void match_fbconfigs(Display *dpy, XVisualInfo *vis, GLXFBConfig **acfgs, GLXFBConfig **dcfgs)
{
  static int (*dglXGetConfig)(Display*, XVisualInfo*, int, int*)
    = (int (*)(Display*, XVisualInfo*, int, int*)) dlsym(primus.dfns.handle, "glXGetConfig");
  int acfgattrs[] = {
    GLX_DRAWABLE_TYPE,	GLX_WINDOW_BIT,
    GLX_RENDER_TYPE, GLX_RGBA_BIT,
    GLX_DOUBLEBUFFER, GL_TRUE,
    GLX_STEREO, GL_FALSE,
    GLX_AUX_BUFFERS, 0,
    GLX_RED_SIZE, 0,
    GLX_GREEN_SIZE, 0,
    GLX_BLUE_SIZE, 0,
    GLX_ALPHA_SIZE, 0,
    GLX_DEPTH_SIZE, 0,
    GLX_STENCIL_SIZE, 0,
    GLX_ACCUM_RED_SIZE, 0,
    GLX_ACCUM_GREEN_SIZE, 0,
    GLX_ACCUM_BLUE_SIZE, 0,
    GLX_ACCUM_ALPHA_SIZE, 0,
    GLX_SAMPLE_BUFFERS, 0,
    GLX_SAMPLES, 0,
    None
  };
  dglXGetConfig(dpy, vis, GLX_DOUBLEBUFFER, &acfgattrs[5]);
  dglXGetConfig(dpy, vis, GLX_STEREO,       &acfgattrs[7]);
  dglXGetConfig(dpy, vis, GLX_AUX_BUFFERS,  &acfgattrs[9]);
  dglXGetConfig(dpy, vis, GLX_RED_SIZE,     &acfgattrs[11]);
  dglXGetConfig(dpy, vis, GLX_GREEN_SIZE,   &acfgattrs[13]);
  dglXGetConfig(dpy, vis, GLX_BLUE_SIZE,    &acfgattrs[15]);
  dglXGetConfig(dpy, vis, GLX_ALPHA_SIZE,   &acfgattrs[17]);
  dglXGetConfig(dpy, vis, GLX_DEPTH_SIZE,   &acfgattrs[19]);
  dglXGetConfig(dpy, vis, GLX_STENCIL_SIZE, &acfgattrs[21]);
  dglXGetConfig(dpy, vis, GLX_ACCUM_RED_SIZE,   &acfgattrs[23]);
  dglXGetConfig(dpy, vis, GLX_ACCUM_GREEN_SIZE, &acfgattrs[25]);
  dglXGetConfig(dpy, vis, GLX_ACCUM_BLUE_SIZE,  &acfgattrs[27]);
  dglXGetConfig(dpy, vis, GLX_ACCUM_ALPHA_SIZE, &acfgattrs[29]);
  dglXGetConfig(dpy, vis, GLX_SAMPLE_BUFFERS, &acfgattrs[31]);
  dglXGetConfig(dpy, vis, GLX_SAMPLES,        &acfgattrs[33]);
  //assert(acfgattrs[5] && !acfgattrs[7]);
  int ncfg;
  *acfgs = primus.afns.glXChooseFBConfig(primus.adpy, 0, acfgattrs, &ncfg);
  assert(ncfg);
  *dcfgs = primus.dfns.glXChooseFBConfig(dpy, 0, acfgattrs, &ncfg);
  assert(ncfg);
}

GLXContext glXCreateContext(Display *dpy, XVisualInfo *vis, GLXContext shareList, Bool direct)
{
  GLXFBConfig *acfgs, *dcfgs;
  match_fbconfigs(dpy, vis, &acfgs, &dcfgs);
  GLXContext dctx = primus.dfns.glXCreateNewContext(primus.ddpy, *dcfgs, GLX_RGBA_TYPE, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, shareList, direct);
  GLXContext rctx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, actx, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2rctx[actx] = rctx;
  primus.actx2fbconfig[actx] = *acfgs;;
  if (direct && !primus.dfns.glXIsDirect(primus.ddpy, dctx))
    primus_trace("warning: failed to acquire direct rendering context for display thread\n");
  return actx;
}

// Find ddpy FBConfig to draw on.  No depth, stencil or such required.
static GLXFBConfig get_dpy_fbc(Display *dpy, GLXFBConfig acfg)
{
  int dcfgattrs[] = {
    GLX_DRAWABLE_TYPE,	GLX_WINDOW_BIT,
    GLX_RENDER_TYPE, GLX_RGBA_BIT,
    GLX_RED_SIZE, 8, GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8, GLX_ALPHA_SIZE, 8,
    GLX_DOUBLEBUFFER, GL_TRUE,
    None
  };
  // FIXME try to glXGetFBConfigAttrib(acfg) to adjust?
  int ncfg;
  GLXFBConfig *dcfg = primus.dfns.glXChooseFBConfig(dpy, 0, dcfgattrs, &ncfg);
  assert(ncfg);
  return *dcfg;
}

GLXContext glXCreateNewContext(Display *dpy, GLXFBConfig config, int renderType, GLXContext shareList, Bool direct)
{
  GLXContext dctx = primus.dfns.glXCreateNewContext(primus.ddpy, get_dpy_fbc(dpy, config), renderType, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, shareList, direct);
  GLXContext rctx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, actx, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2rctx[actx] = rctx;
  primus.actx2fbconfig[actx] = config;
  if (direct && !primus.dfns.glXIsDirect(primus.ddpy, dctx))
    primus_trace("warning: failed to acquire direct rendering context for display thread\n");
  return actx;
}

void glXDestroyContext(Display *dpy, GLXContext ctx)
{
  //GLXContext dctx = primus.actx2dctx[ctx];
  GLXContext rctx = primus.actx2rctx[ctx];
  primus.actx2dctx.erase(ctx);
  primus.actx2rctx.erase(ctx);
  primus.actx2fbconfig.erase(ctx);
  //FIXME: This causes a BadMatch in SMB
  //primus.dfns.glXDestroyContext(primus.ddpy, dctx);
  primus.afns.glXDestroyContext(primus.adpy, rctx);
  primus.afns.glXDestroyContext(primus.adpy, ctx);
}

// Find out the dimensions of the window
static void note_geometry(Display *dpy, Drawable draw, int *width, int *height)
{
  Window root;
  int x, y;
  unsigned bw, d;
  XGetGeometry(dpy, draw, &root, &x, &y, (unsigned *)width, (unsigned *)height, &bw, &d);
}

// Create or recall backing Pbuffer for the drawable
static GLXPbuffer lookup_pbuffer(Display *dpy, GLXDrawable draw, GLXContext ctx)
{
  if (!draw)
    return 0;
  assert(ctx);
  bool known = primus.drawables.known(draw);
  DrawableInfo &di = primus.drawables[draw];
  if (!known)
  {
    // Drawable is a plain X Window. Get the FBConfig from the context
    GLXFBConfig acfg = primus.actx2fbconfig[ctx];
    assert(acfg);
    di.kind = di.XWindow;
    di.fbconfig = acfg;
    note_geometry(dpy, draw, &di.width, &di.height);
  }
  GLXPbuffer &pbuffer = di.pbuffer;
  if (!pbuffer)
  {
    int pbattrs[] = {GLX_PBUFFER_WIDTH, di.width, GLX_PBUFFER_HEIGHT, di.height, GLX_PRESERVED_CONTENTS, True, None};
    pbuffer = primus.afns.glXCreatePbuffer(primus.adpy, di.fbconfig, pbattrs);
  }
  return pbuffer;
}

Bool glXMakeCurrent(Display *dpy, GLXDrawable drawable, GLXContext ctx)
{
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, drawable, ctx);
  tsprimus.make_current(dpy, drawable, drawable, ctx);
  return primus.afns.glXMakeCurrent(primus.adpy, pbuffer, ctx);
}

Bool glXMakeContextCurrent(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext ctx)
{
  if (draw == read)
    return glXMakeCurrent(dpy, draw, ctx);
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, draw, ctx);
  tsprimus.make_current(dpy, draw, read, ctx);
  GLXPbuffer pb_read = lookup_pbuffer(dpy, read, ctx);
  return primus.afns.glXMakeContextCurrent(primus.adpy, pbuffer, pb_read, ctx);
}

// Recreate the backing Pbuffer and reinitialize workers upon resize
static void update_geometry(Display *dpy, GLXDrawable drawable, DrawableInfo &di)
{
  int w, h;
  note_geometry(dpy, drawable, &w, &h);
  if (w == di.width && h == di.height)
    return;
  primus_trace("recreating backing pbuffer for a resized drawable\n");
  di.width = w; di.height = h;
  primus.afns.glXDestroyPbuffer(primus.adpy, di.pbuffer);
  int pbattrs[] = {GLX_PBUFFER_WIDTH, di.width, GLX_PBUFFER_HEIGHT, di.height, GLX_PRESERVED_CONTENTS, True, None};
  di.pbuffer = primus.afns.glXCreatePbuffer(primus.adpy, di.fbconfig, pbattrs);
  GLXContext actx = glXGetCurrentContext();
  primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, actx);
  tsprimus.make_current(dpy, drawable, drawable, actx);
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  assert(primus.drawables.known(drawable));
  DrawableInfo &di = primus.drawables[drawable];
  if (di.kind == di.Pbuffer)
    return primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  if (di.kind == di.XWindow)
    update_geometry(dpy, drawable, di);
  // Readback thread needs a sync object to avoid reading an incomplete frame
  tsprimus.r.sync = primus.afns.glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
  pthread_mutex_unlock(&tsprimus.r.rmutex);
  pthread_mutex_lock(&tsprimus.r.amutex);
  primus.afns.glDeleteSync(tsprimus.r.sync);
  primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
}

GLXWindow glXCreateWindow(Display *dpy, GLXFBConfig config, Window win, const int *attribList)
{
  GLXWindow glxwin = primus.dfns.glXCreateWindow(primus.ddpy, get_dpy_fbc(dpy, config), win, attribList);
  DrawableInfo &di = primus.drawables[glxwin];
  di.kind = di.Window;
  di.fbconfig = config;
  note_geometry(dpy, win, &di.width, &di.height);
  return glxwin;
}

void glXDestroyWindow(Display *dpy, GLXWindow window)
{
  primus.dfns.glXDestroyWindow(primus.ddpy, window);
  if (primus.drawables[window].pbuffer)
    primus.afns.glXDestroyPbuffer(primus.adpy, primus.drawables[window].pbuffer);
  primus.drawables.erase(window);
}

GLXPbuffer glXCreatePbuffer(Display *dpy, GLXFBConfig config, const int *attribList)
{
  GLXPbuffer pbuffer = primus.afns.glXCreatePbuffer(primus.adpy, config, attribList);
  DrawableInfo &di = primus.drawables[pbuffer];
  di.kind = di.Pbuffer;
  di.fbconfig = config;
  di.pbuffer = pbuffer;
  return pbuffer;
}

void glXDestroyPbuffer(Display *dpy, GLXPbuffer pbuf)
{
  primus.drawables.erase(pbuf);
  primus.afns.glXDestroyPbuffer(primus.adpy, pbuf);
}

GLXPixmap glXCreatePixmap(Display *dpy, GLXFBConfig config, Pixmap pixmap, const int *attribList)
{
  GLXPixmap glxpix = primus.dfns.glXCreatePixmap(dpy, get_dpy_fbc(dpy, config), pixmap, attribList);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  di.fbconfig = config;
  note_geometry(dpy, pixmap, &di.width, &di.height);
  return glxpix;
}

void glXDestroyPixmap(Display *dpy, GLXPixmap pixmap)
{
  primus.dfns.glXDestroyPixmap(dpy, pixmap);
  if (primus.drawables[pixmap].pbuffer)
    primus.afns.glXDestroyPbuffer(primus.adpy, primus.drawables[pixmap].pbuffer);
  primus.drawables.erase(pixmap);
}

GLXPixmap glXCreateGLXPixmap(Display *dpy, XVisualInfo *visual, Pixmap pixmap)
{
  GLXPixmap glxpix = primus.dfns.glXCreateGLXPixmap(primus.ddpy, visual, pixmap);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  note_geometry(dpy, pixmap, &di.width, &di.height);
  GLXFBConfig *acfgs, *dcfgs;
  match_fbconfigs(dpy, visual, &acfgs, &dcfgs);
  di.fbconfig = *acfgs;
  return glxpix;
}

void glXDestroyGLXPixmap(Display *dpy, GLXPixmap pixmap)
{
  glXDestroyPixmap(primus.ddpy, pixmap);
}

XVisualInfo *glXGetVisualFromFBConfig(Display *dpy, GLXFBConfig config)
{
  return primus.dfns.glXGetVisualFromFBConfig(dpy, get_dpy_fbc(dpy, config));
}

void glXQueryDrawable(Display *dpy, GLXDrawable draw, int attribute, unsigned int *value)
{
  assert(primus.drawables.known(draw));
  primus.afns.glXQueryDrawable(primus.adpy, primus.drawables[draw].pbuffer, attribute, value);
}

void glXUseXFont(Font font, int first, int count, int list)
{
  primus_trace("sorry, not implemented: %s\n", __func__);
  for (int i = 0; i < count; i++)
  {
    primus.afns.glNewList(list + i, GL_COMPILE);
    primus.afns.glEndList();
  }
}

GLXContext glXGetCurrentContext(void)
{
  return primus.afns.glXGetCurrentContext();
}

GLXDrawable glXGetCurrentDrawable(void)
{
  return tsprimus.d.drawable;
}

void glXWaitGL(void)
{
}

void glXWaitX(void)
{
}

Display *glXGetCurrentDisplay(void)
{
  return tsprimus.d.dpy;
}

GLXDrawable glXGetCurrentReadDrawable(void)
{
  return tsprimus.d.read_drawable;
}

// Application sees ddpy-side Visuals, but adpy-side FBConfigs and Contexts
XVisualInfo* glXChooseVisual(Display *dpy, int screen, int *attribList)
{
  return primus.dfns.glXChooseVisual(dpy, screen, attribList);
}

int glXGetConfig(Display *dpy, XVisualInfo *visual, int attrib, int *value)
{
  return primus.dfns.glXGetConfig(dpy, visual, attrib, value);
}

// GLX forwarders that reroute to adpy
#define DEF_GLX_PROTO(ret, name, par, ...) \
ret name par \
{ return primus.afns.name(primus.adpy, __VA_ARGS__); }
#include "glx-dpyredir.def"
#undef DEF_GLX_PROTO

// OpenGL forwarders
#define DEF_GLX_PROTO(ret, name, par, ...) \
extern "C" ret name par \
{ return primus.afns.name(__VA_ARGS__); }
#include "gl-passthru.def"
#undef DEF_GLX_PROTO

// GLX extensions

extern "C" int glXSwapIntervalSGI(int interval)
{
  return 1; // Indicate failure to set swapinterval
}

__GLXextFuncPtr glXGetProcAddress(const GLubyte *procName)
{
  static const char * const redefined_names[] = {
#define DEF_GLX_PROTO(ret, name, args, ...) #name,
#include "glx-reimpl.def"
#include "glxext-reimpl.def"
#include "glx-dpyredir.def"
#undef  DEF_GLX_PROTO
  };
  static const __GLXextFuncPtr redefined_fns[] = {
#define DEF_GLX_PROTO(ret, name, args, ...) (__GLXextFuncPtr)name,
#include "glx-reimpl.def"
#include "glxext-reimpl.def"
#include "glx-dpyredir.def"
#undef  DEF_GLX_PROTO
  };
  enum {n_redefined = sizeof(redefined_fns) / sizeof(redefined_fns[0])};
  // Non-GLX functions are forwarded to the accelerating libGL
  if (memcmp(procName, "glX", 3))
    return primus.afns.glXGetProcAddress(procName);
  // All GLX functions are either implemented in primus or not available
  for (int i = 0; i < n_redefined; i++)
    if (!strcmp((const char *)procName, redefined_names[i]))
      return redefined_fns[i];
  return NULL;
}

__GLXextFuncPtr glXGetProcAddressARB(const GLubyte *procName)
{
  return glXGetProcAddress(procName);
}

const char *glXGetClientString(Display *dpy, int name)
{
  switch (name)
  {
    case GLX_VENDOR: return "primus";
    case GLX_VERSION: return "1.4";
    case GLX_EXTENSIONS: return "";
    default: return NULL;
  }
}

const char *glXQueryExtensionsString(Display *dpy, int screen)
{
  return "";
}

// OpenGL ABI specifies that anything above OpenGL 1.2 + ARB_multitexture must
// be obtained via glXGetProcAddress, but some applications link against
// extension functions, and Mesa and vendor libraries let them
#ifndef PRIMUS_STRICT
#warning Enabled workarounds for applications demanding more than promised by the OpenGL ABI
#endif
