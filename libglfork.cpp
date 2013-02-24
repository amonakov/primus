#include <dlfcn.h>
#include <pthread.h>
#include <semaphore.h>
#include <time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cassert>
#include <map>
#include <X11/Xatom.h>
#pragma GCC visibility push(default)
#define GLX_GLXEXT_PROTOTYPES
#define GL_GLEXT_PROTOTYPES
#include <GL/glx.h>
#pragma GCC visibility pop

#define primus_print(c, ...) do { if (c) fprintf(stderr, "primus: " __VA_ARGS__); } while (0)

#define die_if(cond, ...)  do {if (cond) {primus_print(true, "fatal: " __VA_ARGS__); exit(1);} } while (0)
#define primus_warn(...) primus_print(primus.loglevel >= 1, "warning: " __VA_ARGS__)
#define primus_perf(...) primus_print(primus.loglevel >= 2, "profiling: " __VA_ARGS__)

// Try to load any of the colon-separated libraries
static void *mdlopen(const char *paths, int flag)
{
  char *p = strdupa(paths);
  char errors[1024], *errors_ptr = errors, *errors_end = errors + 1024;
  for (char *c = p; c; p = c + 1)
  {
    if ((c = strchr(p, ':')))
      *c = 0;
    die_if(p[0] != '/', "need absolute library path: %s\n", p);
    void *handle = dlopen(p, flag);
    if (handle)
      return handle;
    errors_ptr += snprintf(errors_ptr, errors_end - errors_ptr, "%s\n", dlerror());
  }
  die_if(true, "failed to load any of the libraries: %s\n%s", paths, errors);
}

static void *real_dlsym(void *handle, const char *symbol)
{
  typedef void* (*dlsym_fn)(void *, const char*);
  static dlsym_fn pdlsym = (dlsym_fn) dlsym(RTLD_DEFAULT, "dlsym");
  return pdlsym(handle, symbol);
}

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
    handle = mdlopen(lib, RTLD_LAZY);
#define DEF_GLX_PROTO(ret, name, args, ...) name = (ret (*) args)real_dlsym(handle, #name);
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
  Drawable window;
  int width, height;
  GLvoid *pixeldata;
  GLsync sync;
  GLXContext actx;

  struct {
    pthread_t worker;
    sem_t acqsem, relsem;
    bool reinit;

    void spawn_worker(GLXDrawable draw, void* (*work)(void*))
    {
      reinit = true;
      sem_init(&acqsem, 0, 0);
      sem_init(&relsem, 0, 0);
      pthread_create(&worker, NULL, work, (void*)draw);
    }
    void reap_worker()
    {
      //pthread_cancel(worker);
      pthread_join(worker, NULL);
      sem_destroy(&relsem);
      sem_destroy(&acqsem);
      worker = 0;
    }
  } r, d;
  void reap_workers()
  {
    if (r.worker)
    {
      width = -1;
      r.reinit = true;
      sem_post(&r.acqsem);
      sem_wait(&r.relsem);
      r.reap_worker();
      d.reap_worker();
    }
  }
  ~DrawableInfo();
};

struct DrawablesInfo: public std::map<GLXDrawable, DrawableInfo> {
  bool known(GLXDrawable draw)
  {
    return this->find(draw) != this->end();
  }
};

#define stringify(s) #s
// Shorthand for obtaining compile-time configurable value that can be
// overridden by environment
#define getconf(V) (getenv(#V) ? getenv(#V) : stringify(V))

// Runs before all other initialization takes place
struct EarlyInitializer {
  EarlyInitializer()
  {
#ifdef BUMBLEBEE_SOCKET
    // Signal the Bumblebee daemon to bring up secondary X
    errno = 0;
    int sock = socket(PF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, getconf(BUMBLEBEE_SOCKET), sizeof(addr.sun_path));
    connect(sock, (struct sockaddr *)&addr, sizeof(addr));
    if (errno)
      perror("connect");
    static char c[256] = "C";
    send(sock, &c, 1, 0);
    recv(sock, &c, 255, 0);
    die_if(c[0] == 'N', "Bumblebee daemon reported: %s\n", c + 5);
    die_if(c[0] != 'Y', "failure contacting Bumblebee daemon\n");
    // the socket will be closed when the application quits, then bumblebee will shut down the secondary X
#else
#warning Building without Bumblebee daemon support
#endif
  }
};

// Process-wide data
static struct PrimusInfo {
  EarlyInitializer ei;
  // Readback-display synchronization method
  // 0: no sync, 1: D lags behind one frame, 2: fully synced
  int sync;
  // 0: only errors, 1: warnings, 2: profiling
  int loglevel;
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

  PrimusInfo():
    sync(atoi(getconf(PRIMUS_SYNC))),
    loglevel(atoi(getconf(PRIMUS_VERBOSE))),
    adpy(XOpenDisplay(getconf(PRIMUS_DISPLAY))),
    ddpy(XOpenDisplay(NULL)),
    needed_global(dlopen(getconf(PRIMUS_LOAD_GLOBAL), RTLD_LAZY | RTLD_GLOBAL)),
    afns(getconf(PRIMUS_libGLa)),
    dfns(getconf(PRIMUS_libGLd))
  {
    die_if(!adpy, "failed to open secondary X display\n");
    die_if(!needed_global, "failed to load PRIMUS_LOAD_GLOBAL\n");
  }
} primus;

// Thread-specific data
static __thread struct {
  Display *dpy;
  GLXDrawable drawable, read_drawable;
  void make_current(Display *dpy, GLXDrawable draw, GLXDrawable read)
  {
    this->dpy = dpy;
    this->drawable = draw;
    this->read_drawable = read;
  }
} tsdata;

// Profiler
class Profiler {
  const char *name;
  const char * const *state_names;
  int nstates;

  int state;
  double *state_time;
  double prev_timestamp, print_timestamp;
  int nframes;
public:
  Profiler(const char *name, const char * const *state_names):
    name(name),
    state_names(state_names),
    nstates(0), state(0), nframes(0)
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
  ~Profiler()
  {
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
    char buf[64], *cbuf = buf, *end = buf+64;
    for (int i = 0; i < nstates; i++)
      cbuf += snprintf(cbuf, end - cbuf, ", %.1f%% %s", 100 * state_time[i] / period, state_names[i]);
    primus_perf("%s: %.1f fps%s\n", name, nframes / period, buf);
    // start counting again
    print_timestamp = timestamp;
    nframes = 0;
    memset(state_time, 0, sizeof(double)*nstates);
  }
};

static GLXFBConfig get_dpy_fbc(Display *dpy, GLXFBConfig acfg);

static void* display_work(void *vd)
{
  GLXDrawable drawable = (GLXDrawable)vd;
  DrawableInfo &di = primus.drawables[drawable];
  int width, height;
  static const float quad_vertex_coords[]  = {-1, -1, -1, 1, 1, 1, 1, -1};
  static const float quad_texture_coords[] = { 0,  0,  0, 1, 1, 1, 1,  0};
  GLuint quad_texture = 0;
  static const char *state_names[] = {"wait", "upload", "draw+swap", NULL};
  Profiler profiler("display", state_names);
  GLXContext context = primus.dfns.glXCreateNewContext(primus.ddpy, get_dpy_fbc(primus.ddpy, di.fbconfig), GLX_RGBA_TYPE, NULL, True);
  die_if(!primus.dfns.glXIsDirect(primus.ddpy, context),
	 "failed to acquire direct rendering context for display thread\n");
  primus.dfns.glXMakeCurrent(primus.ddpy, drawable, context);
  primus.dfns.glVertexPointer  (2, GL_FLOAT, 0, quad_vertex_coords);
  primus.dfns.glTexCoordPointer(2, GL_FLOAT, 0, quad_texture_coords);
  primus.dfns.glEnableClientState(GL_VERTEX_ARRAY);
  primus.dfns.glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  primus.dfns.glGenTextures(1, &quad_texture);
  primus.dfns.glBindTexture(GL_TEXTURE_2D, quad_texture);
  primus.dfns.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  primus.dfns.glEnable(GL_TEXTURE_2D);
  for (;;)
  {
    sem_wait(&di.d.acqsem);
    profiler.tick(true);
    if (di.d.reinit)
    {
      di.d.reinit = false;
      width = di.width; height = di.height;
      if (width == -1)
      {
	primus.dfns.glDeleteTextures(1, &quad_texture);
	primus.dfns.glXMakeCurrent(primus.ddpy, 0, NULL);
	primus.dfns.glXDestroyContext(primus.ddpy, context);
	sem_post(&di.d.relsem);
	return NULL;
      }
      primus.dfns.glViewport(0, 0, width, height);
      primus.dfns.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
      sem_post(&di.d.relsem);
      continue;
    }
    primus.dfns.glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, di.pixeldata);
    if (!primus.sync)
      sem_post(&di.d.relsem); // Unlock as soon as possible
    profiler.tick();
    primus.dfns.glDrawArrays(GL_QUADS, 0, 4);
    primus.dfns.glXSwapBuffers(primus.ddpy, drawable);
    if (primus.sync)
      sem_post(&di.d.relsem); // Unlock only after drawing
    profiler.tick();
  }
  return NULL;
}

static void* readback_work(void *vd)
{
  GLXDrawable drawable = (GLXDrawable)vd;
  DrawableInfo &di = primus.drawables[drawable];
  int width, height;
  GLuint pbos[2] = {0};
  int cbuf = 0;
  static const char *state_names[] = {"app", "map", "wait", NULL};
  Profiler profiler("readback", state_names);
  struct timespec tp;
  if (!primus.sync)
    sem_post(&di.d.relsem); // No PBO is mapped initially
  GLXContext context = primus.afns.glXCreateNewContext(primus.adpy, di.fbconfig, GLX_RGBA_TYPE, di.actx, True);
  die_if(!primus.afns.glXIsDirect(primus.adpy, context),
	 "failed to acquire direct rendering context for readback thread\n");
  primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, context);
  primus.afns.glGenBuffers(2, &pbos[0]);
  primus.afns.glReadBuffer(GL_BACK);
  for (;;)
  {
    sem_wait(&di.r.acqsem);
    profiler.tick(true);
    if (di.r.reinit)
    {
      di.r.reinit = false;
      width = di.width; height = di.height;
      clock_gettime(CLOCK_REALTIME, &tp);
      tp.tv_sec  += 1;
      // Wait for D worker, if active
      if (!primus.sync && sem_timedwait(&di.d.relsem, &tp))
      {
	pthread_cancel(di.d.worker);
	primus_warn("killed a worker to proceed\n");
	sem_post(&di.d.relsem); // Pretend that D worker completed reinit
	assert(width == -1);    // Cannot proceed without the buddy
      }
      di.d.reinit = true;
      sem_post(&di.d.acqsem); // Signal D worker to reinit
      sem_wait(&di.d.relsem); // Wait until reinit was completed
      if (!primus.sync)
	sem_post(&di.d.relsem); // Unlock as no PBO is currently mapped
      if (width == -1)
      {
	primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
	primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
	primus.afns.glDeleteBuffers(2, &pbos[0]);
	primus.afns.glXMakeCurrent(primus.adpy, 0, NULL);
	primus.afns.glXDestroyContext(primus.adpy, context);
	sem_post(&di.r.relsem);
	return NULL;
      }
      primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, context);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
    }
    primus.afns.glWaitSync(di.sync, 0, GL_TIMEOUT_IGNORED);
    primus.afns.glReadPixels(0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
    if (!primus.sync)
      sem_post(&di.r.relsem); // Unblock main thread as soon as possible
    if (primus.sync == 1) // Get the previous framebuffer
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
    GLvoid *pixeldata = primus.afns.glMapBuffer(GL_PIXEL_PACK_BUFFER_EXT, GL_READ_ONLY);
    profiler.tick();
    clock_gettime(CLOCK_REALTIME, &tp);
    tp.tv_sec  += 1;
    if (!primus.sync && sem_timedwait(&di.d.relsem, &tp))
      primus_warn("dropping a frame to avoid deadlock\n");
    else
    {
      di.pixeldata = pixeldata;
      sem_post(&di.d.acqsem);
      if (primus.sync)
      {
	sem_wait(&di.d.relsem);
	sem_post(&di.r.relsem); // Unblock main thread only after D::work has completed
      }
      cbuf ^= 1;
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf]);
    }
    primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
    profiler.tick();
  }
  return NULL;
}

// Find appropriate FBConfigs on both adpy and ddpy for a given Visual
static GLXFBConfig* match_fbconfig(Display *dpy, XVisualInfo *vis)
{
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
  glXGetConfig(dpy, vis, GLX_DOUBLEBUFFER, &acfgattrs[5]);
  glXGetConfig(dpy, vis, GLX_STEREO,       &acfgattrs[7]);
  glXGetConfig(dpy, vis, GLX_AUX_BUFFERS,  &acfgattrs[9]);
  glXGetConfig(dpy, vis, GLX_RED_SIZE,     &acfgattrs[11]);
  glXGetConfig(dpy, vis, GLX_GREEN_SIZE,   &acfgattrs[13]);
  glXGetConfig(dpy, vis, GLX_BLUE_SIZE,    &acfgattrs[15]);
  glXGetConfig(dpy, vis, GLX_ALPHA_SIZE,   &acfgattrs[17]);
  glXGetConfig(dpy, vis, GLX_DEPTH_SIZE,   &acfgattrs[19]);
  glXGetConfig(dpy, vis, GLX_STENCIL_SIZE, &acfgattrs[21]);
  glXGetConfig(dpy, vis, GLX_ACCUM_RED_SIZE,   &acfgattrs[23]);
  glXGetConfig(dpy, vis, GLX_ACCUM_GREEN_SIZE, &acfgattrs[25]);
  glXGetConfig(dpy, vis, GLX_ACCUM_BLUE_SIZE,  &acfgattrs[27]);
  glXGetConfig(dpy, vis, GLX_ACCUM_ALPHA_SIZE, &acfgattrs[29]);
  glXGetConfig(dpy, vis, GLX_SAMPLE_BUFFERS, &acfgattrs[31]);
  glXGetConfig(dpy, vis, GLX_SAMPLES,        &acfgattrs[33]);
  //assert(acfgattrs[5] && !acfgattrs[7]);
  int ncfg;
  return primus.afns.glXChooseFBConfig(primus.adpy, 0, acfgattrs, &ncfg);
}

GLXContext glXCreateContext(Display *dpy, XVisualInfo *vis, GLXContext shareList, Bool direct)
{
  GLXFBConfig *acfgs = match_fbconfig(dpy, vis);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, shareList, direct);
  primus.actx2fbconfig[actx] = *acfgs;
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
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, shareList, direct);
  primus.actx2fbconfig[actx] = config;
  return actx;
}

void glXDestroyContext(Display *dpy, GLXContext ctx)
{
  primus.actx2fbconfig.erase(ctx);
  // kludge: reap background tasks when deleting the last context
  // otherwise something will deadlock during unloading the library
  if (primus.actx2fbconfig.empty())
    for (DrawablesInfo::iterator i = primus.drawables.begin(); i != primus.drawables.end(); i++)
      i->second.reap_workers();
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

static GLXPbuffer create_pbuffer(DrawableInfo &di)
{
  int pbattrs[] = {GLX_PBUFFER_WIDTH, di.width, GLX_PBUFFER_HEIGHT, di.height, GLX_PRESERVED_CONTENTS, True, None};
  return primus.afns.glXCreatePbuffer(primus.adpy, di.fbconfig, pbattrs);
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
    di.window = draw;
    note_geometry(dpy, draw, &di.width, &di.height);
  }
  if (!di.pbuffer)
    di.pbuffer = create_pbuffer(di);
  return di.pbuffer;
}

Bool glXMakeCurrent(Display *dpy, GLXDrawable drawable, GLXContext ctx)
{
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, drawable, ctx);
  tsdata.make_current(dpy, drawable, drawable);
  return primus.afns.glXMakeCurrent(primus.adpy, pbuffer, ctx);
}

Bool glXMakeContextCurrent(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext ctx)
{
  if (draw == read)
    return glXMakeCurrent(dpy, draw, ctx);
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, draw, ctx);
  tsdata.make_current(dpy, draw, read);
  GLXPbuffer pb_read = lookup_pbuffer(dpy, read, ctx);
  return primus.afns.glXMakeContextCurrent(primus.adpy, pbuffer, pb_read, ctx);
}

// Recreate the backing Pbuffer and reinitialize workers upon resize
static void update_geometry(Display *dpy, GLXDrawable drawable, DrawableInfo &di)
{
  int w, h;
  note_geometry(dpy, di.window, &w, &h);
  if (w == di.width && h == di.height)
    return;
  di.width = w; di.height = h;
  primus.afns.glXDestroyPbuffer(primus.adpy, di.pbuffer);
  di.pbuffer = create_pbuffer(di);
  GLXContext actx = glXGetCurrentContext();
  primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, actx);
  di.r.reinit = true;
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  assert(primus.drawables.known(drawable));
  DrawableInfo &di = primus.drawables[drawable];
  if (di.kind == di.Pbuffer)
    return primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  if (di.r.worker && di.actx != glXGetCurrentContext())
  {
    primus_warn("glXSwapBuffers: respawning threads after context change\n");
    di.reap_workers();
  }
  if (di.kind == di.XWindow || di.kind == di.Window)
    update_geometry(dpy, drawable, di);
  if (!di.r.worker)
  {
    // Need to create a sharing context to use GL sync objects
    di.actx = glXGetCurrentContext();
    if (!di.actx)
      primus_warn("glXSwapBuffers: no current context\n");
    di.d.spawn_worker(drawable, display_work);
    di.r.spawn_worker(drawable, readback_work);
  }
  // Readback thread needs a sync object to avoid reading an incomplete frame
  di.sync = primus.afns.glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
  sem_post(&di.r.acqsem); // Signal the readback worker thread
  sem_wait(&di.r.relsem); // Wait until it has issued glReadBuffer
  primus.afns.glDeleteSync(di.sync);
  primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
}

GLXWindow glXCreateWindow(Display *dpy, GLXFBConfig config, Window win, const int *attribList)
{
  GLXWindow glxwin = primus.dfns.glXCreateWindow(primus.ddpy, get_dpy_fbc(dpy, config), win, attribList);
  DrawableInfo &di = primus.drawables[glxwin];
  di.kind = di.Window;
  di.fbconfig = config;
  di.window = win;
  note_geometry(dpy, win, &di.width, &di.height);
  return glxwin;
}

DrawableInfo::~DrawableInfo()
{
  reap_workers();
  if (pbuffer)
    primus.afns.glXDestroyPbuffer(primus.adpy, pbuffer);
}

void glXDestroyWindow(Display *dpy, GLXWindow window)
{
  assert(primus.drawables.known(window));
  primus.drawables.erase(window);
  primus.dfns.glXDestroyWindow(primus.ddpy, window);
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
  assert(primus.drawables.known(pixmap));
  primus.drawables.erase(pixmap);
  primus.dfns.glXDestroyPixmap(dpy, pixmap);
}

GLXPixmap glXCreateGLXPixmap(Display *dpy, XVisualInfo *visual, Pixmap pixmap)
{
  GLXPixmap glxpix = primus.dfns.glXCreateGLXPixmap(primus.ddpy, visual, pixmap);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  note_geometry(dpy, pixmap, &di.width, &di.height);
  GLXFBConfig *acfgs = match_fbconfig(dpy, visual);
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

int glXGetFBConfigAttrib(Display *dpy, GLXFBConfig config, int attribute, int *value)
{
  if (attribute == GLX_VISUAL_ID)
    return primus.dfns.glXGetFBConfigAttrib(dpy, get_dpy_fbc(dpy, config), attribute, value);
  return primus.afns.glXGetFBConfigAttrib(primus.adpy, config, attribute, value);
}

void glXQueryDrawable(Display *dpy, GLXDrawable draw, int attribute, unsigned int *value)
{
  assert(primus.drawables.known(draw));
  primus.afns.glXQueryDrawable(primus.adpy, primus.drawables[draw].pbuffer, attribute, value);
}

void glXUseXFont(Font font, int first, int count, int list)
{
  unsigned long prop;
  XFontStruct *fs = XQueryFont(primus.ddpy, font);
  XGetFontProperty(fs, XA_FONT, &prop);
  char *xlfd = XGetAtomName(primus.ddpy, prop);
  Font afont = XLoadFont(primus.adpy, xlfd);
  primus.afns.glXUseXFont(afont, first, count, list);
  XUnloadFont(primus.adpy, afont);
  XFree(xlfd);
  XFreeFontInfo(NULL, fs, 1);
}

GLXContext glXGetCurrentContext(void)
{
  return primus.afns.glXGetCurrentContext();
}

GLXDrawable glXGetCurrentDrawable(void)
{
  return tsdata.drawable;
}

void glXWaitGL(void)
{
}

void glXWaitX(void)
{
}

Display *glXGetCurrentDisplay(void)
{
  return tsdata.dpy;
}

GLXDrawable glXGetCurrentReadDrawable(void)
{
  return tsdata.read_drawable;
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
void ifunc_##name(void) asm(#name) __attribute__((visibility("default"),ifunc("i" #name))); \
extern "C" { \
static ret l##name par \
{ return primus.afns.name(__VA_ARGS__); } \
static void *i##name(void) \
{ return primus.afns.handle ? real_dlsym(primus.afns.handle, #name) : (void*)l##name; } }
#include "gl-passthru.def"
#undef DEF_GLX_PROTO

// GLX extensions

int glXSwapIntervalSGI(int interval)
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
    case GLX_EXTENSIONS: return "GLX_ARB_get_proc_address ";
    default: return NULL;
  }
}

const char *glXQueryExtensionsString(Display *dpy, int screen)
{
  return "GLX_ARB_get_proc_address ";
}

// OpenGL ABI specifies that anything above OpenGL 1.2 + ARB_multitexture must
// be obtained via glXGetProcAddress, but some applications link against
// extension functions, and Mesa and vendor libraries let them
#ifndef PRIMUS_STRICT
#warning Enabled workarounds for applications demanding more than promised by the OpenGL ABI

// OpenGL extension forwarders
#define P(name) \
void ifunc_##name(void) asm(#name) __attribute__((visibility("default"),ifunc("i" #name))); \
extern "C" { static void *i##name(void) \
{ return primus.afns.handle ? real_dlsym(primus.afns.handle, #name) : NULL; } }
#include "glext-passthru.def"
#undef P
#endif
