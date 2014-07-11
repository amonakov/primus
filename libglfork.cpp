#include <dlfcn.h>
#include <pthread.h>
#include <semaphore.h>
#include <time.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cassert>
#include <map>
#include <string>
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
  static dlsym_fn pdlsym = (dlsym_fn) dlsym(dlopen("libdl.so.2", RTLD_LAZY), "dlsym");
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
  enum ReinitTodo {NONE, RESIZE, SHUTDOWN} reinit;
  GLvoid *pixeldata;
  GLsync sync;
  GLXContext actx;

  struct {
    pthread_t worker;
    sem_t acqsem, relsem;
    ReinitTodo reinit;

    void spawn_worker(GLXDrawable draw, void* (*work)(void*))
    {
      reinit = RESIZE;
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
      r.reinit = SHUTDOWN;
      sem_post(&r.acqsem);
      sem_wait(&r.relsem);
      r.reap_worker();
      d.reap_worker();
    }
  }
  void update_geometry(int width, int height)
  {
    if (this->width == width && this->height == height)
      return;
    this->width = width; this->height = height;
    __sync_synchronize();
    reinit = RESIZE;
  }
  ~DrawableInfo();
};

struct DrawablesInfo: public std::map<GLXDrawable, DrawableInfo> {
  bool known(GLXDrawable draw)
  {
    return this->find(draw) != this->end();
  }
};

struct ContextInfo {
  GLXFBConfig fbconfig;
  int sharegroup;
};

struct ContextsInfo: public std::map<GLXContext, ContextInfo> {
  void record(GLXContext ctx, GLXFBConfig config, GLXContext share)
  {
    static int nsharegroups;
    int sharegroup = share ? (*this)[share].sharegroup : nsharegroups++;
    (*this)[ctx] = (ContextInfo){config, sharegroup};
  }
};

// Shorthand for obtaining compile-time configurable value that can be
// overridden by environment
#define getconf(V) (getenv(#V) ? getenv(#V) : V)

// Runs before all other initialization takes place
struct EarlyInitializer {
  EarlyInitializer(const char **adpy_strp, const char **libgla_strp)
  {
#ifdef BUMBLEBEE_SOCKET
    // Signal the Bumblebee daemon to bring up secondary X
    errno = 0;
    int sock = socket(PF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, getconf(BUMBLEBEE_SOCKET), sizeof(addr.sun_path));
    connect(sock, (struct sockaddr *)&addr, sizeof(addr));
    die_if(errno, "failed to connect to Bumblebee daemon: %s\n", strerror(errno));
    static char c[256];
    if (!getenv("PRIMUS_DISPLAY"))
    {
      send(sock, "Q VirtualDisplay", strlen("Q VirtualDisplay") + 1, 0);
      recv(sock, &c, 255, 0);
      die_if(memcmp(c, "Value: ", strlen("Value: ")), "unexpected query response\n");
      *strchrnul(c, '\n') = 0;
      *adpy_strp = strdup(c + 7);
    }
    if (!getenv("PRIMUS_libGLa"))
    {
      send(sock, "Q LibraryPath", strlen("Q LibraryPath") + 1, 0);
      recv(sock, &c, 255, 0);
      die_if(memcmp(c, "Value: ", strlen("Value: ")), "unexpected query response\n");
      *strchrnul(c, '\n') = 0;
      int npaths = 0;
      for (char *p = c + 7; *p; npaths++, p = strchrnul(p + 1, ':'));
      if (npaths)
      {
	char *bblibs = new char[strlen(c + 7) + npaths * strlen("/libGL.so.1") + 1], *b = bblibs, *n, *p;
	for (p = c + 7; *p; p = n)
	{
	  n = strchrnul(p + 1, ':');
	  b += sprintf(b, "%.*s/libGL.so.1", (int)(n - p), p);
	}
	*libgla_strp = bblibs;
      }
    }
    send(sock, "C", 1, 0);
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
  const char *adpy_str, *libgla_str;
  EarlyInitializer ei;
  // Readback-display synchronization method
  // 0: no sync, 1: D lags behind one frame, 2: fully synced
  int sync;
  // 0: only errors, 1: warnings, 2: profiling
  int loglevel;
  // 0: autodetect, 1: texture, 2: PBO glDrawPixels
  int dispmethod;
  // sleep ratio in readback thread, percent
  int autosleep;
  // The "accelerating" X display
  Display *adpy;
  // The "displaying" X display. The same as the application is using, but
  // primus opens its own connection.
  Display *ddpy;
  // An artifact: primus needs to make symbols from libglapi.so globally
  // visible before loading Mesa
  const void *needed_global;
  CapturedFns afns;
  CapturedFns dfns;
  // FIXME: there are race conditions in accesses to these
  DrawablesInfo drawables;
  ContextsInfo contexts;
  GLXFBConfig *dconfigs;

  PrimusInfo():
    adpy_str(getconf(PRIMUS_DISPLAY)),
    libgla_str(getconf(PRIMUS_libGLa)),
    ei(&adpy_str, &libgla_str),
    sync(atoi(getconf(PRIMUS_SYNC))),
    loglevel(atoi(getconf(PRIMUS_VERBOSE))),
    dispmethod(atoi(getconf(PRIMUS_UPLOAD))),
    autosleep(atoi(getconf(PRIMUS_SLEEP))),
    adpy(XOpenDisplay(adpy_str)),
    ddpy(XOpenDisplay(NULL)),
    needed_global(dlopen(getconf(PRIMUS_LOAD_GLOBAL), RTLD_LAZY | RTLD_GLOBAL)),
    afns(libgla_str),
    dfns(getconf(PRIMUS_libGLd))
  {
    die_if(!adpy, "failed to open secondary X display\n");
    die_if(!ddpy, "failed to open main X display\n");
    die_if(!needed_global, "failed to load PRIMUS_LOAD_GLOBAL\n");
    int ncfg, attrs[] = {GLX_DOUBLEBUFFER, GL_TRUE, None};
    dconfigs = dfns.glXChooseFBConfig(ddpy, 0, attrs, &ncfg);
    die_if(!ncfg, "broken GLX on main X display\n");
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
struct Profiler {
  const char *name;
  const char * const *state_names;

  double state_time[6], prev_timestamp, print_timestamp;
  int state, nframes, width, height;

  static double get_timestamp()
  {
    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC, &tp);
    return tp.tv_sec + 1e-9 * tp.tv_nsec;
  }

  Profiler(const char *name, const char * const *state_names):
    name(name),
    state_names(state_names),
    state(0), nframes(0), width(0), height(0)
  {
    memset(state_time, 0, sizeof(state_time));
    prev_timestamp = print_timestamp = get_timestamp();
  }

  void tick(bool state_reset = false)
  {
    if (primus.loglevel < 2)
      return;
    double timestamp = get_timestamp();
    assert(state_reset || state_names[state]);
    if (state_reset)
      state = 0;
    assert(state * sizeof(state_time[0]) < sizeof(state_time));
    state_time[state++] += timestamp - prev_timestamp;
    prev_timestamp = timestamp;
    if (state_names[state])
      return;
    nframes++;
    // check if it's time to print again
    double period = timestamp - print_timestamp; // time since we printed
    if (period < 5)
      return;
    // construct output
    char buf[128], *cbuf = buf, *end = buf+128;
    for (int i = 0; i < state; i++)
      cbuf += snprintf(cbuf, end - cbuf, ", %.1f%% %s", 100 * state_time[i] / period, state_names[i]);
    primus_perf("%s: %dx%d, %.1f fps%s\n", name, width, height, nframes / period, buf);
    // start counting again
    print_timestamp = timestamp;
    nframes = 0;
    memset(state_time, 0, sizeof(state_time));
  }
};

// Find out the dimensions of the window
static void note_geometry(Display *dpy, Drawable draw, int *width, int *height)
{
  Window root;
  int x, y;
  unsigned bw, d;
  XGetGeometry(dpy, draw, &root, &x, &y, (unsigned *)width, (unsigned *)height, &bw, &d);
}

static bool test_drawpixels_fast(Display *dpy, GLXContext ctx)
{
  int width = 1920, height = 1080;
  int pbattrs[] = {GLX_PBUFFER_WIDTH, width, GLX_PBUFFER_HEIGHT, height, GLX_PRESERVED_CONTENTS, True, None};
  GLXPbuffer pbuffer = primus.dfns.glXCreatePbuffer(dpy, primus.dconfigs[0], pbattrs);
  primus.dfns.glXMakeCurrent(dpy, pbuffer, ctx);
  GLuint pbo;
  primus.dfns.glGenBuffers(1, &pbo);
  primus.dfns.glBindBuffer(GL_PIXEL_UNPACK_BUFFER_EXT, pbo);
  primus.dfns.glBufferData(GL_PIXEL_UNPACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_DRAW);
  void *pixeldata = malloc(width*height*4);

  double end = 0.2 + Profiler::get_timestamp();
  int iters = 0;
  do {
    primus.dfns.glBufferSubData(GL_PIXEL_UNPACK_BUFFER_EXT, 0, width*height*4, pixeldata);
    primus.dfns.glDrawPixels(width, height, GL_BGRA, GL_UNSIGNED_BYTE, NULL);
    primus.dfns.glXSwapBuffers(dpy, pbuffer);
    iters++;
  } while (end > Profiler::get_timestamp());

  free(pixeldata);
  primus.dfns.glDeleteBuffers(1, &pbo);
  primus.dfns.glXDestroyPbuffer(dpy, pbuffer);

  bool is_fast = iters >= 12;
  primus_perf("upload autodetection: will use %s path (%d iters)\n", is_fast ? "PBO" : "texture", iters);
  return is_fast;
}

static void* display_work(void *vd)
{
  GLXDrawable drawable = (GLXDrawable)vd;
  DrawableInfo &di = primus.drawables[drawable];
  int width, height;
  static const float quad_vertex_coords[]  = {-1, -1, -1, 1, 1, 1, 1, -1};
  static const float quad_texture_coords[] = { 0,  0,  0, 1, 1, 1, 1,  0};
  GLuint textures[2] = {0}, pbos[2] = {0};
  int ctex = 0;
  static const char *state_names[] = {"wait", "upload", "draw+swap", NULL};
  Profiler profiler("display", state_names);
  Display *ddpy = XOpenDisplay(NULL);
  assert(di.kind == di.XWindow || di.kind == di.Window);
  XSelectInput(ddpy, di.window, StructureNotifyMask);
  note_geometry(ddpy, di.window, &width, &height);
  di.update_geometry(width, height);
  GLXContext context = primus.dfns.glXCreateNewContext(ddpy, primus.dconfigs[0], GLX_RGBA_TYPE, NULL, True);
  die_if(!primus.dfns.glXIsDirect(ddpy, context),
	 "failed to acquire direct rendering context for display thread\n");
  if (!primus.dispmethod)
    primus.dispmethod = test_drawpixels_fast(ddpy, context) ? 2 : 1;
  primus.dfns.glXMakeCurrent(ddpy, di.window, context);
  bool use_textures = (primus.dispmethod == 1);
  if (use_textures)
  {
    primus.dfns.glVertexPointer  (2, GL_FLOAT, 0, quad_vertex_coords);
    primus.dfns.glTexCoordPointer(2, GL_FLOAT, 0, quad_texture_coords);
    primus.dfns.glEnableClientState(GL_VERTEX_ARRAY);
    primus.dfns.glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    primus.dfns.glGenTextures(2, textures);
    primus.dfns.glEnable(GL_TEXTURE_2D);
  }
  else
    primus.dfns.glGenBuffers(2, pbos);
  for (;;)
  {
    sem_wait(&di.d.acqsem);
    profiler.tick(true);
    if (di.d.reinit)
    {
      if (di.d.reinit == di.SHUTDOWN)
      {
	if (use_textures)
	  primus.dfns.glDeleteTextures(2, textures);
	else
	  primus.dfns.glDeleteBuffers(2, pbos);
	primus.dfns.glXMakeCurrent(ddpy, 0, NULL);
	primus.dfns.glXDestroyContext(ddpy, context);
	XCloseDisplay(ddpy);
	sem_post(&di.d.relsem);
	return NULL;
      }
      di.d.reinit = di.NONE;
      profiler.width = width = di.width;
      profiler.height = height = di.height;
      primus.dfns.glViewport(0, 0, width, height);
      if (use_textures)
      {
	primus.dfns.glBindTexture(GL_TEXTURE_2D, textures[ctex ^ 1]);
	primus.dfns.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	primus.dfns.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, NULL);
	primus.dfns.glBindTexture(GL_TEXTURE_2D, textures[ctex]);
	primus.dfns.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	primus.dfns.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, NULL);
      }
      else
      {
	primus.dfns.glBindBuffer(GL_PIXEL_UNPACK_BUFFER_EXT, pbos[ctex ^ 1]);
	primus.dfns.glBufferData(GL_PIXEL_UNPACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_DRAW);
	primus.dfns.glBindBuffer(GL_PIXEL_UNPACK_BUFFER_EXT, pbos[ctex]);
	primus.dfns.glBufferData(GL_PIXEL_UNPACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_DRAW);
      }
      sem_post(&di.d.relsem);
      continue;
    }
    if (use_textures)
      primus.dfns.glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA, GL_UNSIGNED_BYTE, di.pixeldata);
    else
      primus.dfns.glBufferSubData(GL_PIXEL_UNPACK_BUFFER_EXT, 0, width*height*4, di.pixeldata);
    if (!primus.sync)
      sem_post(&di.d.relsem); // Unlock as soon as possible
    profiler.tick();
    if (use_textures)
    {
      primus.dfns.glDrawArrays(GL_QUADS, 0, 4);
      primus.dfns.glBindTexture(GL_TEXTURE_2D, textures[ctex ^= 1]);
    }
    else
    {
      primus.dfns.glDrawPixels(width, height, GL_BGRA, GL_UNSIGNED_BYTE, NULL);
      primus.dfns.glBindBuffer(GL_PIXEL_UNPACK_BUFFER_EXT, pbos[ctex ^= 1]);
    }
    primus.dfns.glXSwapBuffers(ddpy, di.window);
    for (int pending = XPending(ddpy); pending > 0; pending--)
    {
      XEvent event;
      XNextEvent(ddpy, &event);
      if (event.type == ConfigureNotify)
	di.update_geometry(event.xconfigure.width, event.xconfigure.height);
    }
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
  unsigned sleep_usec = 0;
  static const char *state_names[] = {"app", "sleep", "map", "wait", NULL};
  Profiler profiler("readback", state_names);
  struct timespec tp;
  if (!primus.sync)
    sem_post(&di.d.relsem); // No PBO is mapped initially
  GLXContext context = primus.afns.glXCreateNewContext(primus.adpy, di.fbconfig, GLX_RGBA_TYPE, di.actx, True);
  die_if(!primus.afns.glXIsDirect(primus.adpy, context),
	 "failed to acquire direct rendering context for readback thread\n");
  primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, context);
  primus.afns.glGenBuffers(2, &pbos[0]);
  primus.afns.glReadBuffer(GL_FRONT);
  for (;;)
  {
    sem_wait(&di.r.acqsem);
    profiler.tick(true);
    if (di.r.reinit)
    {
      clock_gettime(CLOCK_REALTIME, &tp);
      tp.tv_sec  += 1;
      // Wait for D worker, if active
      if (!primus.sync && sem_timedwait(&di.d.relsem, &tp))
      {
	pthread_cancel(di.d.worker);
	sem_post(&di.d.relsem); // Pretend that D worker completed reinit
	primus_warn("timeout waiting for display worker\n");
	die_if(di.r.reinit != di.SHUTDOWN, "killed worker on resize\n");
      }
      di.d.reinit = di.r.reinit;
      sem_post(&di.d.acqsem); // Signal D worker to reinit
      sem_wait(&di.d.relsem); // Wait until reinit was completed
      if (!primus.sync)
	sem_post(&di.d.relsem); // Unlock as no PBO is currently mapped
      if (di.r.reinit == di.SHUTDOWN)
      {
	primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
	primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
	primus.afns.glDeleteBuffers(2, &pbos[0]);
	primus.afns.glXMakeCurrent(primus.adpy, 0, NULL);
	primus.afns.glXDestroyContext(primus.adpy, context);
	sem_post(&di.r.relsem);
	return NULL;
      }
      di.r.reinit = di.NONE;
      profiler.width = width = di.width;
      profiler.height = height = di.height;
      primus.afns.glXMakeCurrent(primus.adpy, di.pbuffer, context);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
    }
    primus.afns.glWaitSync(di.sync, 0, GL_TIMEOUT_IGNORED);
    primus.afns.glReadPixels(0, 0, width, height, GL_BGRA, GL_UNSIGNED_BYTE, NULL);
    if (!primus.sync)
      sem_post(&di.r.relsem); // Unblock main thread as soon as possible
    usleep(sleep_usec);
    profiler.tick();
    if (primus.sync == 1) // Get the previous framebuffer
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
    double map_time = Profiler::get_timestamp();
    GLvoid *pixeldata = primus.afns.glMapBuffer(GL_PIXEL_PACK_BUFFER_EXT, GL_READ_ONLY);
    map_time = Profiler::get_timestamp() - map_time;
    sleep_usec = (map_time * 1e6 + sleep_usec) * primus.autosleep / 100;
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

// Find appropriate FBConfigs on adpy for a given Visual on ddpy
static GLXFBConfig* match_fbconfig(XVisualInfo *vis)
{
  int ncfg, attrs[] = {
    GLX_DOUBLEBUFFER, 0, GLX_STEREO, 0, GLX_AUX_BUFFERS, 0,
    GLX_RED_SIZE, 0, GLX_GREEN_SIZE, 0, GLX_BLUE_SIZE, 0,
    GLX_ALPHA_SIZE, 0, GLX_DEPTH_SIZE, 0, GLX_STENCIL_SIZE, 0,
    GLX_ACCUM_RED_SIZE, 0, GLX_ACCUM_GREEN_SIZE, 0, GLX_ACCUM_BLUE_SIZE, 0, GLX_ACCUM_ALPHA_SIZE, 0,
    GLX_SAMPLE_BUFFERS, 0, GLX_SAMPLES, 0, None
  };
  for (int i = 0; attrs[i] != None; i += 2)
    primus.dfns.glXGetConfig(primus.ddpy, vis, attrs[i], &attrs[i+1]);
  return primus.afns.glXChooseFBConfig(primus.adpy, 0, attrs, &ncfg);
}

GLXContext glXCreateContext(Display *dpy, XVisualInfo *vis, GLXContext shareList, Bool direct)
{
  GLXFBConfig *acfgs = match_fbconfig(vis);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, shareList, direct);
  primus.contexts.record(actx, *acfgs, shareList);
  return actx;
}

GLXContext glXCreateNewContext(Display *dpy, GLXFBConfig config, int renderType, GLXContext shareList, Bool direct)
{
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, shareList, direct);
  primus.contexts.record(actx, config, shareList);
  return actx;
}

GLXContext glXCreateContextAttribsARB(Display *dpy, GLXFBConfig config, GLXContext shareList, Bool direct, const int *attrib_list)
{
  GLXContext actx = primus.afns.glXCreateContextAttribsARB(primus.adpy, config, shareList, direct, attrib_list);
  primus.contexts.record(actx, config, shareList);
  return actx;
}

void glXDestroyContext(Display *dpy, GLXContext ctx)
{
  primus.contexts.erase(ctx);
  // kludge: reap background tasks when deleting the last context
  // otherwise something will deadlock during unloading the library
  if (primus.contexts.empty())
    for (DrawablesInfo::iterator i = primus.drawables.begin(); i != primus.drawables.end(); i++)
      i->second.reap_workers();
  primus.afns.glXDestroyContext(primus.adpy, ctx);
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
  bool known = primus.drawables.known(draw);
  DrawableInfo &di = primus.drawables[draw];
  if (!known)
  {
    // Drawable is a plain X Window. Get the FBConfig from the context
    if (ctx)
      di.fbconfig = primus.contexts[ctx].fbconfig;
    else
    {
      XWindowAttributes attrs;
      die_if(!XGetWindowAttributes(dpy, draw, &attrs), "failed to query attributes");
      int nvis;
      XVisualInfo tmpl = {0}, *vis;
      tmpl.visualid = XVisualIDFromVisual(attrs.visual);
      die_if(!(vis = XGetVisualInfo(dpy, VisualIDMask, &tmpl, &nvis)), "no visuals");
      di.fbconfig = *match_fbconfig(vis);
      XFree(vis);
    }
    di.kind = di.XWindow;
    di.window = draw;
    note_geometry(dpy, draw, &di.width, &di.height);
  }
  else if (ctx && di.fbconfig != primus.contexts[ctx].fbconfig)
  {
    if (di.pbuffer)
    {
      primus_warn("recreating incompatible pbuffer\n");
      di.reap_workers();
      primus.afns.glXDestroyPbuffer(primus.adpy, di.pbuffer);
      di.pbuffer = 0;
    }
    di.fbconfig = primus.contexts[ctx].fbconfig;
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
  GLXPbuffer pb_read = lookup_pbuffer(dpy, read, ctx);
  tsdata.make_current(dpy, draw, read);
  return primus.afns.glXMakeContextCurrent(primus.adpy, pbuffer, pb_read, ctx);
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  XFlush(dpy);
  assert(primus.drawables.known(drawable));
  DrawableInfo &di = primus.drawables[drawable];
  primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  if (di.kind == di.Pbuffer || di.kind == di.Pixmap)
    return;
  GLXContext ctx = glXGetCurrentContext();
  if (!ctx)
    primus_warn("glXSwapBuffers: no current context\n");
  else if (drawable != tsdata.drawable)
    primus_warn("glXSwapBuffers: drawable not current\n");
  if (di.r.worker && ctx && (!di.actx || primus.contexts[di.actx].sharegroup != primus.contexts[ctx].sharegroup))
  {
    primus_warn("glXSwapBuffers: respawning threads after context change\n");
    di.reap_workers();
  }
  if (!di.r.worker)
  {
    // Need to create a sharing context to use GL sync objects
    di.actx = ctx;
    di.d.spawn_worker(drawable, display_work);
    di.r.spawn_worker(drawable, readback_work);
  }
  // Readback thread needs a sync object to avoid reading an incomplete frame
  di.sync = primus.afns.glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
  sem_post(&di.r.acqsem); // Signal the readback worker thread
  sem_wait(&di.r.relsem); // Wait until it has issued glReadBuffer
  primus.afns.glDeleteSync(di.sync);
  if (di.reinit == di.RESIZE)
  {
    __sync_synchronize();
    primus.afns.glXDestroyPbuffer(primus.adpy, di.pbuffer);
    di.pbuffer = create_pbuffer(di);
    if (ctx) // FIXME: drawable can be current in other threads
      glXMakeContextCurrent(dpy, tsdata.drawable, tsdata.read_drawable, ctx);
    di.r.reinit = di.reinit;
    di.reinit = di.NONE;
  }
}

GLXWindow glXCreateWindow(Display *dpy, GLXFBConfig config, Window win, const int *attribList)
{
  GLXWindow glxwin = primus.dfns.glXCreateWindow(dpy, primus.dconfigs[0], win, attribList);
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
  primus.dfns.glXDestroyWindow(dpy, window);
}

GLXPbuffer glXCreatePbuffer(Display *dpy, GLXFBConfig config, const int *attribList)
{
  GLXPbuffer pbuffer = primus.dfns.glXCreatePbuffer(dpy, primus.dconfigs[0], attribList);
  DrawableInfo &di = primus.drawables[pbuffer];
  di.kind = di.Pbuffer;
  di.fbconfig = config;
  for (int i = 0; attribList[i] != None; i++)
    if (attribList[i] == GLX_PBUFFER_WIDTH)
      di.width = attribList[i+1];
    else if (attribList[i] == GLX_PBUFFER_HEIGHT)
      di.height = attribList[i+1];
  return pbuffer;
}

void glXDestroyPbuffer(Display *dpy, GLXPbuffer pbuf)
{
  assert(primus.drawables.known(pbuf));
  primus.drawables.erase(pbuf);
  primus.dfns.glXDestroyPbuffer(dpy, pbuf);
}

GLXPixmap glXCreatePixmap(Display *dpy, GLXFBConfig config, Pixmap pixmap, const int *attribList)
{
  GLXPixmap glxpix = primus.dfns.glXCreatePixmap(dpy, primus.dconfigs[0], pixmap, attribList);
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
  GLXPixmap glxpix = primus.dfns.glXCreateGLXPixmap(dpy, visual, pixmap);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  note_geometry(dpy, pixmap, &di.width, &di.height);
  GLXFBConfig *acfgs = match_fbconfig(visual);
  di.fbconfig = *acfgs;
  return glxpix;
}

void glXDestroyGLXPixmap(Display *dpy, GLXPixmap pixmap)
{
  glXDestroyPixmap(dpy, pixmap);
}

static XVisualInfo *match_visual(int attrs[])
{
  XVisualInfo *vis = glXChooseVisual(primus.ddpy, 0, attrs);
  for (int i = 2; attrs[i] != None && vis; i += 2)
  {
    int tmp = attrs[i+1];
    primus.dfns.glXGetConfig(primus.ddpy, vis, attrs[i], &attrs[i+1]);
    if (tmp != attrs[i+1])
      vis = NULL;
  }
  return vis;
}

XVisualInfo *glXGetVisualFromFBConfig(Display *dpy, GLXFBConfig config)
{
  if (!primus.afns.glXGetVisualFromFBConfig(primus.adpy, config))
    return NULL;
  int i, attrs[] = {
    GLX_RGBA, GLX_DOUBLEBUFFER,
    GLX_RED_SIZE, 0, GLX_GREEN_SIZE, 0, GLX_BLUE_SIZE, 0,
    GLX_ALPHA_SIZE, 0, GLX_DEPTH_SIZE, 0, GLX_STENCIL_SIZE, 0,
    GLX_SAMPLE_BUFFERS, 0, GLX_SAMPLES, 0, None
  };
  for (i = 2; attrs[i] != None; i += 2)
    primus.afns.glXGetFBConfigAttrib(primus.adpy, config, attrs[i], &attrs[i+1]);
  XVisualInfo *vis = NULL;
  for (i -= 2; i >= 0 && !vis; i -= 2)
  {
    vis = match_visual(attrs);
    attrs[i] = None;
  }
  return vis;
}

int glXGetFBConfigAttrib(Display *dpy, GLXFBConfig config, int attribute, int *value)
{
  int r = primus.afns.glXGetFBConfigAttrib(primus.adpy, config, attribute, value);
  if (attribute == GLX_VISUAL_ID && *value)
    return primus.dfns.glXGetConfig(primus.ddpy, glXGetVisualFromFBConfig(dpy, config), attribute, value);
  return r;
}

void glXQueryDrawable(Display *dpy, GLXDrawable draw, int attribute, unsigned int *value)
{
  primus.afns.glXQueryDrawable(primus.adpy, lookup_pbuffer(dpy, draw, NULL), attribute, value);
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
static ret l##name par \
{ return primus.afns.name(__VA_ARGS__); } \
asm(".type " #name ", %gnu_indirect_function"); \
void *ifunc_##name(void) asm(#name) __attribute__((visibility("default"))); \
void *ifunc_##name(void) \
{ return primus.afns.handle ? real_dlsym(primus.afns.handle, #name) : (void*)l##name; }
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

static const char glxext_clientside[] = "GLX_ARB_get_proc_address ";
static const char glxext_adpy[] = "GLX_ARB_create_context GLX_ARB_create_context_profile ";
static const char glxext_ddpy[] = "";

const char *glXGetClientString(Display *dpy, int name)
{
  static std::string exts(std::string(glxext_clientside) + glxext_adpy + glxext_ddpy);
  switch (name)
  {
    case GLX_VENDOR: return "primus";
    case GLX_VERSION: return "1.4";
    case GLX_EXTENSIONS: return exts.c_str();
    default: return NULL;
  }
}

static std::string intersect_exts(const char *set1, const char *set2)
{
  std::string r;
  for (const char *p; *set1; set1 = p + 1)
  {
    p = strchr(set1, ' ');
    if (memmem(set2, strlen(set2), set1, p - set1))
      r.append(set1, p - set1 + 1);
  }
  return r;
}

const char *glXQueryExtensionsString(Display *dpy, int screen)
{
  static std::string exts
    (std::string(glxext_clientside)
     + intersect_exts(glxext_adpy, primus.afns.glXQueryExtensionsString(primus.adpy, 0))
     + intersect_exts(glxext_ddpy, primus.dfns.glXQueryExtensionsString(primus.ddpy, 0)));
  return exts.c_str();
}

// OpenGL ABI specifies that anything above OpenGL 1.2 + ARB_multitexture must
// be obtained via glXGetProcAddress, but some applications link against
// extension functions, and Mesa and vendor libraries let them
#ifndef PRIMUS_STRICT
#warning Enabled workarounds for applications demanding more than promised by the OpenGL ABI

// OpenGL extension forwarders
#define P(name) \
asm(".type " #name ", %gnu_indirect_function"); \
void *ifunc_##name(void) asm(#name) __attribute__((visibility("default"))); \
void *ifunc_##name(void) \
{ return primus.afns.handle ? real_dlsym(primus.afns.handle, #name) : NULL; }
#include "glext-passthru.def"
#undef P
#endif
