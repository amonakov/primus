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
#include <GL/glx.h>

#define primus_trace(...) do { if (1) fprintf(stderr, "primus: " __VA_ARGS__); } while(0)

struct CapturedFns {
  void *handle;
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
};

struct DrawableInfo {
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

struct EarlyInitializer {
  EarlyInitializer()
  {
#ifdef BUMBLEBEE_SOCKET
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
#else
#warning Building without Bumblebee daemon support
#endif
  }
};

#define getconf(V) (getenv(#V) ? getenv(#V) : V)

static struct PrimusInfo {
  EarlyInitializer ei;
  Display *adpy;
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
    needed_global(dlopen(getconf(PRIMUS_LOAD_GLOBAL), RTLD_LAZY | RTLD_GLOBAL)),
    afns(getconf(PRIMUS_libGLa)),
    dfns(getconf(PRIMUS_libGLd))
  {
    assert(adpy && "failed to open secondary X display");
    if (afns.handle == dfns.handle && strcmp(getenv("PRIMUS_libGLa"), getenv("PRIMUS_libGLd")))
      primus_trace("warning: unexpectedly got same libGL for rendering and display\n");
    XInitThreads();
  }
} primus;

static __thread struct TSPrimusInfo {

  static void* dwork(void *vd);

  struct D {
    pthread_t worker;
    pthread_mutex_t dmutex, rmutex;
    int width, height;
    GLvoid *buf;
    Display *dpy;
    GLXDrawable drawable, read_drawable;
    GLXContext context;
    bool reinit;

    struct {
      enum State {Wait, Upload, DrawSwap, NStates} state;
      double state_time[NStates];
      double prev_timestamp, old_timestamp;
      int nframes;

      void init()
      {
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
	prev_timestamp = old_timestamp = timestamp;
      }
      void tick()
      {
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
	state_time[state] += timestamp - prev_timestamp;
	state = (State)((state + 1) % NStates);
	prev_timestamp = timestamp;
	nframes += !!(state == Wait);
	double period = timestamp - old_timestamp;
	if (state != Wait || period < 5)
	  return;
	primus_trace("profiling: display: %.1f fps, %.1f%% wait, %.1f%% upload, %.1f%% draw+swap\n", nframes / period,
	             100 * state_time[Wait] / period, 100 * state_time[Upload] / period, 100 * state_time[DrawSwap] / period);
	old_timestamp = timestamp;
	nframes = 0;
	memset(state_time, 0, sizeof(state_time));
      }
    } profiler;

    void spawn_worker()
    {
      pthread_mutex_init(&dmutex, NULL);
      pthread_mutex_lock(&dmutex);
      pthread_mutex_init(&rmutex, NULL);
      pthread_create(&worker, NULL, dwork, (void*)this);
    }

    void set_drawable(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext actx)
    {
      if (primus.drawables[draw].kind == DrawableInfo::Pbuffer)
	return;
      if (!worker)
	spawn_worker();
      this->dpy = dpy;
      this->drawable = draw;
      this->read_drawable = read;
      this->context = actx ? primus.actx2dctx[actx] : NULL;
      this->width  = primus.drawables[draw].width;
      this->height = primus.drawables[draw].height;
    }
  } d;

  static void* rwork(void *vr);

  struct R {
    pthread_t worker;
    pthread_mutex_t amutex, rmutex;
    int width, height;
    GLXDrawable pbuffer;
    GLXContext context;
    D *pd;
    bool reinit;
    GLsync sync;

    struct {
      enum State {App, Map, Wait, NStates} state;
      double state_time[NStates];
      double prev_timestamp, old_timestamp;
      int nframes, ndropped;

      void init()
      {
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
	prev_timestamp = old_timestamp = timestamp;
      }
      void tick()
      {
	struct timespec tp;
	clock_gettime(CLOCK_MONOTONIC, &tp);
	double timestamp = tp.tv_sec + 1e-9 * tp.tv_nsec;
	state_time[state] += timestamp - prev_timestamp;
	state = (State)((state + 1) % NStates);
	prev_timestamp = timestamp;
	nframes += !!(state == App);
	double T = timestamp - old_timestamp;
	if (state != App || T < 5)
	  return;
	primus_trace("profiling: readback: %.1f fps, %.1f%% app, %.1f%% map, %.1f%% wait\n", (nframes + ndropped) / T,
	             100 * state_time[App] / T, 100 * state_time[Map] / T, 100 * state_time[Wait] / T);
	old_timestamp = timestamp;
	nframes = ndropped = 0;
	memset(state_time, 0, sizeof(state_time));
      }
    } profiler;

    void spawn_worker()
    {
      pthread_mutex_init(&amutex, NULL);
      pthread_mutex_init(&rmutex, NULL);
      pthread_mutex_lock(&rmutex);
      pthread_mutex_lock(&amutex);
      pthread_create(&worker, NULL, rwork, (void*)this);
    }

    void set_drawable(GLXDrawable draw, GLXContext actx, D *pd)
    {
      if (primus.drawables[draw].kind == DrawableInfo::Pbuffer)
	return;
      if (!worker)
	spawn_worker();
      this->pbuffer = primus.drawables[draw].pbuffer;
      this->context = actx ? primus.actx2rctx[actx] : NULL;
      this->width  = primus.drawables[draw].width;
      this->height = primus.drawables[draw].height;
      this->pd     = pd;
      this->reinit = true;
    }
  } r;
} tsprimus;

void* TSPrimusInfo::dwork(void *vd)
{
  struct D &d = *(D *)vd;
  int width, height;
  d.profiler.init();
  for (;;)
  {
    pthread_mutex_lock(&d.dmutex);
    d.profiler.tick();
    if (d.reinit)
    {
      d.reinit = false;
      width = d.width; height = d.height;
      primus.dfns.glXMakeCurrent(d.dpy, d.drawable, d.context);
      primus.dfns.glViewport(0, 0, d.width, d.height);
      float quad_vertex_coords[]  = {-1, -1, -1, 1, 1, 1, 1, -1};
      float quad_texture_coords[] = { 0,  0,  0, 1, 1, 1, 1,  0};
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
    }
    primus.dfns.glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, d.buf);
    d.profiler.tick();
    primus.dfns.glDrawArrays(GL_QUADS, 0, 4);
    pthread_mutex_unlock(&d.rmutex);
    primus.dfns.glXSwapBuffers(d.dpy, d.drawable);
    d.profiler.tick();
  }
  return NULL;
}

void* TSPrimusInfo::rwork(void *vr)
{
  GLuint pbos[2];
  int cbuf = 0;
  struct R &r = *(R *)vr;
  r.profiler.init();
  for (;;)
  {
    pthread_mutex_lock(&r.rmutex);
    r.profiler.tick();
    if (r.reinit)
    {
      r.reinit = false;
      if (pbos[0])
      {
	pthread_mutex_lock(&r.pd->rmutex);
	pthread_mutex_unlock(&r.pd->rmutex);
	primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf ^ 1]);
	primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
	primus.afns.glDeleteBuffers(2, &pbos[0]);
      }
      r.pd->reinit = true;
      primus.afns.glXMakeCurrent(primus.adpy, r.pbuffer, r.context);
      if (!pbos[0])
	primus.afns.glGenBuffers(2, &pbos[0]);
      primus.afns.glReadBuffer(GL_BACK);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[1]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, r.width*r.height*4, NULL, GL_STREAM_READ);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[0]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, r.width*r.height*4, NULL, GL_STREAM_READ);
    }
    primus.afns.glWaitSync(r.sync, 0, GL_TIMEOUT_IGNORED);
    primus.afns.glReadPixels(0, 0, r.width, r.height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
    pthread_mutex_unlock(&r.amutex);
    GLvoid *pixeldata = primus.afns.glMapBuffer(GL_PIXEL_PACK_BUFFER_EXT, GL_READ_ONLY);
    r.profiler.tick();
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    tp.tv_nsec += 20000000;
    tp.tv_sec  += tp.tv_nsec / 1000000000;
    tp.tv_nsec %= 1000000000;
    if (pthread_mutex_timedlock(&r.pd->rmutex, &tp))
    {
      primus_trace("warning: dropping a frame to avoid deadlock\n");
      r.profiler.ndropped++;
    }
    else
    {
      r.pd->buf = pixeldata;
      pthread_mutex_unlock(&r.pd->dmutex);
      cbuf ^= 1;
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[cbuf]);
    }
    primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
    r.profiler.tick();
  }
  return NULL;
}

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
  GLXContext dctx = primus.dfns.glXCreateNewContext(dpy, *dcfgs, GLX_RGBA_TYPE, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, shareList, direct);
  GLXContext rctx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, actx, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2rctx[actx] = rctx;
  primus.actx2fbconfig[actx] = *acfgs;;
  if (direct && !primus.dfns.glXIsDirect(dpy, dctx))
    primus_trace("warning: failed to acquire direct rendering context for display thread\n");
  return actx;
}

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
  GLXContext dctx = primus.dfns.glXCreateNewContext(dpy, get_dpy_fbc(dpy, config), renderType, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, shareList, direct);
  GLXContext rctx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, actx, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2rctx[actx] = rctx;
  primus.actx2fbconfig[actx] = config;
  if (direct && !primus.dfns.glXIsDirect(dpy, dctx))
    primus_trace("warning: failed to acquire direct rendering context for display thread\n");
  return actx;
}

void glXDestroyContext(Display *dpy, GLXContext ctx)
{
  GLXContext dctx = primus.actx2dctx[ctx];
  GLXContext rctx = primus.actx2rctx[ctx];
  primus.actx2dctx.erase(ctx);
  primus.actx2rctx.erase(ctx);
  primus.actx2fbconfig.erase(ctx);
  primus.dfns.glXDestroyContext(dpy, dctx);
  primus.afns.glXDestroyContext(primus.adpy, rctx);
  primus.afns.glXDestroyContext(primus.adpy, ctx);
}

static void note_geometry(Display *dpy, Drawable draw, int *width, int *height)
{
  Window root;
  int x, y;
  unsigned bw, d;
  XGetGeometry(dpy, draw, &root, &x, &y, (unsigned *)width, (unsigned *)height, &bw, &d);
}

static GLXPbuffer lookup_pbuffer(Display *dpy, GLXDrawable draw, GLXContext ctx)
{
  if (!draw)
    return 0;
  assert(ctx);
  bool known = primus.drawables.known(draw);
  DrawableInfo &di = primus.drawables[draw];
  if (!known)
  {
    // Drawable is a plain X Window
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
  tsprimus.d.set_drawable(dpy, drawable, drawable, ctx);
  tsprimus.r.set_drawable(drawable, ctx, &tsprimus.d);
  return primus.afns.glXMakeCurrent(primus.adpy, pbuffer, ctx);
}

Bool glXMakeContextCurrent(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext ctx)
{
  if (draw == read)
    return glXMakeCurrent(dpy, draw, ctx);
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, draw, ctx);
  tsprimus.d.set_drawable(dpy, draw, read, ctx);
  tsprimus.r.set_drawable(draw, ctx, &tsprimus.d);
  GLXPbuffer pb_read = lookup_pbuffer(dpy, read, ctx);
  return primus.afns.glXMakeContextCurrent(primus.adpy, pbuffer, pb_read, ctx);
}

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
  tsprimus.d.set_drawable(dpy, drawable, drawable, actx);
  tsprimus.r.set_drawable(drawable, actx, &tsprimus.d);
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  assert(primus.drawables.known(drawable));
  DrawableInfo &di = primus.drawables[drawable];
  if (di.kind == di.Pbuffer)
    return primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  if (di.kind == di.XWindow)
    update_geometry(dpy, drawable, di);
  tsprimus.r.sync = primus.afns.glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
  pthread_mutex_unlock(&tsprimus.r.rmutex);
  pthread_mutex_lock(&tsprimus.r.amutex);
  primus.afns.glDeleteSync(tsprimus.r.sync);
  primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
}

GLXWindow glXCreateWindow(Display *dpy, GLXFBConfig config, Window win, const int *attribList)
{
  GLXWindow glxwin = primus.dfns.glXCreateWindow(dpy, get_dpy_fbc(dpy, config), win, attribList);
  DrawableInfo &di = primus.drawables[glxwin];
  di.kind = di.Window;
  di.fbconfig = config;
  note_geometry(dpy, win, &di.width, &di.height);
  return glxwin;
}

void glXDestroyWindow(Display *dpy, GLXWindow window)
{
  primus.dfns.glXDestroyWindow(dpy, window);
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
  GLXPixmap glxpix = primus.dfns.glXCreateGLXPixmap(dpy, visual, pixmap);
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
  glXDestroyPixmap(dpy, pixmap);
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
  // FIXME: implement for single-buffered apps
}

void glXWaitX(void)
{
  // FIXME: implement for single-buffered apps
}

Display *glXGetCurrentDisplay(void)
{
  return tsprimus.d.dpy;
}

GLXDrawable glXGetCurrentReadDrawable(void)
{
  return tsprimus.d.read_drawable;
}

XVisualInfo* glXChooseVisual(Display *dpy, int screen, int *attribList)
{
  return primus.dfns.glXChooseVisual(dpy, screen, attribList);
}

int glXGetConfig(Display *dpy, XVisualInfo *visual, int attrib, int *value)
{
  return primus.dfns.glXGetConfig(dpy, visual, attrib, value);
}

#define DEF_GLX_PROTO(ret, name, par, ...) \
ret name par \
{ return primus.afns.name(primus.adpy, __VA_ARGS__); }
#include "glx-dpyredir.def"
#undef DEF_GLX_PROTO

#define DEF_GLX_PROTO(ret, name, par, ...) \
ret name par \
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
  if (memcmp(procName, "glX", 3))
    return primus.afns.glXGetProcAddress(procName);
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
