#include <dlfcn.h>
#include <pthread.h>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cassert>
#include <map>
#include <GL/glx.h>

#define primus_trace(...) do { if (1) fprintf(stderr, __VA_ARGS__); } while(0)

struct CapturedFns {
  void *handle;
#define DEF_GLX_PROTO(ret, name, args, ...) ret (*name) args;
#include "glx-reimpl.def"
#include "glx-dpyredir.def"
#include "glxext-reimpl.def"
#include "glxext-dpyredir.def"
#include "gl-needed.def"
#undef DEF_GLX_PROTO
  CapturedFns(const char *lib)
  {
    assert(lib && lib[0] == '/');
    handle = dlopen(lib, RTLD_LAZY);
    primus_trace("loading %s: %p\n", lib, handle);
    assert(handle);
#define DEF_GLX_PROTO(ret, name, args, ...) name = (ret (*) args)dlsym(handle, #name);
#include "glx-reimpl.def"
#include "glx-dpyredir.def"
#undef DEF_GLX_PROTO
#define DEF_GLX_PROTO(ret, name, args, ...) name = (ret (*) args)this->glXGetProcAddress((GLubyte*)#name);
#include "glxext-reimpl.def"
#include "glxext-dpyredir.def"
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

static struct PrimusInfo {
  Display *adpy;
  CapturedFns afns;
  const void *needed_global;
  CapturedFns dfns;
  DrawablesInfo drawables;
  // FIXME: there are race conditions in accesses to these
  std::map<GLXContext, GLXFBConfig> actx2fbconfig;
  std::map<GLXContext, GLXContext> actx2dctx;

  PrimusInfo():
    adpy(XOpenDisplay(getenv("PRIMUS_DISPLAY"))),
    afns(getenv("PRIMUS_libGLa")),
    needed_global(dlopen(getenv("PRIMUS_LOAD_GLOBAL"), RTLD_LAZY | RTLD_GLOBAL)),
    dfns(getenv("PRIMUS_libGLd"))
  {
    if (afns.handle == dfns.handle && strcmp(getenv("PRIMUS_libGLa"), getenv("PRIMUS_libGLd")))
      primus_trace("primus: warning: unexpectedly got same libGL for rendering and display\n");
    XInitThreads();
  }
} primus;

static __thread struct TSPrimusInfo {

  static void* tsprimus_work(void *vd);

  struct D {
    pthread_t thread;
    pthread_mutex_t dmutex, amutex;
    int width, height;
    GLvoid *buf;
    Display *dpy;
    GLXDrawable drawable;
    GLXContext context;

    void init()
    {
      pthread_mutex_init(&dmutex, NULL);
      pthread_mutex_lock(&dmutex);
      pthread_mutex_init(&amutex, NULL);
      pthread_create(&thread, NULL, tsprimus_work, (void*)this);
    }

    void set_drawable(Display *dpy, GLXDrawable draw, GLXContext ctx)
    {
      if (primus.drawables[draw].kind == DrawableInfo::Pbuffer)
	return;
      if (!thread)
	init();
      pthread_mutex_lock(&amutex);
      this->dpy = dpy;
      this->drawable = draw;
      this->context = ctx;
      this->width  = primus.drawables[draw].width;
      this->height = primus.drawables[draw].height;
      pthread_mutex_unlock(&amutex);
    }
    void wait()
    {
      pthread_mutex_lock(&amutex);
      pthread_mutex_unlock(&amutex);
    }
  } d;
  struct A {
    GLuint pbos[2];
    int cbuf;
    int width, height;
    bool first;

    void update(const DrawableInfo& di)
    {
      if (width == di.width && height == di.height)
	return;
      width = di.width; height = di.height;
      first = true;
      if (!pbos[0])
	primus.afns.glGenBuffers(2, &pbos[0]);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[0]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
      primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, pbos[1]);
      primus.afns.glBufferData(GL_PIXEL_PACK_BUFFER_EXT, width*height*4, NULL, GL_STREAM_READ);
    }
  } bufs;
} tsprimus;

void* TSPrimusInfo::tsprimus_work(void *vd)
{
  struct D &d = *(D *)vd;
  for (;;)
  {
    pthread_mutex_lock(&d.dmutex);
    asm volatile ("" : : : "memory");
    primus.dfns.glXMakeCurrent(d.dpy, d.drawable, d.context);
    primus.dfns.glDrawPixels(d.width, d.height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, d.buf);
    pthread_mutex_unlock(&d.amutex);
    primus.dfns.glXSwapBuffers(d.dpy, d.drawable);
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
  primus_trace("%s\n", __func__);
  GLXFBConfig *acfgs, *dcfgs;
  match_fbconfigs(dpy, vis, &acfgs, &dcfgs);
  GLXContext dctx = primus.dfns.glXCreateNewContext(dpy, *dcfgs, GLX_RGBA_TYPE, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, *acfgs, GLX_RGBA_TYPE, shareList, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2fbconfig[actx] = *acfgs;;
  if (direct && !primus.dfns.glXIsDirect(dpy, dctx))
    primus_trace("primus: warning: failed to acquire direct rendering context for display thread\n");
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
  primus_trace("%s\n", __func__);
  GLXContext dctx = primus.dfns.glXCreateNewContext(dpy, get_dpy_fbc(dpy, config), renderType, NULL, direct);
  GLXContext actx = primus.afns.glXCreateNewContext(primus.adpy, config, renderType, shareList, direct);
  primus.actx2dctx[actx] = dctx;
  primus.actx2fbconfig[actx] = config;
  if (direct && !primus.dfns.glXIsDirect(dpy, dctx))
    primus_trace("primus: warning: failed to acquire direct rendering context for display thread\n");
  return actx;
}

void glXDestroyContext(Display *dpy, GLXContext ctx)
{
  tsprimus.d.wait();
  GLXContext dctx = primus.actx2dctx[ctx];
  primus.actx2dctx.erase(ctx);
  primus.actx2fbconfig.erase(ctx);
  primus.dfns.glXDestroyContext(dpy, dctx);
  primus.afns.glXDestroyContext(primus.adpy, ctx);
}

static void note_geometry(Display *dpy, Drawable draw, DrawableInfo &di)
{
  Window root;
  int x, y;
  unsigned bw, d;
  XGetGeometry(dpy, draw, &root, &x, &y, (unsigned *)&di.width, (unsigned *)&di.height, &bw, &d);
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
    note_geometry(dpy, draw, di);
  }
  GLXPbuffer &pbuffer = di.pbuffer;
  // FIXME the drawable could have been resized
  if (!pbuffer)
  {
    int pbattrs[] = {GLX_PBUFFER_WIDTH, di.width, GLX_PBUFFER_HEIGHT, di.height, GLX_PRESERVED_CONTENTS, True, None};
    pbuffer = primus.afns.glXCreatePbuffer(primus.adpy, di.fbconfig, pbattrs);
  }
  return pbuffer;
}

Bool glXMakeCurrent(Display *dpy, GLXDrawable drawable, GLXContext ctx)
{
  primus_trace("%s\n", __func__);
  GLXContext dctx = ctx ? primus.actx2dctx[ctx] : NULL;
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, drawable, ctx);
  tsprimus.d.set_drawable(dpy, drawable, dctx);
  return primus.afns.glXMakeCurrent(primus.adpy, pbuffer, ctx);
}

Bool glXMakeContextCurrent(Display *dpy, GLXDrawable draw, GLXDrawable read, GLXContext ctx)
{
  primus_trace("%s\n", __func__);
  if (draw == read)
    return glXMakeCurrent(dpy, draw, ctx);
  GLXContext dctx = ctx ? primus.actx2dctx[ctx] : NULL;
  GLXPbuffer pbuffer = lookup_pbuffer(dpy, draw, ctx);
  tsprimus.d.set_drawable(dpy, draw, dctx);
  GLXPbuffer pb_read = lookup_pbuffer(dpy, read, ctx);
  return primus.afns.glXMakeContextCurrent(primus.adpy, pbuffer, pb_read, ctx);
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  static bool dropframes = getenv("PRIMUS_DROPFRAMES");
  TSPrimusInfo::A& bufs = tsprimus.bufs;
  assert(primus.drawables.known(drawable));
  const DrawableInfo &di = primus.drawables[drawable];
  if (di.kind == di.Pbuffer)
    return primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  bufs.update(di);
  int orig_read_buf;
  if (!bufs.first)
  {
    if (!dropframes)
      pthread_mutex_lock(&tsprimus.d.amutex);
    else if (pthread_mutex_trylock(&tsprimus.d.amutex))
      return primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  }
  primus.afns.glGetIntegerv(GL_READ_BUFFER, &orig_read_buf);
  primus.afns.glReadBuffer(GL_BACK);
  primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, bufs.pbos[bufs.cbuf]);
  primus.afns.glUnmapBuffer(GL_PIXEL_PACK_BUFFER_EXT);
  primus.afns.glReadPixels(0, 0, di.width, di.height, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, NULL);
  primus.afns.glReadBuffer(orig_read_buf);
  primus.afns.glXSwapBuffers(primus.adpy, di.pbuffer);
  if (!bufs.first)
  {
    primus.afns.glBindBuffer(GL_PIXEL_PACK_BUFFER_EXT, bufs.pbos[bufs.cbuf ^ 1]);
    GLvoid *pixeldata = primus.afns.glMapBuffer(GL_PIXEL_PACK_BUFFER_EXT, GL_READ_ONLY);
    assert(pixeldata);
    tsprimus.d.buf = pixeldata;
    asm volatile ("" : : : "memory");
    pthread_mutex_unlock(&tsprimus.d.dmutex);
  }
  bufs.first = false;
  bufs.cbuf ^= 1;
}

GLXWindow glXCreateWindow(Display *dpy, GLXFBConfig config, Window win, const int *attribList)
{
  primus_trace("%s\n", __func__);
  GLXWindow glxwin = primus.dfns.glXCreateWindow(dpy, get_dpy_fbc(dpy, config), win, attribList);
  DrawableInfo &di = primus.drawables[glxwin];
  di.kind = di.Window;
  di.fbconfig = config;
  note_geometry(dpy, win, di);
  return glxwin;
}

void glXDestroyWindow(Display *dpy, GLXWindow window)
{
  tsprimus.d.wait();
  primus.dfns.glXDestroyWindow(dpy, window);
  if (primus.drawables[window].pbuffer)
    primus.afns.glXDestroyPbuffer(primus.adpy, primus.drawables[window].pbuffer);
  primus.drawables.erase(window);
}

GLXPbuffer glXCreatePbuffer(Display *dpy, GLXFBConfig config, const int *attribList)
{
  primus_trace("%s\n", __func__);
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
  primus_trace("%s\n", __func__);
  GLXPixmap glxpix = primus.dfns.glXCreatePixmap(dpy, get_dpy_fbc(dpy, config), pixmap, attribList);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  di.fbconfig = config;
  note_geometry(dpy, pixmap, di);
  return glxpix;
}

void glXDestroyPixmap(Display *dpy, GLXPixmap pixmap)
{
  tsprimus.d.wait();
  primus.dfns.glXDestroyPixmap(dpy, pixmap);
  if (primus.drawables[pixmap].pbuffer)
    primus.afns.glXDestroyPbuffer(primus.adpy, primus.drawables[pixmap].pbuffer);
  primus.drawables.erase(pixmap);
}

GLXPixmap glXCreateGLXPixmap(Display *dpy, XVisualInfo *visual, Pixmap pixmap)
{
  primus_trace("%s\n", __func__);
  GLXPixmap glxpix = primus.dfns.glXCreateGLXPixmap(dpy, visual, pixmap);
  DrawableInfo &di = primus.drawables[glxpix];
  di.kind = di.Pixmap;
  note_geometry(dpy, pixmap, di);
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
  primus_trace("%s\n", __func__);
  return primus.dfns.glXGetVisualFromFBConfig(dpy, get_dpy_fbc(dpy, config));
}

void glXQueryDrawable(Display *dpy, GLXDrawable draw, int attribute, unsigned int *value)
{
  primus_trace("%s\n", __func__);
  assert(primus.drawables.known(draw));
  primus.afns.glXQueryDrawable(primus.adpy, primus.drawables[draw].pbuffer, attribute, value);
}

void glXUseXFont(Font font, int first, int count, int list)
{
  primus_trace("primus: sorry, not implemented: %s\n", __func__);
  for (int i = 0; i < count; i++)
  {
    primus.afns.glNewList(list + i, GL_COMPILE);
    primus.afns.glEndList();
  }
}

#define DEF_GLX_PROTO(ret, name, par, ...) \
ret name par \
{ primus_trace("primus: blindly redirecting dpy for %s\n", #name); \
  return primus.afns.name(primus.adpy, __VA_ARGS__); }
#include "glx-dpyredir.def"
#undef DEF_GLX_PROTO

#define DEF_GLX_PROTO(ret, name, par, dpy, ...) \
extern "C" ret name par \
{ primus_trace("primus: blindly redirecting dpy for %s\n", #name); \
  return primus.afns.name(primus.adpy, __VA_ARGS__); }
#include "glxext-dpyredir.def"
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
#include "glxext-dpyredir.def"
#undef  DEF_GLX_PROTO
  };
  static const __GLXextFuncPtr redefined_fns[] = {
#define DEF_GLX_PROTO(ret, name, args, ...) (__GLXextFuncPtr)name,
#include "glx-reimpl.def"
#include "glxext-reimpl.def"
#include "glx-dpyredir.def"
#include "glxext-dpyredir.def"
#undef  DEF_GLX_PROTO
  };
  __GLXextFuncPtr retval = NULL;
  primus_trace("glXGetProcAddress(\"%s\"): ", procName);
  enum {n_redefined = sizeof(redefined_fns) / sizeof(redefined_fns[0])};
  for (int i = 0; i < n_redefined; i++)
    if (!strcmp((const char *)procName, redefined_names[i]))
    {
      retval = redefined_fns[i];
      primus_trace("local %p\n", retval);
    }
  if (!retval)
  {
    retval = primus.afns.glXGetProcAddress(procName);
    primus_trace("native %p\n", retval);
  }
  return retval;
}

__GLXextFuncPtr glXGetProcAddressARB(const GLubyte *procName)
{
  return glXGetProcAddress(procName);
}
