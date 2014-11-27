#ifndef QTROOT_TQTGLINCLUDES
#define QTROOT_TQTGLINCLUDES

#ifndef __CINT__
#if !defined( __APPLE__ ) || defined(Q_WS_X11)
#  if  ROOT_VERSION_CODE >= ROOT_VERSION(5,22,0)
#    include  "TGLIncludes.5.22.h"
#  else
#    if  ROOT_VERSION_CODE >= ROOT_VERSION(5,15,9)
#      include  "TGLIncludes.h"
#    else
#      include "TRootGLU.h"
#    endif
#  endif
#else
#  include <glu.h>
#endif
#endif
#endif
