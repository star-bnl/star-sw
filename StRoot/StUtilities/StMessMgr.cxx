#define  StMessMgrImpl
#include "StMessMgr.h"

#ifdef __ROOT__
ClassImp(StMessMgr)
#endif

StMessMgr::StMessMgr() : ostrstream(new char[1024],1024,ios::out) {}
