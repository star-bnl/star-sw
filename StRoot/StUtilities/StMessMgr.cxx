#define  StMessMgrImpl
#include "StMessMgr.h"

ClassImp(StMessMgr)

StMessMgr::StMessMgr() : ostrstream(new char[1024],1024,ios::out) {}
