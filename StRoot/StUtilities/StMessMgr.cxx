// $Id: StMessMgr.cxx,v 1.4 2000/01/05 19:53:45 genevb Exp $
// $Log: StMessMgr.cxx,v $
// Revision 1.4  2000/01/05 19:53:45  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.3  1999/10/28 16:06:58  genevb
// Fixed bug in C msg_enable routine - same as earlier fix for StMessage routines
//
#include "StMessMgr.h"

#ifdef __ROOT__
ClassImp(StMessMgr)
#endif

StMessMgr::StMessMgr() : ostrstream(new char[1024],1024,ios::out) {}
