// $Id: StMessMgr.cxx,v 1.3 1999/10/28 16:06:58 genevb Exp $
// $Log: StMessMgr.cxx,v $
// Revision 1.3  1999/10/28 16:06:58  genevb
// Fixed bug in C msg_enable routine - same as earlier fix for StMessage routines
//
#define  StMessMgrImpl
#include "StMessMgr.h"

#ifdef __ROOT__
ClassImp(StMessMgr)
#endif

StMessMgr::StMessMgr() : ostrstream(new char[1024],1024,ios::out) {}
