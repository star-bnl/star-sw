#include "StMessMgr.h"

#ifdef __ROOT__
ClassImp(StMessMgr)
#endif

StMessMgr::StMessMgr() : ostrstream() {}

//_____________________________________________________________________________
// $Id: StMessMgr.cxx,v 1.6 2003/09/25 21:19:22 genevb Exp $
// $Log: StMessMgr.cxx,v $
// Revision 1.6  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.5  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2000/01/05 19:53:45  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.3  1999/10/28 16:06:58  genevb
// Fixed bug in C msg_enable routine - same as earlier fix for StMessage routines
//
