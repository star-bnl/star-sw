//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrackCollection.h,v 1.1 2000/03/24 22:37:42 nystrand Exp $
// $Log: StPeCTrackCollection.h,v $
// Revision 1.1  2000/03/24 22:37:42  nystrand
// First version
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCTrackCollection_h
#define StPeCTrackCollection_h
#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#include "StPrimaryTrack.h"
typedef vector<StTrack*>           StPeCPrimaryTrackCollection;
typedef vector<StTrack*>::iterator StPeCPrimaryTrackIterator;
typedef vector<StTrack*>           StPeCNonPrimaryTrackCollection;
typedef vector<StTrack*>::iterator StPeCNonPrimaryTrackIterator;
#endif /*__CINT__*/
#endif
