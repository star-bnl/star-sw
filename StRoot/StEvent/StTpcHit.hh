/***************************************************************************
 *
 * $Id: StTpcHit.hh,v 1.1 1999/01/15 20:40:09 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.hh,v $
 * Revision 1.1  1999/01/15 20:40:09  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.3  1999/01/26 16:33:19  wenaus
 * StXXXHit table constructors
 *

 * version with constructors for table-based loading
#include "StHit.hh"
#include "StGlobalTrack.hh"
#include "StTrackCollection.hh"
#define StTpcHit_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StTpcHit : public StHit {
public:
    StTpcHit(const StThreeVector<float>&,
	     const StThreeVector<float>&,
	     float, unsigned char = 0);
    StTpcHit(dst_point_st* pt) : StHit(pt) {};
    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
};

#endif
