/***************************************************************************
 *
 * $Id: StSvtHit.hh,v 1.1 1999/01/15 20:40:06 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.hh,v $
 * Revision 1.1  1999/01/15 20:40:06  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.3  1999/01/26 16:33:17  wenaus
 * StXXXHit table constructors
 *

 * version with constructors for table-based loading
#include "StHit.hh"
#include "StGlobalTrack.hh"
#include "StTrackCollection.hh"
#define StSvtHit_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StSvtHit : public StHit {
public:
    StSvtHit(const StThreeVector<float>&,
	     const StThreeVector<float>&,
	     float, unsigned char = 0);
    StSvtHit(dst_point_st* pt) : StHit(pt) {};
    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
};

#endif
