/***************************************************************************
 *
 * $Id: StSvtHit.hh,v 1.2 1999/01/15 22:53:55 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.hh,v $
 * Revision 1.2  1999/01/15 22:53:55  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.3  1999/01/26 16:33:17  wenaus
 * StXXXHit table constructors
 *

 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StSvtHit_hh
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
