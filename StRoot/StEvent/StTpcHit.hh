/***************************************************************************
 *
 * $Id: StTpcHit.hh,v 1.4 1999/03/04 15:57:04 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.hh,v $
 * Revision 1.4  1999/03/04 15:57:04  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.5  1999/03/04 18:17:25  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.4  1999/03/04 15:57:04  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/01/26 16:33:19  wenaus
 * StXXXHit table constructors
 *
 * Revision 1.2  1999/01/15 22:53:59  wenaus
 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StTpcHit_hh
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
