/***************************************************************************
 *
 * $Id: StFtpcHit.hh,v 1.2 1999/01/15 22:53:43 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.hh,v $
 * Revision 1.2  1999/01/15 22:53:43  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.3  1999/01/26 16:33:14  wenaus
 * StXXXHit table constructors
 *

 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StFtpcHit_hh
#define StFtpcHit_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StFtpcHit : public StHit {
public:
    StFtpcHit(const StThreeVector<float>&,
	      const StThreeVector<float>&,
	      float, unsigned char = 0);
    StFtpcHit(dst_point_st* pt) : StHit(pt) {};
    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
};

#endif
