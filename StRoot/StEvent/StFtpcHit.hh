/***************************************************************************
 *
 * $Id: StFtpcHit.hh,v 1.4 1999/03/04 15:56:57 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.hh,v $
 * Revision 1.4  1999/03/04 15:56:57  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.5  1999/03/04 18:17:02  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.4  1999/03/04 15:56:57  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/01/26 16:33:14  wenaus
 * StXXXHit table constructors
 *
 * Revision 1.2  1999/01/15 22:53:43  wenaus
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
