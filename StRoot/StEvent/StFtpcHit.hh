/***************************************************************************
 *
 * $Id: StFtpcHit.hh,v 1.6 1999/03/23 21:51:05 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.hh,v $
 * Revision 1.6  1999/03/23 21:51:05  ullrich
 * Added table-based class specific constructor.
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
 **************************************************************************/
#ifndef StFtpcHit_hh
#define StFtpcHit_hh

#include "StEvent/StHit.hh"
#include "StEvent/StTrackCollection.hh"
#include "StEvent/StVecPtrGlobalTrack.hh"
class dst_point_st;

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StFtpcHit : public StHit {
public:
    StFtpcHit(const StThreeVector<float>&,
	      const StThreeVector<float>&,
	      float, unsigned char = 0);
    StFtpcHit(dst_point_st*);
    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
};

#endif
