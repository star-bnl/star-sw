/***************************************************************************
 *
 * $Id: StSvtHit.h,v 1.1 1999/01/30 03:58:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/03/04 15:57:02  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/01/26 16:33:17  wenaus
 * StXXXHit table constructors
 *
 * Revision 1.2  1999/01/15 22:53:55  wenaus
 * version with constructors for table-based loading
 *

#ifdef __ROOT__
#include "TObject.h"
#endif
#ifndef __CINT__
#include <vector>
#else
  template< class T > class vector;
#endif
class StVecPtrGlobalTrack;
class StGlobalTrackCollection;
#include "StTrackCollection.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StSvtHit(const StThreeVectorF&,
	     const StThreeVectorF&,
	     Float_t, UChar_t = 0);
    StSvtHit(dst_point_st* pt) : StHit(pt) {};
#if 0    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
#endif
#ifdef __ROOT__
	ClassDef(StSvtHit,1)  //StSvtHit structure
#endif
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);

public:
StCollectionDef(SvtHit)

    StSvtHit();

    ULong_t layer() const;
    ULong_t ladder() const;
    ULong_t wafer() const;
    ULong_t barrel() const;
    ULong_t wafer() const;      // wafer=[0-6]
    ClassDef(StSvtHit,1)  //StSvtHit structure
    ULong_t hybrid() const;

protected:
    static StMemoryPool mPool;  //!
    return ((mHardwarePosition>>4)%1000)/100 - 1;
}

#endif
