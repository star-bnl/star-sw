/***************************************************************************
 *
 * $Id: StSvtHit.h,v 1.2 1999/04/27 01:24:25 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.h,v $
 * Revision 1.2  1999/04/27 01:24:25  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.3  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.6  1999/03/23 21:51:16  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.5  1999/03/04 18:17:21  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
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

 *
class StVecPtrGlobalTrack;
class StGlobalTrackCollection;
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
#include "StGlobalTrack.h"
 *
 * Revision 2.2  1999/11/04 21:40:57  ullrich
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StSvtHit(const StThreeVectorF&,
	     const StThreeVectorF&,
	     Float_t, UChar_t = 0);
    StSvtHit(dst_point_st*);
#if 0    
    StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);
#endif
    ClassDef(StSvtHit,1)  //StSvtHit structure
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);
#endif
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
