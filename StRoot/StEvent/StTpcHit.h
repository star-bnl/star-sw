/***************************************************************************
 *
 * $Id: StTpcHit.h,v 1.3 1999/04/28 22:27:37 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 1.3  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.6  1999/03/23 21:51:12  ullrich
 * Added table-based class specific constructor.
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
 * Revision 2.4  1999/12/01 15:56:31  ullrich
 * Renamed xxxInCluster() methods to xxxInHit()
 *
class StVecPtrGlobalTrack;
class StGlobalTrackCollection;
#include "StArray.h"
 * Inlined sector() and padrow().
#include "StGlobalTrack.h"
#include "StGlobalTrack.h"
 *
 * Revision 2.2  1999/11/09 19:35:27  ullrich
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StTpcHit(const StThreeVectorF&,
	     const StThreeVectorF&,
	     Float_t, UChar_t = 0);
    StTpcHit(dst_point_st*);
    
    StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);
#ifndef __CINT__  
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);
#endif
  ClassDef(StTpcHit,1)  //StTpcHit structure
             ULong_t, Float_t, UChar_t = 0);
StCollectionDef(TpcHit)

    StTpcHit(const dst_point_st&);
    ~StTpcHit();

    void  operator delete(void* p) { mPool.free(p); }
    ClassDef(StTpcHit,1)  //StTpcHit structure
    ULong_t   pixelsInCluster() const;
    ULong_t   padrow() const;          // 0-44
    ULong_t   padsInHit() const;
    ULong_t   pixelsInHit() const;
inline ULong_t
StTpcHit::padrow() const
{
    return bits(9, 6)-1;   // bits 9-14, padrow=[0-44]
}

#endif
