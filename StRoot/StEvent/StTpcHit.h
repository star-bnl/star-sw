/***************************************************************************
 *
 * $Id: StTpcHit.h,v 1.1 1999/01/30 03:58:08 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.h,v $
 * Revision 1.1  1999/01/30 03:58:08  fisyak
 * Root Version of StEvent
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

#ifdef __ROOT__
#include "TObject.h"
 * Revision 2.2  1999/11/09 19:35:27  ullrich
#ifndef __CINT__
#include <vector>
#else
  template< class T > class vector;
#endif
#include "StHit.h"
#include "StGlobalTrack.h"
#include "StTrackCollection.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StTpcHit(const StThreeVectorF&,
	     const StThreeVectorF&,
	     Float_t, UChar_t = 0);
    StTpcHit(dst_point_st* pt) : StHit(pt) {};
#if 0    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
#endif
#ifdef __ROOT__
	ClassDef(StTpcHit,1)  //StTpcHit structure
#endif
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);

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
