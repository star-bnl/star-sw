/***************************************************************************
 *
 * $Id: StFtpcHit.h,v 1.2 1999/04/27 01:24:20 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.h,v $
 * Revision 1.2  1999/04/27 01:24:20  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.3  1999/04/28 22:27:32  fisyak
 * New version with pointer instead referencies
 *
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
 * Revision 2.3  1999/12/06 18:28:24  ullrich
 * Changed method names xxxInCluster to xxxInHit
 *
class StVecPtrGlobalTrack;
class StGlobalTrackCollection;
#include "tables/dst_point.h"
 * Memory now allocated using StMemoryPool via overloaded new/delete
#include "StGlobalTrack.h"
 *
 * Revision 2.1  1999/10/28 22:25:19  ullrich
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StFtpcHit(const StThreeVectorF&,
	      const StThreeVectorF&,
	      Float_t, UChar_t = 0);
    StFtpcHit(dst_point_st*);
#if 0    
    StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection&);
#endif
    ClassDef(StFtpcHit,1)  //StFtpcHit structure
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection*);
#endif
  ClassDef(StFtpcHit,1)  //StFtpcHit structure
    // StFtpcHit& operator=(const StFtpcHit&); use default
StCollectionDef(FtpcHit)

    
    ClassDef(StFtpcHit,1)  //StFtpcHit structure
    ULong_t padsInCluster() const;
    ULong_t plane() const;         // 0-19
    ULong_t padsInHit() const;
    ULong_t timebinsInHit() const;

protected:    
    static StMemoryPool mPool;  //!
    StObject* clone();
    ClassDef(StFtpcHit,1) 
};
#endif
