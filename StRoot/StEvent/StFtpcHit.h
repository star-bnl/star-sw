/***************************************************************************
 *
 * $Id: StFtpcHit.h,v 1.1 1999/01/30 03:58:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.h,v $
 * Revision 1.1  1999/01/30 03:58:05  fisyak
 * Root Version of StEvent
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
 * Memory now allocated using StMemoryPool via overloaded new/delete
#ifdef __ROOT__
#include "TObject.h"
#include "StTemplates.hh"
 * Revision 2.1  1999/10/28 22:25:19  ullrich
#ifndef __CINT__
#include <vector>
#endif
#include "StHit.h"
#include "StTrackCollection.h"
#include "StVecPtrGlobalTrack.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
    StFtpcHit(const StThreeVectorF&,
	      const StThreeVectorF&,
	      Float_t, UChar_t = 0);
    StFtpcHit(dst_point_st* pt) : StHit(pt) {};
#if 0    
    StVecPtrGlobalTrack relatedTracks(const StTrackCollection&);
#endif
#ifdef __ROOT__
	ClassDef(StFtpcHit,1)  //StFtpcHit structure
#endif
  StVecPtrGlobalTrack relatedTracks(const StGlobalTrackCollection*);
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
