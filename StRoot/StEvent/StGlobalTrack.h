/***************************************************************************
 *
 * $Id: StGlobalTrack.h,v 1.3 1999/04/27 01:24:20 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.h,v $
 * Revision 1.3  1999/04/27 01:24:20  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:33  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.11  1999/04/08 14:58:34  ullrich
 * Moved PID traits from StTrack to StGlobalTrack.
 *
 * Revision 1.10  1999/03/23 21:47:37  ullrich
 * Member function made virtual
 *
 * Revision 1.9  1999/03/04 18:17:04  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.8  1999/03/04 15:56:58  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.7  1999/02/24 12:49:06  ullrich
 * Added argument (h) to constructor needed to instatiate helix
 *
 * Revision 1.6  1999/02/23 21:23:59  ullrich
 * Removed obsolete EMC/SMD hit information (future cluster).
 *
 * Revision 1.5  1999/02/15 16:17:03  wenaus
 * fix Double_t& -> Double_t referencing bug
 *
 * Revision 1.4  1999/02/12 02:01:18  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.3  1999/01/30 23:03:12  wenaus
 * table load intfc change; include ref change
 *
 * Revision 2.0  1999/10/12 18:42:15  ullrich
class StTpcHit;
class StSvtHit;
class StSvtHitCollection;
class StFtpcHitCollection;
class StVecPtrSvtHit;
class StFtpcHitIterator;
class StVecPtrSvtHit;
class StVecPtrTpcHit;
#include "StArray.h"
#ifndef __ROOT__
#include <vector>
#include "StTpcHit.h"
#include "StDedx.h"
#include "tables/dst_track.h"
#include "StFtpcHit.h"
#include "StTrackPidTraits.h"
#include "dst_track.h"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
#define StGlobalTrack_hh

#include "StTrack.h"
                  StThreeVectorF& origin,
                  Double_t dip,
                  Double_t phase,
                  double curvature,
		  Int_t h);
                  double phase,
                  StThreeVectorD& origin,
		  int h);
    virtual StDedx*                   svtDedx();
    virtual StDedx*                   tpcDedx();
    virtual StDedx*                   ftpcDedx();
    virtual const StDedx*             tpcDedx() const;
    virtual const StDedx*             ftpcDedx() const;
    virtual const StTrackPidTraits*   pidTraits() const;
    virtual const StDedx*             tpcDedx() const   { return  mTpcDedx;   };
    virtual int  numberOfSvtHits() const;
    virtual int  numberOfFtpcHits() const;

    virtual void setTpcDedx(StDedx*);      
    virtual void setFtpcDedx(StDedx*);     
    virtual void  setNumberOfFtpcHits(unsigned char);

    //
    // The following methods also manage the ref counting,
    // i.e. they increase or decrease the reference counter
    // of the referring hit (see StHit).
    //
    virtual void addTpcHit(StTpcHit*);
    virtual void addFtpcHit(StFtpcHit*);
    StVecPtrTpcHit*  mTpcHits;
    StVecPtrSvtHit*  mSvtHits;
    StVecPtrFtpcHit* mFtpcHits;
    StDedx*         mTpcDedx;
    StDedx*         mFtpcDedx;
    StDedx*         mSvtDedx;       
    ClassDef(StGlobalTrack,1)  //StGlobalTrack structure
    StVecPtrFtpcHit*  mFtpcHits;
    StDedx*           mTpcDedx;

  ClassDef(StGlobalTrack,1)  //StGlobalTrack structure
    unsigned char     mNumberOfFtpcHits;
    
    ClassDef(StGlobalTrack,1)  //StGlobalTrack structure

    const StVertex* vertex() const;
StCollectionDef(GlobalTrack)
typedef StGlobalTrackIterator        StTrackIterator; 
#ifndef __CINT__
inline const StVecPtrTpcHit* StGlobalTrack::tpcHits() const { return mTpcHits; }

inline const StVecPtrSvtHit* StGlobalTrack::svtHits() const { return mSvtHits; }
inline StDedx* StGlobalTrack::svtDedx() { return mSvtDedx; }
inline const StDedx* StGlobalTrack::svtDedx() const { return mSvtDedx; }
inline StDedx* StGlobalTrack::tpcDedx() { return mTpcDedx; }
inline const StDedx* StGlobalTrack::tpcDedx() const { return mTpcDedx; }
inline StDedx* StGlobalTrack::ftpcDedx() { return mFtpcDedx; }
inline const StDedx* StGlobalTrack::ftpcDedx() const { return mFtpcDedx; }

inline const StTrackPidTraits* StGlobalTrack::pidTraits() const { return mPidTraits; }
#endif
typedef StGlobalTrackIterator        StTrackConstIterator; 
typedef StGlobalTrackCollection      StTrackCollection;
typedef StVecPtrGlobalTrack          StVecPtrTrack;



    ClassDef(StGlobalTrack,1)
};
#endif
