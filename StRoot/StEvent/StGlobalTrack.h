/***************************************************************************
 *
 * $Id: StGlobalTrack.h,v 1.2 1999/02/10 02:17:35 fisyak Exp $
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
 * Revision 1.2  1999/02/10 02:17:35  fisyak
 * Merging with new Torre stuff
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
#ifdef __ROOT__
#include "TObject.h"
#endif
#ifndef __CINT__
#include <vector>
#else
  template< class T > class vector;
#endif
class StVecPtrSvtHit;
class StVecPtrTpcHit;
#include "StArray.h"
#ifndef __ROOT__
#include <vector>
#include "StEmcHit.h"
#include "StSmdHit.h"
#include "StVecPtrTpcHit.h"
#include "StVecPtrSvtHit.h"
#include "StVecPtrFtpcHit.h"
#include "StTpcHit.h"
#include "StTrackPidTraits.h"
#include "dst_track.h"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
    StGlobalTrack(dst_track_st*);
                  Double_t phase,
                  double curvature,
		  Int_t h);
    const StVecPtrTpcHit&     tpcHits() const;
    const StVecPtrSvtHit&     svtHits() const;
    const StVecPtrFtpcHit&    ftpcHits() const;
    StDedx*                   svtDedx();
    StDedx*                   tpcDedx();
    StDedx*                   ftpcDedx();
    StEmcHit*                 emcHit();
    StSmdHit*                 smdHit();

    void setEmcHit(StEmcHit*);       
    void setSmdHit(StSmdHit*);       
    void setTpcDedx(StDedx*);      
    void setFtpcDedx(StDedx*);     
    void setSvtDedx(StDedx*);
    virtual int  numberOfSvtHits() const;
    virtual int  numberOfFtpcHits() const;

    virtual void setTpcDedx(StDedx*);      
    virtual void setFtpcDedx(StDedx*);     
    virtual void  setNumberOfFtpcHits(unsigned char);
    void addTpcHit(StTpcHit*);
    void addFtpcHit(StFtpcHit*);
    void addSvtHit(StSvtHit*);
    void removeTpcHit(StTpcHit*);
    void removeFtpcHit(StFtpcHit*);
    void removeSvtHit(StSvtHit*);
    virtual void addTpcHit(StTpcHit*);
    
    virtual void addFtpcHit(StFtpcHit*);
    StVecPtrTpcHit  mTpcHits;
    StVecPtrSvtHit  mSvtHits;
    StVecPtrFtpcHit mFtpcHits;
    StEmcHit*       mEmcHit;
    StSmdHit*       mSmdHit;
    StDedx*         mTpcDedx;
    StDedx*         mFtpcDedx;
    StDedx*         mSvtDedx;       
#ifdef __ROOT__
	ClassDef(StGlobalTrack,1)  //StGlobalTrack structure
#endif
    StVecPtrFtpcHit*  mFtpcHits;

inline const StVecPtrTpcHit& StGlobalTrack::tpcHits() const { return mTpcHits; }
typedef StGlobalTrackIterator        StTrackIterator; 
inline const StVecPtrSvtHit& StGlobalTrack::svtHits() const { return mSvtHits; }
inline const StVecPtrTpcHit* StGlobalTrack::tpcHits() const { return mTpcHits; }
inline const StVecPtrFtpcHit& StGlobalTrack::ftpcHits() const { return mFtpcHits; }
inline const StVecPtrSvtHit* StGlobalTrack::svtHits() const { return mSvtHits; }
inline StDedx* StGlobalTrack::svtDedx() { return mSvtDedx; }
inline const StDedx* StGlobalTrack::svtDedx() const { return mSvtDedx; }
inline StDedx* StGlobalTrack::tpcDedx() { return mTpcDedx; }
inline const StDedx* StGlobalTrack::tpcDedx() const { return mTpcDedx; }
inline StDedx* StGlobalTrack::ftpcDedx() { return mFtpcDedx; }

inline StEmcHit* StGlobalTrack::emcHit() { return mEmcHit; }

inline StSmdHit* StGlobalTrack::smdHit() { return mSmdHit; }


inline const StTrackPidTraits* StGlobalTrack::pidTraits() const { return mPidTraits; }
#endif
typedef StGlobalTrackIterator        StTrackConstIterator; 
typedef StGlobalTrackCollection      StTrackCollection;
typedef StVecPtrGlobalTrack          StVecPtrTrack;



    ClassDef(StGlobalTrack,1)
};
#endif
