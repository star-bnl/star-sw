/***************************************************************************
 *
 * $Id: StGlobalTrack.hh,v 1.5 1999/02/15 16:17:03 wenaus Exp $
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
 * $Log: StGlobalTrack.hh,v $
 * Revision 1.5  1999/02/15 16:17:03  wenaus
 * fix double& -> double referencing bug
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
 * fix double& -> double referencing bug
 *
 * New track constructor to load helix params independently of table
 *
using namespace std;
#include "StEvent/StEmcHit.hh"
#include "StEvent/StSmdHit.hh"
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:45  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StGlobalTrack_hh
#define StGlobalTrack_hh

#include <vector>
#include "StEvent/StDedx.hh"
#include "StEvent/StTrack.hh"
#include "StEvent/StVecPtrSvtHit.hh"
#include "StEvent/StTrackPidTraits.hh"
#include "tables/dst_track.h"

                  StThreeVector<double>& origin);
#endif

class StGlobalTrack : public StTrack {
public:
    StGlobalTrack();
    ~StGlobalTrack();
    StEmcHit*                 emcHit();
    StSmdHit*                 smdHit();
    StGlobalTrack(dst_track_st* trk,
    void setEmcHit(StEmcHit*);       
    void setSmdHit(StSmdHit*);       
                  double curvature,
                  double dip,
    const StVecPtrTpcHit&     tpcHits() const;
    const StVecPtrSvtHit&     svtHits() const;
    const StVecPtrFtpcHit&    ftpcHits() const;
    StDedx*                   svtDedx();
    StDedx*                   tpcDedx();
    StDedx*                   ftpcDedx();
    virtual StDedx*                   svtDedx();
    void setTpcDedx(StDedx*);      
    void setFtpcDedx(StDedx*);     
    void setSvtDedx(StDedx*);
    virtual const StDedx*             ftpcDedx() const;
    virtual const StTrackPidTraits&   pidTraits() const;

    virtual void setTpcDedx(StDedx*);      
    virtual void setFtpcDedx(StDedx*);     
    virtual void setSvtDedx(StDedx*);
    void addTpcHit(StTpcHit*);
    void addFtpcHit(StFtpcHit*);
    void addSvtHit(StSvtHit*);
    StEmcHit*       mEmcHit;
    StSmdHit*       mSmdHit;
    void removeTpcHit(StTpcHit*);
    void removeFtpcHit(StFtpcHit*);
    void removeSvtHit(StSvtHit*);
    virtual void addTpcHit(StTpcHit*);
    
    virtual void addFtpcHit(StFtpcHit*);
    virtual void addSvtHit(StSvtHit*);
    virtual void removeTpcHit(StTpcHit*);
    virtual void removeFtpcHit(StFtpcHit*);
    StVecPtrTpcHit  mTpcHits;
    StVecPtrSvtHit  mSvtHits;
    StVecPtrFtpcHit mFtpcHits;
    StDedx*         mTpcDedx;
    StDedx*         mFtpcDedx;
    StDedx*         mSvtDedx;       
    StDedx*          mFtpcDedx;

inline StEmcHit* StGlobalTrack::emcHit() { return mEmcHit; }

inline StSmdHit* StGlobalTrack::smdHit() { return mSmdHit; }
    StDedx*          mSvtDedx;       
    StTrackPidTraits mPidTraits;
};

inline const StVecPtrTpcHit& StGlobalTrack::tpcHits() const { return mTpcHits; }

inline const StVecPtrSvtHit& StGlobalTrack::svtHits() const { return mSvtHits; }
inline StDedx* StGlobalTrack::svtDedx() { return mSvtDedx; }
inline const StDedx* StGlobalTrack::svtDedx() const { return mSvtDedx; }
inline StDedx* StGlobalTrack::tpcDedx() { return mTpcDedx; }
inline const StDedx* StGlobalTrack::tpcDedx() const { return mTpcDedx; }
inline StDedx* StGlobalTrack::ftpcDedx() { return mFtpcDedx; }
inline const StDedx* StGlobalTrack::ftpcDedx() const { return mFtpcDedx; }

inline const StTrackPidTraits& StGlobalTrack::pidTraits() const { return mPidTraits; }

#endif


