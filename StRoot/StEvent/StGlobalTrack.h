/***************************************************************************
 *
 * $Id: StGlobalTrack.h,v 1.7 1999/09/24 01:23:01 fisyak Exp $
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
 * Revision 1.7  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/06/27 22:45:28  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.5  1999/06/16 10:50:19  ullrich
 * Added members to hold the number of hits in case
 * the hits are not stored on the DST. Sync changes in
 * StEvent with StRootEvent.
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
 * Revision 1.2  1999/01/15 22:53:45  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.0  1999/10/12 18:42:15  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
class StSvtHit;
class StTpcHit;
class StFtpcHit;
class StSvtHitCollection;
class StTpcHitCollection;
class StFtpcHitCollection;
class StFtpcHitIterator;
class StSvtHitIterator;
class StTpcHitIterator;
class StVecPtrFtpcHit;
class StVecPtrSvtHit;
class StVecPtrTpcHit;
#include "StArray.h"
#ifndef __ROOT__
#include <vector>
#endif
#include "StFtpcHit.h"
#include "StTpcHit.h"
#include "StSvtHit.h"
#include "StDedx.h"
#ifndef StGlobalTrack_hh
#include "StFtpcHit.h"
#include "StTrackPidTraits.h"
#include "dst_track.h"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
#define StGlobalTrack_hh

#include "StTrack.h"

    StGlobalTrack(const dst_track_st&);
    StGlobalTrack(dst_track_st* trk,
                  double curvature,
                  double dip,
                  double phase,
                  StThreeVectorD& origin,
		  int h);
    // StGlobalTrack(const StGlobalTrack&);     use default
    // const StGlobalTrack & operator=(const StGlobalTrack &);
    
    virtual const StVecPtrTpcHit&     tpcHits() const   { return *mTpcHits;   };
    virtual const StVecPtrSvtHit&     svtHits() const   { return *mSvtHits;   };
    virtual const StVecPtrFtpcHit&    ftpcHits() const  { return *mFtpcHits;  };
    virtual const StDedx*             svtDedx() const   { return  mSvtDedx;   };
    virtual const StDedx*             tpcDedx() const   { return  mTpcDedx;   };
    virtual const StDedx*             ftpcDedx() const  { return  mFtpcDedx;  }; 
    virtual const StTrackPidTraits&   pidTraits() const { return *mPidTraits; };
    StGlobalTrack(const StGlobalTrack&);
    virtual int  numberOfTpcHits() const;
    virtual int  numberOfSvtHits() const;
    virtual int  numberOfFtpcHits() const;

    virtual void setTpcDedx(StDedx*);      
    virtual void setFtpcDedx(StDedx*);     
    virtual void setSvtDedx(StDedx*);

    //
    //  These 3 set-functions only make sense if no hits are 
    //  available (e.g. they are not stored on the DST).
    //  Else the number of hits is simply determined by the size
    //  of the hit collections.
    //
    virtual void  setNumberOfTpcHits(unsigned char);
    virtual void  setNumberOfSvtHits(unsigned char);
    virtual void  setNumberOfFtpcHits(unsigned char);

    //
    // The following methods also manage the ref counting,
    // i.e. they increase or decrease the reference counter
    // of the referring hit (see StHit).
    //
    virtual void addTpcHit(StTpcHit*);
    virtual void addFtpcHit(StFtpcHit*);
    virtual void addSvtHit(StSvtHit*);
    virtual void removeTpcHit(StTpcHit*);
    virtual void removeFtpcHit(StFtpcHit*);
    virtual void removeSvtHit(StSvtHit*);

protected:
    StVecPtrTpcHit*   mTpcHits;
    StVecPtrSvtHit*   mSvtHits;
    StVecPtrFtpcHit*  mFtpcHits;
    StDedx*           mTpcDedx;
    StDedx*           mFtpcDedx;
    StDedx*           mSvtDedx;       
    StTrackPidTraits* mPidTraits;
    unsigned char     mNumberOfTpcHits;
    unsigned char     mNumberOfSvtHits;
    unsigned char     mNumberOfFtpcHits;
    
    ClassDef(StGlobalTrack,1)  //StGlobalTrack structure
    const StVertex* vertex() const;
StCollectionDef(GlobalTrack)
typedef StGlobalTrackIterator        StTrackIterator; 
typedef StGlobalTrackIterator        StTrackConstIterator; 
typedef StGlobalTrackCollection      StTrackCollection;
typedef StVecPtrGlobalTrack          StVecPtrTrack;



    ClassDef(StGlobalTrack,1)
};
#endif
