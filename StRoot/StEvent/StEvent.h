/***************************************************************************
 *
 * $Id: StEvent.h,v 1.8 1999/05/05 22:36:39 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.h,v $
 * Revision 1.8  1999/05/05 22:36:39  fisyak
 * restore relatedTracks
 *
 * Revision 1.8  1999/05/05 22:36:39  fisyak
 * restore relatedTracks
 *
 * Revision 1.7  1999/05/04 20:59:24  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.6  1999/05/03 01:36:18  fisyak
 * Add Print
 *
 * Revision 1.5  1999/04/30 13:16:28  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:32  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.7  1999/03/23 21:47:43  ullrich
 * Member function made virtual
 *
 * Revision 1.6  1999/03/04 18:17:00  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.5  1999/03/04 15:56:56  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.4  1999/02/23 21:20:06  ullrich
 * Modified EMC hit collections.
 *
 * Revision 1.3  1999/01/30 23:03:11  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:40  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.9  2000/05/15 18:35:37  ullrich
 * All data member related to collections and containers are now
 * kept by pointer. The interface (public methods) stays the same.
class StCtbCounterCollection;
class StEmcHitCollection;
class StEmcPreShowerHitCollection;
class StEmcTowerHitCollection;
class StFtpcHitCollection;
class StGlobalTrackCollection;
class StMwcSectorCollection;
class StSmdEtaHitCollection;
class StSmdHitCollection;
class StSmdPhiHitCollection;
class StSvtHitCollection;
class StTpcHitCollection;
class StVertexCollection;
class StVpdCounterCollection;
class StZdcSegmentCollection;
class StCtbCounterIterator;
class StEmcHitIterator;
class StEmcPreShowerHitIterator;
class StEmcTowerHitIterator;
class StFtpcHitIterator;
class StGlobalTrackIterator;
class StMwcSectorIterator;
class StSmdEtaHitIterator;
class StSmdHitIterator;
class StSmdPhiHitIterator;
class StSvtHitIterator;
class StTpcHitIterator;
class StVertexIterator;
class StVpdCounterIterator;
class StZdcSegmentIterator;
class StVecPtrCtbCounter;
class StVecPtrEmcHit;
class StVecPtrEmcPreShowerHit;
class StVecPtrEmcTowerHit;
class StVecPtrFtpcHit;
class StVecPtrGlobalTrack;
class StVecPtrMwcSector;
class StVecPtrSmdEtaHit;
class StVecPtrSmdHit;
class StVecPtrSmdPhiHit;
class StVecPtrSvtHit;
class StVecPtrTpcHit;
class StVecPtrVertex;
class StVecPtrVpdCounter;
class StVecPtrZdcSegment;
 * Those methods which returns references were modified to create
 * an empty collection in case the pointer is null.
#include "TDatime.h"
#ifndef __CINT__
#include <iostream.h>
#endif
#ifndef __ROOT__
#include <utility>
#endif
 *
#ifndef __ROOT__
#include <time.h>
#endif
#include "StDstEventSummary.h"
#include "StRun.h"
#include "StGlobalTrack.h"
#include "StFtpcHit.h"
#include "StVertex.h"
#include "StSvtHit.h"
#include "StTpcHit.h"
#include "StEmcTowerHit.h"
#include "StEmcPreShowerHit.h"
#include "StSmdPhiHit.h"
#include "StSmdEtaHit.h"
#include "tables/dst_event_header.h"
#include "tables/dst_event_summary.h"
#include "StEnumerations.h"
#include "dst_event_header.h"
#include "dst_event_summary.h"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
struct pairL : public TObject
{
  Long_t first;
  Long_t second;
  pairL(Long_t a=0, Long_t b=0): first(a), second(b) {};
};
 *
 * Revision 2.2  1999/11/04 13:30:42  ullrich
 * Added constructor without summary table
 * Adapted new StArray version. First version to compile on Linux and Sun.
    StEvent(StRun*, dst_event_header_st&, dst_event_summary_st&);
 **************************************************************************/
#include "StTrackDetectorInfo.h"
    Int_t operator==(const StEvent &right) const;
    Int_t operator!=(const StEvent &right) const;
    virtual const char*      type() const {return (const char *) mType;};
    void init(StRun* run=NULL);
    virtual void     Browse(TBrowser *b);
    virtual const char*      type() const ;
    virtual pairL            id() const { return mId;};
    virtual TDatime          GetDateTime() const {return mTime;};
    virtual Long_t           time()        const {return GetDateTime().GetTime();};
    virtual ULong_t          GetUTime()    const {return GetDateTime().Get();};
    virtual Int_t     	     GetDate()     const {return GetDateTime().GetDate();};
    virtual Int_t     	     GetTime()     const {return GetDateTime().GetTime();};
//		must be in here in .h
    virtual ULong_t          runNumber() const  { return mRunNumber;};             
    virtual ULong_t          triggerMask() const{ return mTriggerMask;};
    virtual StRun*                       run() { return mRun;};
    virtual Double_t         luminosity() const{ return mLuminosity;};
    virtual void     Print(Option_t *opt=""); // *MENU*
    //    virtual StRun*                       run() { return mRun;};
    virtual StVertex*                    primaryVertex(){ return mPrimaryVertex;};
    virtual StDstEventSummary*           summary(){ return mSummary;};
    virtual StGlobalTrackCollection*     trackCollection(){ return mTracks;};
    virtual StTpcHitCollection*          tpcHitCollection(){ return mTpcHits;};
    virtual StEmcPreShowerHitCollection* emcPreShowerHitCollection() { return mEmcPreShowerHits;};
    virtual StFtpcHitCollection*         ftpcHitCollection(){ return mFtpcHits;};
    virtual StEmcTowerHitCollection*     emcTowerHitCollection(){ return mEmcTowerHits;};
    virtual StEmcPreShowerHitCollection* emcPreShowerHitCollection(){ return mEmcPreShowerHits;};
    virtual StSmdPhiHitCollection*       smdPhiHitCollection(){ return mSmdPhiHits;};
    virtual StL0Trigger*                 l0Trigger(){ return mL0Trigger;};                        
    virtual StVertexCollection*          vertexCollection(){ return mVertices;};
    virtual StTriggerDetectorCollection* triggerDetectorCollection(){ return mTriggerDetectors;};
    virtual StL0Trigger*                 l0Trigger(){ return  mL0Trigger;};                        
    virtual Float_t          beamPolarization(StBeamDirection, StBeamPolarizationAxis);

    virtual void setType(const Char_t*);                                   // *MENU*
    virtual void setId(const pairL&);                                      // *MENU*
    virtual void setTime(Int_t dt)                 {SetDateTime(dt,0);};   // *MENU*
    virtual void SetDateTime(int iDate,int iTime){mTime.Set(iDate,iTime);};// *MENU*
    virtual void SetDateTime(TDatime dt)	 {mTime=dt;};              // *MENU*
    virtual void setRunNumber(ULong_t);                                    // *MENU*
    virtual void setRun(StRun*);                                           // *MENU*
    virtual void setBunchCrossingNumber(ULong_t);                          // *MENU*
    virtual void setLuminosity(Double_t);                                  // *MENU*
    //    virtual void setRun(StRun*);                                           // *MENU*
    virtual void setPrimaryVertex(StVertex*);                              
    virtual void setSummary(StDstEventSummary*);                           
    virtual void setTrackCollection(StGlobalTrackCollection*);             
    virtual void setTpcHitCollection(StTpcHitCollection*);                 
    virtual void setSvtHitCollection(StSvtHitCollection*);                 
    virtual void setFtpcHitCollection(StFtpcHitCollection*);              
    virtual void setEmcTowerHitCollection(StEmcTowerHitCollection*);              
    virtual void setEmcPreShowerHitCollection(StEmcPreShowerHitCollection*);              
    virtual void setSmdPhiHitCollection(StSmdPhiHitCollection*);              
    virtual void setSmdEtaHitCollection(StSmdEtaHitCollection*);              
    virtual void setVertexCollection(StVertexCollection*);               
    virtual void setTriggerDetectorCollection(StTriggerDetectorCollection*);      
    virtual void setL0Trigger(StL0Trigger*);                      
    virtual void setBeamPolarization(StBeamDirection, StBeamPolarizationAxis, Float_t); // *MENU*
    const StFtpcHitCollection*          ftpcHitCollection() const;
    
    
    pairL                        mId;        //!
    ULong_t                      mRunNumber;
    TDatime                      mTime;

    StRun*                       mRun;
    Double_t                     mLuminosity;
    StGlobalTrackCollection*     mTracks;
    StVertexCollection*          mVertices;
    StDstEventSummary*           mSummary;
    //    StRun*                       mRun;
    StVertex*                    mPrimaryVertex;
    StTpcHitCollection*          mTpcHits;
    StSvtHitCollection*          mSvtHits;
    StFtpcHitCollection*         mFtpcHits;
    StEmcTowerHitCollection*     mEmcTowerHits;
    StEmcPreShowerHitCollection* mEmcPreShowerHits;
    StTriggerDetectorCollection* mTriggerDetectors;
    StGlobalTrackCollection*     mTracks;
    StVertexCollection*          mVertices;
    StL0Trigger*                 mL0Trigger;                
    Float_t                      mBeamPolarizationEast[3];
    Float_t                      mBeamPolarizationWest[3];
    StFtpcHitCollection*         mFtpcHits;
    StSvtHitCollection*          mSvtHits;
    const StEvent& operator=(const StEvent&);
    StTriggerDetectorCollection* mTriggerDetectors;  
  ClassDef(StEvent,1)  //StEvent structure
    StL3Trigger*                 mL3Trigger;
#ifndef __CINT__
ostream&  operator<<(ostream& os, const StEvent&);
#endif

inline Float_t StEvent::beamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis)
{
    if (dir == east)
	return mBeamPolarizationEast[axis];
    else
	return mBeamPolarizationWest[axis];
}

    StSPtrVecTrackDetectorInfo   mTrackDetectorInfo;
    StSPtrVecTrackNode           mTrackNodes;
    StSPtrVecPrimaryVertex       mPrimaryVertices;
    StSPtrVecV0Vertex            mV0Vertices;
    StSPtrVecXiVertex            mXiVertices;
    StSPtrVecKinkVertex          mKinkVertices;

    static TString               mCvsTag;
    mutable StSPtrVecTrackDetectorInfo*  mTrackDetectorInfo;
    mutable StSPtrVecTrackNode*          mTrackNodes;

    mutable StSPtrVecPrimaryVertex*      mPrimaryVertices;
           mContentLength };
    
    mutable StSPtrVecObject  mContent;
    static TString           mCvsTag;

private:
    StEvent& operator=(const StEvent&);
    StEvent(const StEvent&);
    void initToZero();
    void init(const event_header_st&);
    
    ClassDef(StEvent,1)
};
#endif







