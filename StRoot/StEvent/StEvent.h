/***************************************************************************
 *
 * $Id: StEvent.h,v 1.4 1999/04/28 22:27:32 fisyak Exp $
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
 * Revision 1.4  1999/04/28 22:27:32  fisyak
 * New version with pointer instead referencies
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
#include "TObject.h"
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

struct pairL
using namespace std;
    long first;
    long second;
    pairL(long, long);
    pairL();
  Long_t first;
  Long_t second;
class StEvent : public TObject {
};
 *
 * Revision 2.2  1999/11/04 13:30:42  ullrich
 * Added constructor without summary table
 * Adapted new StArray version. First version to compile on Linux and Sun.
    StEvent(StRun*, dst_event_header_st&, dst_event_summary_st&);
 **************************************************************************/
#include "StTrackDetectorInfo.h"
    Int_t operator==(const StEvent &right) const;
    virtual const TString&   type() const;
    virtual pairL            id() const;
    virtual Long_t           time()        const {return GetTime();};
    ULong_t     	     GetUTime()    const {return mTime.Get();};
    Int_t     	             GetDate()     const {return ((TDatime *)&mTime)->GetDate();};
    Int_t     	             GetTime()     const {return ((TDatime *)&mTime)->GetTime();};
    TDatime                  GetDateTime() const {return mTime;};
   
    virtual ULong_t                runNumber() const;              
    virtual ULong_t                triggerMask() const;
    virtual ULong_t                bunchCrossingNumber() const;
    virtual Double_t                       luminosity() const;
    virtual StRun*                       run();
    virtual StVertex*                    primaryVertex();
    virtual StDstEventSummary*           summary();
    virtual StGlobalTrackCollection*           trackCollection();
    virtual StTpcHitCollection*          tpcHitCollection();
    virtual StSvtHitCollection*          svtHitCollection();
    virtual StFtpcHitCollection*         ftpcHitCollection();
    virtual StEmcTowerHitCollection*     emcTowerHitCollection();
    virtual StEmcPreShowerHitCollection* emcPreShowerHitCollection();
    virtual StSmdPhiHitCollection*       smdPhiHitCollection();
    virtual StSmdEtaHitCollection*       smdEtaHitCollection();
    virtual StVertexCollection*          vertexCollection();
    virtual StTriggerDetectorCollection* triggerDetectorCollection();
    virtual StL0Trigger*                 l0Trigger();                        
    virtual Float_t                        beamPolarization(StBeamDirection, StBeamPolarizationAxis);

    virtual void setType(const Char_t*);
    virtual void setId(const pairL&);
    virtual void setTime(Int_t dt)                 {SetDateTime(dt,0);};
    virtual void SetDateTime(int iDate,int iTime){mTime.Set(iDate,iTime);};
    virtual void SetDateTime(TDatime dt)	 {mTime=dt;};
    virtual void setRunNumber(ULong_t);                
    virtual void setTriggerMask(ULong_t);              
    virtual void setBunchCrossingNumber(ULong_t);      
    virtual void setLuminosity(Double_t);               
    virtual void setRun(StRun*);                            
    virtual void setPrimaryVertex(StVertex*);                  
    virtual void setSummary(StDstEventSummary*);                        
    virtual void setTrackCollection(StGlobalTrackCollection*);                
    virtual void setTpcHitCollection(StTpcHitCollection*);               
    virtual void setSvtHitCollection(StSvtHitCollection*);               
    virtual void setTrackCollection(StGlobalTrackCollection*);             
    virtual void setTpcHitCollection(StTpcHitCollection*);                 
    virtual void setSvtHitCollection(StSvtHitCollection*);                 
    virtual void setFtpcHitCollection(StFtpcHitCollection*);              
    virtual void setEmcTowerHitCollection(StEmcTowerHitCollection*);              
    virtual void setEmcPreShowerHitCollection(StEmcPreShowerHitCollection*);              
    virtual void setSmdPhiHitCollection(StSmdPhiHitCollection*);              
    virtual void setSmdEtaHitCollection(StSmdEtaHitCollection*);              
    virtual void setBeamPolarization(StBeamDirection, StBeamPolarizationAxis, Float_t);                   
    virtual void setTriggerDetectorCollection(StTriggerDetectorCollection*);      
    virtual void setL0Trigger(StL0Trigger*);                      
    virtual void setBeamPolarization(StBeamDirection, StBeamPolarizationAxis, Float_t); // *MENU*
    pairL                        mId;                      
    
    
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
    Float_t                        mBeamPolarizationEast[3];
    Float_t                        mBeamPolarizationWest[3];
    StL0Trigger*                 mL0Trigger;                
    Float_t                      mBeamPolarizationEast[3];
    Float_t                      mBeamPolarizationWest[3];
    StFtpcHitCollection*         mFtpcHits;
    StSvtHitCollection*          mSvtHits;
    const StEvent& operator=(const StEvent&);
    StTriggerDetectorCollection* mTriggerDetectors;  
  ClassDef(StEvent,1)  //StEvent structure

inline const TString& StEvent::type() const { return mType;}

inline pairL StEvent::id() const { return mId;}

inline ULong_t StEvent::runNumber() const { return mRunNumber;}             

inline ULong_t StEvent::triggerMask() const { return mTriggerMask;}

inline ULong_t StEvent::bunchCrossingNumber() const { return mBunchCrossingNumber;}

inline Double_t StEvent::luminosity() const { return mLuminosity;}

inline StRun* StEvent::run() { return mRun;}

inline StVertex* StEvent::primaryVertex() { return mPrimaryVertex;}

inline StDstEventSummary* StEvent::summary() { return mSummary;}

inline StGlobalTrackCollection* StEvent::trackCollection() { return mTracks;}

inline StTpcHitCollection* StEvent::tpcHitCollection() { return mTpcHits;}

inline StSvtHitCollection* StEvent::svtHitCollection() { return mSvtHits;}

inline StFtpcHitCollection* StEvent::ftpcHitCollection() { return mFtpcHits;}

inline StEmcTowerHitCollection* StEvent::emcTowerHitCollection() { return mEmcTowerHits;}

inline StEmcPreShowerHitCollection* StEvent::emcPreShowerHitCollection() { return mEmcPreShowerHits;}

inline StSmdPhiHitCollection* StEvent::smdPhiHitCollection() { return mSmdPhiHits;}

inline StSmdEtaHitCollection* StEvent::smdEtaHitCollection() { return mSmdEtaHits;}

inline StVertexCollection* StEvent::vertexCollection() { return mVertices;}

inline StTriggerDetectorCollection* StEvent::triggerDetectorCollection() { return mTriggerDetectors;}

inline StL0Trigger* StEvent::l0Trigger() { return mL0Trigger;}                        
#ifndef __CINT__
ostream&  operator<<(ostream& os, const StEvent&);
#endif

inline Float_t StEvent::beamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis)
{
    if (dir == east)
	return mBeamPolarizationEast[axis];
#endif
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







