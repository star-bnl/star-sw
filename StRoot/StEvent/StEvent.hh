/***************************************************************************
 *
 * $Id: StEvent.hh,v 1.4 1999/02/23 21:20:06 ullrich Exp $
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
 * $Log: StEvent.hh,v $
 * Revision 1.4  1999/02/23 21:20:06  ullrich
 * Modified EMC hit collections.
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

 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StEvent_hh
#define StEvent_hh

#include <iostream.h>
#include <utility>
#include <string>
#include <time.h>
#include "StEvent/StDstEventSummary.hh"
#include "StEvent/StRun.hh"
#include "StEvent/StTrackCollection.hh"
#include "StEvent/StFtpcHitCollection.hh"
#include "StEvent/StVertexCollection.hh"
#include "StEvent/StSvtHitCollection.hh"
#include "StEvent/StTpcHitCollection.hh"
#include "StEvent/StEmcTowerHitCollection.hh"
#include "StEvent/StEmcPreShowerHitCollection.hh"
#include "StEvent/StSmdPhiHitCollection.hh"
#include "StEvent/StSmdEtaHitCollection.hh"
#include "StEvent/StL0Trigger.hh"
#include "StEvent/StTriggerDetectorCollection.hh"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StEvent {
public:
    StEvent();
    StEvent(StRun*, dst_event_header_st&, dst_event_summary_st&);
    virtual ~StEvent();
    
    int operator==(const StEvent &right) const;
    int operator!=(const StEvent &right) const;

    void init(StRun* run=NULL);
    const string&                type() const;
    pair<long, long>             id() const;
    time_t                       time() const;
    unsigned long                runNumber() const;              
    unsigned long                triggerMask() const;
    unsigned long                bunchCrossingNumber() const;
    double                       luminosity() const;
    StRun*                       run();
    StVertex*                    primaryVertex();
    StDstEventSummary*              summary();
    StTrackCollection*           trackCollection();
    StTpcHitCollection*          tpcHitCollection();
    StSvtHitCollection*          svtHitCollection();
    StFtpcHitCollection*         ftpcHitCollection();
    StEmcTowerHitCollection*     emcTowerHitCollection();
    StEmcPreShowerHitCollection* emcPreShowerHitCollection();
    StSmdPhiHitCollection*       smdPhiHitCollection();
    StSmdEtaHitCollection*       smdEtaHitCollection();
    StVertexCollection*          vertexCollection();
    StTriggerDetectorCollection* triggerDetectorCollection();
    StL0Trigger*                 l0Trigger();                        
    float                        beamPolarization(StBeamDirection, StBeamPolarizationAxis);

    void setType(const char*);
    void setId(const pair<long, long>&);
    void setTime(time_t);
    void setRunNumber(unsigned long);                
    void setTriggerMask(unsigned long);              
    void setBunchCrossingNumber(unsigned long);      
    void setLuminosity(double);               
    void setRun(StRun*);                            
    void setPrimaryVertex(StVertex*);                  
    void setSummary(StDstEventSummary*);                        
    void setTrackCollection(StTrackCollection*);                
    void setTpcHitCollection(StTpcHitCollection*);               
    void setSvtHitCollection(StSvtHitCollection*);               
    void setFtpcHitCollection(StFtpcHitCollection*);              
    void setEmcTowerHitCollection(StEmcTowerHitCollection*);              
    void setEmcPreShowerHitCollection(StEmcPreShowerHitCollection*);              
    void setSmdPhiHitCollection(StSmdPhiHitCollection*);              
    void setSmdEtaHitCollection(StSmdEtaHitCollection*);              
    void setVertexCollection(StVertexCollection*);               
    void setTriggerDetectorCollection(StTriggerDetectorCollection*);      
    void setL0Trigger(StL0Trigger*);                      
    void setBeamPolarization(StBeamDirection, StBeamPolarizationAxis, float);                   

protected:
    string                       mType;
    pair<long, long>             mId;                      
    unsigned long                mRunNumber;
    time_t                       mTime;
    unsigned long                mTriggerMask;
    unsigned long                mBunchCrossingNumber;
    double                       mLuminosity;
    StDstEventSummary*           mSummary;
    StRun*                       mRun;
    StVertex*                    mPrimaryVertex;
    StTrackCollection*           mTracks;
    StVertexCollection*          mVertices;
    StTpcHitCollection*          mTpcHits;
    StSvtHitCollection*          mSvtHits;
    StFtpcHitCollection*         mFtpcHits;
    StEmcTowerHitCollection*     mEmcTowerHits;
    StEmcPreShowerHitCollection* mEmcPreShowerHits;
    StSmdPhiHitCollection*       mSmdPhiHits;
    StSmdEtaHitCollection*       mSmdEtaHits;
    StTriggerDetectorCollection* mTriggerDetectors;
    StL0Trigger*                 mL0Trigger;                
    float                        mBeamPolarizationEast[3];
    float                        mBeamPolarizationWest[3];

private:
    const StEvent& operator=(const StEvent&);
    StEvent(const StEvent&);
};

ostream&  operator<<(ostream& os, const StEvent&);

inline const string& StEvent::type() const { return mType;}

inline pair<long, long> StEvent::id() const { return mId;}

inline time_t StEvent::time() const { return mTime;}

inline unsigned long StEvent::runNumber() const { return mRunNumber;}             

inline unsigned long StEvent::triggerMask() const { return mTriggerMask;}

inline unsigned long StEvent::bunchCrossingNumber() const { return mBunchCrossingNumber;}

inline double StEvent::luminosity() const { return mLuminosity;}

inline StRun* StEvent::run() { return mRun;}

inline StVertex* StEvent::primaryVertex() { return mPrimaryVertex;}

inline StDstEventSummary* StEvent::summary() { return mSummary;}

inline StTrackCollection* StEvent::trackCollection() { return mTracks;}

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

inline float StEvent::beamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis)
{
    if (dir == east)
	return mBeamPolarizationEast[axis];
    else
	return mBeamPolarizationWest[axis];
}

#endif
