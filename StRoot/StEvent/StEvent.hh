/***************************************************************************
 *
 * $Id: StEvent.hh,v 1.1 1999/01/15 20:39:44 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.hh,v $
 * Revision 1.1  1999/01/15 20:39:44  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.3  1999/01/30 23:03:11  wenaus
 * table load intfc change; include ref change
 *

 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StEvent_hh
#include "StEventSummary.hh"
#include "StRun.hh"
#include "StTrackCollection.hh"
#include "StFtpcHitCollection.hh"
#include "StVertexCollection.hh"
#include "StSvtHitCollection.hh"
#include "StTpcHitCollection.hh"
#include "StEmcHitCollection.hh"
#include "StSmdHitCollection.hh"
#include "StL0Trigger.hh"
#include "StTriggerDetectorCollection.hh"
#include "StEnumerations.hh"
#include "StEvent/StSmdEtaHitCollection.hh"
#include "StEvent/StL0Trigger.hh"
#include "StEvent/StTriggerDetectorCollection.hh"
#if !defined(ST_NO_NAMESPACES)
#endif

class StEvent {
public:
    StEvent();
    virtual ~StEvent();
    
    int operator==(const StEvent &right) const;
    int operator!=(const StEvent &right) const;

    void init(StRun* run=NULL);
    const string&                type() const;
    pair<long, long>             id() const;
    time_t                       time() const;
    StEventSummary*              summary();
    unsigned long                triggerMask() const;
    unsigned long                bunchCrossingNumber() const;
    double                       luminosity() const;
    StRun*                       run();
    StEmcHitCollection*          emcHitCollection();
    StSmdHitCollection*          smdHitCollection();
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
    void setSummary(StEventSummary*);                        
    void setTriggerMask(unsigned long);              
    void setBunchCrossingNumber(unsigned long);      
    void setLuminosity(double);               
    void setRun(StRun*);                            
    void setEmcHitCollection(StEmcHitCollection*);              
    void setSmdHitCollection(StSmdHitCollection*);              
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
    StEventSummary*              mSummary;
    unsigned long                mRunNumber;
    time_t                       mTime;
    unsigned long                mTriggerMask;
    unsigned long                mBunchCrossingNumber;
    double                       mLuminosity;
    StDstEventSummary*           mSummary;
    StRun*                       mRun;
    StEmcHitCollection*          mEmcHits;
    StSmdHitCollection*          mSmdHits;
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

inline StEventSummary* StEvent::summary() { return mSummary;}

inline StRun* StEvent::run() { return mRun;}

inline StVertex* StEvent::primaryVertex() { return mPrimaryVertex;}

inline StDstEventSummary* StEvent::summary() { return mSummary;}

inline StTrackCollection* StEvent::trackCollection() { return mTracks;}

inline StEmcHitCollection* StEvent::emcHitCollection() { return mEmcHits;}

inline StSmdHitCollection* StEvent::smdHitCollection() { return mSmdHits;}

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
