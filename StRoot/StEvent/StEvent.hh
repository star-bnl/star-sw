/***************************************************************************
 *
 * $Id: StEvent.hh,v 1.7 1999/03/23 21:47:43 ullrich Exp $
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
 **************************************************************************/
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
#include "StEvent/StEnumerations.hh"
#include "tables/dst_event_header.h"
#include "tables/dst_event_summary.h"

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
    virtual const string&                type() const;
    virtual pair<long, long>             id() const;
    virtual time_t                       time() const;
    virtual unsigned long                runNumber() const;              
    virtual unsigned long                triggerMask() const;
    virtual unsigned long                bunchCrossingNumber() const;
    virtual double                       luminosity() const;
    virtual StRun*                       run();
    virtual StVertex*                    primaryVertex();
    virtual StDstEventSummary*           summary();
    virtual StTrackCollection*           trackCollection();
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
    virtual float                        beamPolarization(StBeamDirection, StBeamPolarizationAxis);

    virtual void setType(const char*);
    virtual void setId(const pair<long, long>&);
    virtual void setTime(time_t);
    virtual void setRunNumber(unsigned long);                
    virtual void setTriggerMask(unsigned long);              
    virtual void setBunchCrossingNumber(unsigned long);      
    virtual void setLuminosity(double);               
    virtual void setRun(StRun*);                            
    virtual void setPrimaryVertex(StVertex*);                  
    virtual void setSummary(StDstEventSummary*);                        
    virtual void setTrackCollection(StTrackCollection*);                
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
    virtual void setBeamPolarization(StBeamDirection, StBeamPolarizationAxis, float);                   

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
