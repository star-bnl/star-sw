/***************************************************************************
 *
 * StMcEvent.hh
 *
 * 
 **************************************************************************/
#ifndef StMcEvent_hh
#define StMcEvent_hh

#include <iostream.h>
#include "StMcEvent/StMcTrackCollection.hh"
#include "StMcEvent/StMcFtpcHitCollection.hh"
#include "StMcEvent/StMcVertexCollection.hh"
#include "StMcEvent/StMcSvtHitCollection.hh"
#include "StMcEvent/StMcTpcHitCollection.hh"

#include "tables/g2t_event.h"


#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcVertex;

class StMcEvent {
    
public:
  StMcEvent();  
  StMcEvent(g2t_event_st&);
  virtual ~StMcEvent();
  
  int operator==(const StMcEvent&) const;
  int operator!=(const StMcEvent&) const;

  void init();
  //"Get" Methods

  
  
  // The following stuff will be read directly from g2t_event table
  
  unsigned long                eventGeneratorEventLabel() const;
  unsigned long                eventNumber() const;
  unsigned long                runNumber() const;              
  unsigned long                zWest() const;
  unsigned long                nWest() const;
  unsigned long                zEast() const;
  unsigned long                nEast() const;
  float                        impactParameter() const;
  float                        phiReactionPlane() const;
  float                        triggerTimeOffset() const;

  
  
  StMcVertex*                    primaryVertex();
  StMcVertexCollection*          vertexCollection();
  StMcTrackCollection*           trackCollection();
  StMcTpcHitCollection*          tpcHitCollection();
  StMcSvtHitCollection*          svtHitCollection();
  StMcFtpcHitCollection*         ftpcHitCollection();
  
  // "Set" Methods

  void setEventGeneratorEventLabel(unsigned long);
  void setEventNumber(unsigned long);
  void setRunNumber(unsigned long);
  void setZWest(unsigned long);
  void setNWest(unsigned long);
  void setZEast(unsigned long);
  void setNEast(unsigned long);
  void setImpactParameter(float);
  void setPhiReactionPlane(float);
  void setTriggerTimeOffset(float);
  void setPrimaryVertex(StMcVertex*);  
  void setVertexCollection(StMcVertexCollection*);      
  void setTrackCollection(StMcTrackCollection*);                
  void setTpcHitCollection(StMcTpcHitCollection*);               
  void setSvtHitCollection(StMcSvtHitCollection*);               
  void setFtpcHitCollection(StMcFtpcHitCollection*);              
  

protected:
  unsigned long                  mEventGeneratorEventLabel;
  unsigned long                  mEventNumber;
  unsigned long                  mRunNumber;
  unsigned long                  mZWest;
  unsigned long                  mNWest;
  unsigned long                  mZEast;
  unsigned long                  mNEast;
  float                          mImpactParameter;
  float                          mPhiReactionPlane;
  float                          mTriggerTimeOffset;
  StMcVertex*                    mPrimaryVertex;
  StMcVertexCollection*          mVertices;
  StMcTrackCollection*           mTracks;
  StMcTpcHitCollection*          mTpcHits;
  StMcSvtHitCollection*          mSvtHits;
  StMcFtpcHitCollection*         mFtpcHits;
  
private:
    const StMcEvent& operator=(const StMcEvent&);
    StMcEvent(const StMcEvent&);
};

ostream&  operator<<(ostream& os, const StMcEvent&);

// Definition of "Get" methods

inline unsigned long StMcEvent::eventGeneratorEventLabel() const { return mEventGeneratorEventLabel; }

inline unsigned long StMcEvent::eventNumber() const { return mEventNumber; }

inline unsigned long StMcEvent::runNumber() const { return mRunNumber;}             

inline unsigned long StMcEvent::zWest() const { return mZWest;}

inline unsigned long StMcEvent::nWest() const { return mNWest;}

inline unsigned long StMcEvent::zEast() const { return mZEast;}

inline unsigned long StMcEvent::nEast() const { return mNEast;}

inline float StMcEvent::impactParameter() const { return mImpactParameter; }

inline float StMcEvent::phiReactionPlane() const { return mPhiReactionPlane; }

inline float StMcEvent::triggerTimeOffset() const { return mTriggerTimeOffset;}

inline StMcVertex* StMcEvent::primaryVertex() { return mPrimaryVertex;}

inline StMcVertexCollection* StMcEvent::vertexCollection() { return mVertices;}

inline StMcTrackCollection* StMcEvent::trackCollection() { return mTracks;}

inline StMcTpcHitCollection* StMcEvent::tpcHitCollection() { return mTpcHits;}

inline StMcSvtHitCollection* StMcEvent::svtHitCollection() { return mSvtHits;}

inline StMcFtpcHitCollection* StMcEvent::ftpcHitCollection() { return mFtpcHits;}


#endif

