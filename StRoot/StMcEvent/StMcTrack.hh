/***************************************************************************
 *
 * StMcTrack.hh
 *
 **************************************************************************/
#ifndef StMcTrack_hh
#define StMcTrack_hh 
#include "StThreeVector.hh"
#include <algorithm>

#include "StMcEvent/StMcFtpcHitCollection.hh"
#include "StMcEvent/StMcTpcHitCollection.hh"
#include "StMcEvent/StMcSvtHitCollection.hh"
#include "StMcEvent/StMcVertexCollection.hh"

//#include "StEvent/StParticle.hh"
#include "tables/g2t_track.h"

class StMcVertex;
class StParticleDefinition;

class StMcTrack {
public:
    StMcTrack();
    virtual ~StMcTrack();
    StMcTrack(g2t_track_st* trk);
    
    // StMcTrack(const StMcTrack&);                     use default
    // const StMcTrack & operator=(const StMcTrack&);   use default
    
    int operator==(const StMcTrack&) const;
    int operator!=(const StMcTrack&) const;

    void init();
  // "Get" Methods
    StThreeVector<float>&  momentum();
    StMcVertex*            startVertex();
    StMcVertex*            stopVertex();
    StMcVertexCollection*  intermediateVertices(); //do these need to be virtual and/or const 
    StMcTpcHitCollection*  tpcHits();
    StMcSvtHitCollection*  svtHits();
    StMcFtpcHitCollection* ftpcHits();
    StParticleDefinition*  particleDefinition();
    char                   isShower();
    long                   geantId();


  // "Set" Methods
    void setMomentum(const StThreeVector<float>&);
    void setStartVertex(StMcVertex*);
    void setStopVertex(StMcVertex*);
    void setIntermediateVertices(StMcVertexCollection*);
    void setTpcHits(StMcTpcHitCollection*);
    void setSvtHits(StMcSvtHitCollection*);
    void setFtpcHits(StMcFtpcHitCollection*);
  
    void setShower(char);
    void setGeantId(long);

    virtual void addTpcHit(StMcTpcHit*);
    virtual void addFtpcHit(StMcFtpcHit*);
    virtual void addSvtHit(StMcSvtHit*);
    virtual void removeTpcHit(StMcTpcHit*);
    virtual void removeFtpcHit(StMcFtpcHit*);
    virtual void removeSvtHit(StMcSvtHit*);
    
protected:
    StThreeVector<float>   mMomentum;
    StMcVertex*            mStartVertex;
    StMcVertex*            mStopVertex;
    StMcVertexCollection*  mIntermediateVertices;
    StMcTpcHitCollection*  mTpcHits;
    StMcSvtHitCollection*  mSvtHits;
    StMcFtpcHitCollection* mFtpcHits;
    StParticleDefinition*  mParticleDefinition;
    char                   mIsShower;
    long                   mGeantId;
};

inline StThreeVector<float>& StMcTrack::momentum() { return mMomentum; }

inline StMcVertex* StMcTrack::startVertex() { return mStartVertex; }

inline StMcVertex* StMcTrack::stopVertex() { return mStopVertex; }

inline StMcVertexCollection* StMcTrack::intermediateVertices() { return mIntermediateVertices; }

inline StMcTpcHitCollection* StMcTrack::tpcHits() { return mTpcHits; }

inline StMcSvtHitCollection* StMcTrack::svtHits() { return mSvtHits; }

inline StMcFtpcHitCollection* StMcTrack::ftpcHits() { return mFtpcHits; }

inline StParticleDefinition* StMcTrack::particleDefinition() { return mParticleDefinition; }

inline char StMcTrack::isShower() { return mIsShower; }

inline long StMcTrack::geantId() { return mGeantId; }

#endif




