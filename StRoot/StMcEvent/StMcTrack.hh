/***************************************************************************
 *
 * $Id: StMcTrack.hh,v 1.3 1999/09/23 21:25:54 calderon Exp $
 * $Log: StMcTrack.hh,v $
 * Revision 1.3  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTrack_hh
#define StMcTrack_hh 
#include <algorithm>
#include "StThreeVectorF.hh"


#include "tables/g2t_track.h"

#include "StMcFtpcHitCollection.hh"
#include "StMcVertexCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcTpcHitCollection.hh"


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
    StThreeVectorF&        momentum();
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
    void setMomentum(const StThreeVectorF&);
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
    StThreeVectorF         mMomentum;
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

inline StThreeVectorF& StMcTrack::momentum() { return mMomentum; }

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




