/***************************************************************************
 *
 * $Id: StMcTrack.hh,v 2.1 1999/11/19 19:06:34 calderon Exp $
 * $Log: StMcTrack.hh,v $
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:17  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTrack_hh
#define StMcTrack_hh 
#include <algorithm>
#include "StLorentzVectorF.hh"
#include "StMcContainers.hh"
#include "StTrackTopologyMap.h"
class StParticleDefinition;
class g2t_track_st;
class StMcTrack {
public:
    StMcTrack();
    virtual ~StMcTrack();
    StMcTrack(g2t_track_st* trk);
    
    // StMcTrack(const StMcTrack&);                     use default
    // const StMcTrack & operator=(const StMcTrack&);   use default
    
    int operator==(const StMcTrack&) const;
    int operator!=(const StMcTrack&) const;

    void initToZero();
  // "Get" Methods
    StLorentzVectorF&      fourMomentum(); //!
    const StThreeVectorF&  momentum() const; //!
    float                  energy(); //!
    float                  pt(); //!
    float                  rapidity(); //!
    float                  pseudoRapidity(); //!
    StMcVertex*            startVertex(); //!
    StMcVertex*            stopVertex(); //!
    StPtrVecMcVertex&      intermediateVertices(); //!
    StPtrVecMcTpcHit&      tpcHits(); //!
    StPtrVecMcSvtHit&      svtHits(); //!
    StPtrVecMcFtpcHit&     ftpcHits(); //!
    StParticleDefinition*  particleDefinition(); //!
    char                   isShower(); //!
    long                   geantId(); //!


  // "Set" Methods
    void setFourMomentum(const StLorentzVectorF&); //!
    void setStartVertex(StMcVertex*); //!
    void setStopVertex(StMcVertex*); //!
    void setIntermediateVertices(StPtrVecMcVertex&); //!
    void setTpcHits(StPtrVecMcTpcHit&); //!
    void setSvtHits(StPtrVecMcSvtHit&); //!
    void setFtpcHits(StPtrVecMcFtpcHit&); //!
  
    void setShower(char); //!
    void setGeantId(long); //!

    void addTpcHit(StMcTpcHit*); //!
    void addFtpcHit(StMcFtpcHit*); //!
    void addSvtHit(StMcSvtHit*); //!
    void removeTpcHit(StMcTpcHit*); //!
    void removeFtpcHit(StMcFtpcHit*); //!
    void removeSvtHit(StMcSvtHit*); //!

    void setTopologyMap(StTrackTopologyMap&); //!
    
protected:
    StLorentzVectorF       mFourMomentum; //! 
    StMcVertex*            mStartVertex; //!
    StMcVertex*            mStopVertex; //!
    StPtrVecMcVertex       mIntermediateVertices; //!
    StPtrVecMcTpcHit       mTpcHits; //!
    StPtrVecMcSvtHit       mSvtHits; //!
    StPtrVecMcFtpcHit      mFtpcHits; //!
    StParticleDefinition*  mParticleDefinition; //!
    char                   mIsShower; //!
    long                   mGeantId; //!
    StTrackTopologyMap     mTopologyMap; //!
};

inline StLorentzVectorF& StMcTrack::fourMomentum() { return mFourMomentum; }

inline const StThreeVectorF& StMcTrack::momentum() const { return mFourMomentum.vect(); }

inline float StMcTrack::energy() { return mFourMomentum.e(); }

inline float StMcTrack::pt() { return mFourMomentum.perp(); }

inline float StMcTrack::rapidity() { return mFourMomentum.rapidity(); }

inline float StMcTrack::pseudoRapidity() { return mFourMomentum.pseudoRapidity(); }

inline StMcVertex* StMcTrack::startVertex() { return mStartVertex; }

inline StMcVertex* StMcTrack::stopVertex() { return mStopVertex; }

inline StPtrVecMcVertex& StMcTrack::intermediateVertices() { return mIntermediateVertices; }

inline StPtrVecMcTpcHit& StMcTrack::tpcHits() { return mTpcHits; }

inline StPtrVecMcSvtHit& StMcTrack::svtHits() { return mSvtHits; }

inline StPtrVecMcFtpcHit& StMcTrack::ftpcHits() { return mFtpcHits; }

inline StParticleDefinition* StMcTrack::particleDefinition() { return mParticleDefinition; }

inline char StMcTrack::isShower() { return mIsShower; }

inline long StMcTrack::geantId() { return mGeantId; }

#endif




