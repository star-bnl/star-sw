/***************************************************************************
 *
 * $Id: StMcTrack.hh,v 2.4 2000/03/06 18:05:23 calderon Exp $
 * $Log: StMcTrack.hh,v $
 * Revision 2.4  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/03 00:51:53  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
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
#include <iostream.h>
#include "StLorentzVectorF.hh"
#include "StMcContainers.hh"
//#include "StTrackTopologyMap.h"
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
    const StLorentzVectorF&      fourMomentum() const; //!
    const StThreeVectorF&        momentum() const; //!
    float                        energy() const; //!
    float                        pt() const; //!
    float                        rapidity() const; //!
    float                        pseudoRapidity() const; //!
    StMcVertex*                  startVertex(); //!
    const StMcVertex*            startVertex() const; //!
    StMcVertex*                  stopVertex(); //!
    const StMcVertex*            stopVertex() const; //!
    StPtrVecMcVertex&            intermediateVertices(); //!
    const StPtrVecMcVertex&      intermediateVertices() const; //!
    StPtrVecMcTpcHit&            tpcHits(); //!
    const StPtrVecMcTpcHit&      tpcHits() const; //!
    StPtrVecMcSvtHit&            svtHits(); //!
    const StPtrVecMcSvtHit&      svtHits() const; //!
    StPtrVecMcFtpcHit&           ftpcHits(); //!
    const StPtrVecMcFtpcHit&     ftpcHits() const; //!
    StPtrVecMcRichHit&           richHits(); //!
    const StPtrVecMcRichHit&     richHits() const; //!
    StParticleDefinition*        particleDefinition(); //!
    const StParticleDefinition*  particleDefinition() const; //!
    int                          isShower() const; //! 1 = yes, 0 = no
    long                         geantId() const;  //!
    long                         key()     const;  //!

  // "Set" Methods
    void setFourMomentum(const StLorentzVectorF&); //!
    void setStartVertex(StMcVertex*); //!
    void setStopVertex(StMcVertex*); //!
    void setIntermediateVertices(StPtrVecMcVertex&); //!
    void setTpcHits(StPtrVecMcTpcHit&); //!
    void setSvtHits(StPtrVecMcSvtHit&); //!
    void setFtpcHits(StPtrVecMcFtpcHit&); //!
    void setRichHits(StPtrVecMcRichHit&); //!
  
    void setShower(char); //!
    void setGeantId(long); //!
    void setKey(long);     //!

    void addTpcHit(StMcTpcHit*); //!
    void addSvtHit(StMcSvtHit*); //!
    void addFtpcHit(StMcFtpcHit*); //!
    void addRichHit(StMcRichHit*); //!
    void removeTpcHit(StMcTpcHit*); //!
    void removeSvtHit(StMcSvtHit*); //!
    void removeFtpcHit(StMcFtpcHit*); //!
    void removeRichHit(StMcRichHit*); //!

    //    void setTopologyMap(StTrackTopologyMap&); //!
    
protected:
    StLorentzVectorF       mFourMomentum; //! 
    StMcVertex*            mStartVertex; //!
    StMcVertex*            mStopVertex; //!
    StPtrVecMcVertex       mIntermediateVertices; //!
    StPtrVecMcTpcHit       mTpcHits; //!
    StPtrVecMcSvtHit       mSvtHits; //!
    StPtrVecMcFtpcHit      mFtpcHits; //!
    StPtrVecMcRichHit      mRichHits; //!
    StParticleDefinition*  mParticleDefinition; //!
    char                   mIsShower; //!
    long                   mGeantId; //!
    long                   mKey;     //!
    //    StTrackTopologyMap     mTopologyMap; //!
};

ostream&  operator<<(ostream& os, const StMcTrack&);

inline const StLorentzVectorF& StMcTrack::fourMomentum() const { return mFourMomentum; }

inline const StThreeVectorF& StMcTrack::momentum() const { return mFourMomentum.vect(); }

inline float StMcTrack::energy() const { return mFourMomentum.e(); }

inline float StMcTrack::pt() const { return mFourMomentum.perp(); }

inline float StMcTrack::rapidity() const { return mFourMomentum.rapidity(); }

inline float StMcTrack::pseudoRapidity() const { return mFourMomentum.pseudoRapidity(); }

inline StMcVertex* StMcTrack::startVertex() { return mStartVertex; }

inline const StMcVertex* StMcTrack::startVertex() const { return mStartVertex; }

inline StMcVertex* StMcTrack::stopVertex() { return mStopVertex; }

inline const StMcVertex* StMcTrack::stopVertex() const { return mStopVertex; }

inline StPtrVecMcVertex& StMcTrack::intermediateVertices() { return mIntermediateVertices; }

inline const StPtrVecMcVertex& StMcTrack::intermediateVertices() const { return mIntermediateVertices; }

inline StPtrVecMcTpcHit& StMcTrack::tpcHits() { return mTpcHits; }

inline const StPtrVecMcTpcHit& StMcTrack::tpcHits() const { return mTpcHits; }

inline StPtrVecMcSvtHit& StMcTrack::svtHits() { return mSvtHits; }

inline const StPtrVecMcSvtHit& StMcTrack::svtHits() const { return mSvtHits; }

inline StPtrVecMcFtpcHit& StMcTrack::ftpcHits() { return mFtpcHits; }

inline const StPtrVecMcFtpcHit& StMcTrack::ftpcHits() const { return mFtpcHits; }

inline StPtrVecMcRichHit& StMcTrack::richHits() { return mRichHits; }

inline const StPtrVecMcRichHit& StMcTrack::richHits() const { return mRichHits; }

inline StParticleDefinition* StMcTrack::particleDefinition() { return mParticleDefinition; }

inline const StParticleDefinition* StMcTrack::particleDefinition() const { return mParticleDefinition; }

inline int StMcTrack::isShower() const { return mIsShower; }

inline long StMcTrack::geantId() const { return mGeantId; }

#endif




