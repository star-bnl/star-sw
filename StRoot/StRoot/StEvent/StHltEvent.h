/***************************************************************************
 *
 * $Id: StHltEvent.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltEvent.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltEvent_hh
#define StHltEvent_hh

#include "StObject.h"
#include "StContainers.h"
#include "StThreeVectorF.hh"

class  StHltTrack;
class  StHltTrackNode;
class  StHltBEmcTowerHit; 
class  StHltBTofHit;
class  StHltVpdHit;
class  StHltHighPt;
class  StHltHeavyFragment;
class  StHltDiElectron;
class  StHltTriggerReason;

class StHltEvent : public StObject{
public:
    StHltEvent();
    virtual ~StHltEvent();
    
    unsigned int version() const;
    unsigned int triggerReasonBitOred() const;
    
    StThreeVectorF&   vertex();
    const StThreeVectorF& vertex() const;
    
    StThreeVectorF&   lowMultVertex();
    const StThreeVectorF& lowMultVertex() const;
    
    float vpdVertexZ() const;
    float t0() const;
    float innerSecGain() const;
    float outerSecGain() const;
    
    StSPtrVecHltTrack& globalTrack();
    const StSPtrVecHltTrack& globalTrack() const;
    StSPtrVecHltTrack& primaryTrack();
    const StSPtrVecHltTrack& primaryTrack() const;
    StSPtrVecHltTrackNode& trackNode();
    const StSPtrVecHltTrackNode& trackNode() const;
    StSPtrVecHltBEmcTowerHit& bEmcTowerHits();
    const StSPtrVecHltBEmcTowerHit& bEmcTowerHits() const;
    StSPtrVecHltBTofHit& bTofHit();
    const StSPtrVecHltBTofHit& bTofHit() const;
    StSPtrVecHltVpdHit& vpdHit();
    const StSPtrVecHltVpdHit& vpdHit() const;
    StSPtrVecHltHighPt& highPt();
    const StSPtrVecHltHighPt& highPt() const;
    StSPtrVecHltHeavyFragment& heavyFragment();
    const StSPtrVecHltHeavyFragment& heavyFragment() const;
    StSPtrVecHltDiElectron& diElectron();
    const StSPtrVecHltDiElectron& diElectron() const;
    StSPtrVecHltTriggerReason& triggerReason();
    const StSPtrVecHltTriggerReason& triggerReason() const;
    
    void addGlobalTrack(const StHltTrack*);
    void addPrimaryTrack(const StHltTrack*);
    void addTrackNode(const StHltTrackNode*);
    void addBEmcTowerHit(const StHltBEmcTowerHit*);
    void addBTofHit(const StHltBTofHit*);
    void addVpdHit(const StHltVpdHit*);
    void addHighPt(const StHltHighPt*);
    void addHeavyFragment(const StHltHeavyFragment*);
    void addDiElectron(const StHltDiElectron*);
    void addTriggerReason(const StHltTriggerReason*);
    
    void setVersion(unsigned int);
    void setTriggerReasonBitOred(unsigned int);
    void setVertex(const StThreeVectorF&);
    void setLowMultVertex(const StThreeVectorF&);
    void setVpdVertexZ(float);
    void setT0(float);
    void setInnerSecGain(float);
    void setOuterSecGain(float);
    
    
private:
    unsigned int mVersion;
    unsigned int mTriggerReasonBitOred; ///< StHltTriggerReason::reasonBit with "OR" operated.
    StThreeVectorF mVertex;
    StThreeVectorF mLowMultVertex;
    float mVpdVertexZ;
    float mT0;
    float mInnerSecGain; ///< dedx gain
    float mOuterSecGain; 
    
    StSPtrVecHltTrack mGlobalTrack;
    StSPtrVecHltTrack mPrimaryTrack;
    StSPtrVecHltTrackNode mTrackNode;
    StSPtrVecHltBEmcTowerHit mBEmcTowerHits;
    StSPtrVecHltBTofHit mBTofHit;
    StSPtrVecHltVpdHit  mVpdHit;
    StSPtrVecHltHighPt  mHighPt;
    StSPtrVecHltHeavyFragment  mHeavyFragment;
    StSPtrVecHltDiElectron  mDiElectron;
    StSPtrVecHltTriggerReason  mTriggerReason;
    
    ClassDef(StHltEvent,1)
};

inline unsigned int StHltEvent::version() const {return mVersion;}
inline unsigned int StHltEvent::triggerReasonBitOred() const {return mTriggerReasonBitOred;}
inline float StHltEvent::vpdVertexZ() const {return mVpdVertexZ;}
inline float StHltEvent::t0() const {return mT0;}
inline float StHltEvent::innerSecGain() const {return mInnerSecGain;}
inline float StHltEvent::outerSecGain() const {return mOuterSecGain;}

inline StSPtrVecHltTrack& StHltEvent::globalTrack() {return mGlobalTrack;}
inline const StSPtrVecHltTrack& StHltEvent::globalTrack() const {return mGlobalTrack;}
inline StSPtrVecHltTrack& StHltEvent::primaryTrack() {return mPrimaryTrack;}
inline const StSPtrVecHltTrack& StHltEvent::primaryTrack() const {return mPrimaryTrack;}
inline StSPtrVecHltTrackNode& StHltEvent::trackNode() {return mTrackNode;}
inline const StSPtrVecHltTrackNode& StHltEvent::trackNode() const {return mTrackNode;}
inline StSPtrVecHltBEmcTowerHit& StHltEvent::bEmcTowerHits() {return mBEmcTowerHits;}
inline const StSPtrVecHltBEmcTowerHit& StHltEvent::bEmcTowerHits() const {return mBEmcTowerHits;}
inline StSPtrVecHltBTofHit& StHltEvent::bTofHit() {return mBTofHit;}
inline const StSPtrVecHltBTofHit& StHltEvent::bTofHit() const {return mBTofHit;}
inline StSPtrVecHltVpdHit& StHltEvent::vpdHit() {return mVpdHit;}
inline const StSPtrVecHltVpdHit& StHltEvent::vpdHit() const {return mVpdHit;}
inline StSPtrVecHltHighPt& StHltEvent::highPt() {return mHighPt;}
inline const StSPtrVecHltHighPt& StHltEvent::highPt() const {return mHighPt;}
inline StSPtrVecHltHeavyFragment& StHltEvent::heavyFragment() {return mHeavyFragment;}
inline const StSPtrVecHltHeavyFragment& StHltEvent::heavyFragment() const {return mHeavyFragment;}
inline StSPtrVecHltDiElectron& StHltEvent::diElectron() {return mDiElectron;}
inline const StSPtrVecHltDiElectron& StHltEvent::diElectron() const {return mDiElectron;}
inline StSPtrVecHltTriggerReason& StHltEvent::triggerReason() {return mTriggerReason;}
inline const StSPtrVecHltTriggerReason& StHltEvent::triggerReason() const {return mTriggerReason;}


#endif



