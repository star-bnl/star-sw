/***********************************************************************
 *
 * $Id: StODMc.hh,v 2.0 2000/06/05 05:19:40 genevb Exp $
 * $Log: StODMc.hh,v $
 * Revision 2.0  2000/06/05 05:19:40  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo "One Daughter" (OD) micro dst class
 *
 ***********************************************************************/
#ifndef  STAR_StODMc
#define  STAR_StODMc
#include "StStrangeMuDst.hh"

class StMcVertex;
class StMcTrack;

class StODMc : public StStrangeMuDst {
public:
  StODMc();
  StODMc(StMcVertex*, StMcTrack*);
  ~StODMc();

  Int_t    decayMode() const;
  Int_t    geantIdParent() const;
  Int_t    geantIdDaughter() const;
  Float_t  parentMomentumX() const;
  Float_t  parentMomentumY() const;
  Float_t  parentMomentumZ() const;
  Float_t  daughterMomentumX() const;
  Float_t  daughterMomentumY() const;
  Float_t  daughterMomentumZ() const;
  Float_t  positionX() const;
  Float_t  positionY() const;
  Float_t  positionZ() const;

  void SetHitInfo(Int_t hits, Int_t commonHits);
  
  Int_t tpcHits() const;
  Int_t simTpcHits() const;
  Int_t commonTpcHits() const;
  
  Int_t mTpcHits;
  Int_t mSimTpcHits;
  Int_t  mCommonTpcHits;
  
  Int_t    mDecayMode;
  Int_t    mParentGeantId;
  Int_t    mDaughterGeantId;
  Float_t  mParentMomentumX;
  Float_t  mParentMomentumY;
  Float_t  mParentMomentumZ;
  Float_t  mDaughterMomentumX;
  Float_t  mDaughterMomentumY;
  Float_t  mDaughterMomentumZ;
  Float_t  mPositionX;
  Float_t  mPositionY;
  Float_t  mPositionZ;

private:
  ClassDef(StODMc,1)
};

inline Int_t StODMc::decayMode() const
             { return mDecayMode; }
inline void StODMc::SetHitInfo(Int_t hits, Int_t commonHits) 
             { mTpcHits = hits; mCommonTpcHits = commonHits; }
inline Int_t StODMc::tpcHits() const
             { return mTpcHits; }
inline Int_t StODMc::commonTpcHits() const
             { return mCommonTpcHits; }
inline Int_t StODMc::simTpcHits() const
             { return mSimTpcHits; }
inline Int_t StODMc::geantIdParent() const
             { return mParentGeantId; }
inline Int_t StODMc::geantIdDaughter() const
             { return mDaughterGeantId; }
inline Float_t StODMc::parentMomentumX() const
             { return mParentMomentumX; }
inline Float_t StODMc::parentMomentumY() const
             { return mParentMomentumY; }
inline Float_t StODMc::parentMomentumZ() const
             { return mParentMomentumZ; }
inline Float_t StODMc::daughterMomentumX() const
             { return mDaughterMomentumX; }
inline Float_t StODMc::daughterMomentumY() const
             { return mDaughterMomentumY; }
inline Float_t StODMc::daughterMomentumZ() const
             { return mDaughterMomentumZ; }
inline Float_t StODMc::positionX() const
             { return mPositionX; }
inline Float_t StODMc::positionY() const
             { return mPositionY; }
inline Float_t StODMc::positionZ() const
             { return mPositionZ; }

#endif
