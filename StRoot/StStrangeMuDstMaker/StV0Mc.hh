/***********************************************************************
 *
 * $Id: StV0Mc.hh,v 2.0 2000/06/05 05:19:45 genevb Exp $
 * $Log: StV0Mc.hh,v $
 * Revision 2.0  2000/06/05 05:19:45  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo V0 micro dst class
 *
 ***********************************************************************/
#ifndef  STAR_StV0Mc
#define  STAR_StV0Mc
#include "StStrangeMuDst.hh"

class StMcVertex;
class StMcTrack;

class StV0Mc : public StStrangeMuDst
{

public:
  StV0Mc();
  StV0Mc(StMcVertex*, StMcTrack*, StMcTrack*);
  ~StV0Mc();

  Int_t    decayMode() const;
  Int_t    geantIdParent() const;
  Int_t    geantIdPositive() const;
  Int_t    geantIdNegative() const;
  Float_t  parentMomentumX() const;
  Float_t  parentMomentumY() const;
  Float_t  parentMomentumZ() const;
  Float_t  positiveMomentumX() const;
  Float_t  positiveMomentumY() const;
  Float_t  positiveMomentumZ() const;
  Float_t  negativeMomentumX() const;
  Float_t  negativeMomentumY() const;
  Float_t  negativeMomentumZ() const;
  Float_t  positionX() const;
  Float_t  positionY() const;
  Float_t  positionZ() const;

  Int_t positiveTpcHits() const;
  Int_t positiveSimTpcHits() const;
  Int_t positiveCommonTpcHits() const;
  Int_t negativeTpcHits() const;
  Int_t negativeSimTpcHits() const;
  Int_t negativeCommonTpcHits() const;

  void SetHitInfoPositive(Int_t hits, Int_t commonHits);
  void SetHitInfoNegative(Int_t hits, Int_t commonHits);
  
  Int_t mPositiveTpcHits;
  Int_t mPositiveSimTpcHits;
  Int_t mPositiveCommonTpcHits;
  
  Int_t mNegativeTpcHits;
  Int_t mNegativeSimTpcHits;
  Int_t mNegativeCommonTpcHits;

  Int_t    mDecayMode;
  Int_t    mParentGeantId;
  Int_t    mPositiveGeantId;
  Int_t    mNegativeGeantId;
  Float_t  mParentMomentumX;
  Float_t  mParentMomentumY;
  Float_t  mParentMomentumZ;
  Float_t  mPositiveMomentumX;
  Float_t  mPositiveMomentumY;
  Float_t  mPositiveMomentumZ;
  Float_t  mNegativeMomentumX;
  Float_t  mNegativeMomentumY;
  Float_t  mNegativeMomentumZ;
  Float_t  mPositionX;
  Float_t  mPositionY;
  Float_t  mPositionZ;

private:
  ClassDef(StV0Mc,1)
};

inline Int_t StV0Mc::decayMode() const
            { return mDecayMode; }
inline void StV0Mc::SetHitInfoPositive(Int_t hits, Int_t commonHits)
            { mPositiveTpcHits = hits; mPositiveCommonTpcHits = commonHits; }
inline Int_t StV0Mc::positiveTpcHits() const
            { return mPositiveTpcHits; }
inline Int_t StV0Mc::positiveCommonTpcHits() const
            { return mPositiveCommonTpcHits; }
inline Int_t StV0Mc::positiveSimTpcHits() const
            { return mPositiveSimTpcHits; }
inline void StV0Mc::SetHitInfoNegative(Int_t hits, Int_t commonHits)
            { mNegativeTpcHits = hits; mNegativeCommonTpcHits = commonHits; }
inline Int_t StV0Mc::negativeTpcHits() const
            { return mNegativeTpcHits; }
inline Int_t StV0Mc::negativeCommonTpcHits() const
            { return mNegativeCommonTpcHits; }
inline Int_t StV0Mc::negativeSimTpcHits() const
            { return mNegativeSimTpcHits; }
inline Int_t StV0Mc::geantIdParent() const
            { return mParentGeantId; }
inline Int_t StV0Mc::geantIdPositive() const
            { return mPositiveGeantId; }
inline Int_t StV0Mc::geantIdNegative() const
            { return mNegativeGeantId; }
inline Float_t  StV0Mc::parentMomentumX() const
            { return mParentMomentumX; }
inline Float_t  StV0Mc::parentMomentumY() const
            { return mParentMomentumY; }
inline Float_t  StV0Mc::parentMomentumZ() const
            { return mParentMomentumZ; }
inline Float_t  StV0Mc::positiveMomentumX() const
            { return mPositiveMomentumX; }
inline Float_t  StV0Mc::positiveMomentumY() const
            { return mPositiveMomentumY; }
inline Float_t  StV0Mc::positiveMomentumZ() const
            { return mPositiveMomentumZ; }
inline Float_t  StV0Mc::negativeMomentumX() const
            { return mNegativeMomentumX; }
inline Float_t  StV0Mc::negativeMomentumY() const
            { return mNegativeMomentumY; }
inline Float_t  StV0Mc::negativeMomentumZ() const
            { return mNegativeMomentumZ; }
inline Float_t  StV0Mc::positionX() const
            { return mPositionX; }
inline Float_t  StV0Mc::positionY() const
            { return mPositionY; }
inline Float_t StV0Mc::positionZ() const
            { return mPositionZ; }

#endif
