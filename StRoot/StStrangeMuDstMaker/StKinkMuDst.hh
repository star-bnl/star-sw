/***********************************************************************
 *
 * $Id: StKinkMuDst.hh,v 3.2 2000/09/06 21:09:03 wdeng Exp $
 *
 * Author: Wensheng Deng, Kent State University, 29-Mar-2000
 *
 ***********************************************************************
 *
 * Description: Kink micro dst class
 *
 ***********************************************************************
 *
 * $Log: StKinkMuDst.hh,v $
 * Revision 3.2  2000/09/06 21:09:03  wdeng
 * Added track charges and total momenta
 *
 * Revision 3.1  2000/08/10 01:16:24  genevb
 * Added number of dedx points
 *
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:10  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.1  2000/03/30 00:18:08  genevb
 * Introduction of StKinkMuDst
 *
 *
 ***********************************************************************/
#ifndef  STAR_StKinkMuDst
#define  STAR_StKinkMuDst
#include "StStrangeMuDst.hh"

class StKinkVertex;

class StKinkMuDst : public StStrangeMuDst {
public:
  StKinkMuDst();
  StKinkMuDst(StKinkVertex*);
  ~StKinkMuDst();

  UShort_t geantIdParent() const;
  UShort_t geantIdDaughter() const;
  Float_t  dcaParentDaughter() const;
  Float_t  dcaDaughterPrimaryVertex() const;
  Float_t  dcaParentPrimaryVertex() const;
  Float_t  hitDistanceParentDaughter() const;
  Float_t  hitDistanceParentVertex() const;
  Float_t  mindE() const;
  Float_t  decayAngle() const;
  Float_t  parentMomentumX() const;
  Float_t  parentMomentumY() const;
  Float_t  parentMomentumZ() const;
  Float_t  parentMomentum() const;
  Short_t  parentCharge() const;
  Float_t  daughterMomentumX() const;
  Float_t  daughterMomentumY() const;
  Float_t  daughterMomentumZ() const;
  Float_t  daughterMomentum() const;
  Short_t  daughterCharge() const;
  Float_t  positionX() const;
  Float_t  positionY() const;
  Float_t  positionZ() const;
  Float_t  decayLength() const;
  Float_t  transverseMomentum() const;
  Float_t  transverseMassKaon() const;
  Float_t  transverseMassPion() const;
  Float_t  rapidityKaon() const;
  Float_t  rapidityPion() const;
  Float_t  chi2Kink()     const;       // Chi square of Kink
  Float_t  clKink()       const;       // Confidence level of Kink
  Float_t  chi2Parent()   const;       // Chi square of parent
  Float_t  clParent()     const;       // Confidence level of parent
  Float_t  chi2Daughter() const;       // Chi square of daughter
  Float_t  clDaughter()   const;       // Confidence level of daughter
  Float_t  dedxParent()   const;       // dE/dX of parent
  Float_t  dedxDaughter() const;       // dE/dX of daughter
  UShort_t numDedxParent()   const;    // Number of dE/dX points for parent
  UShort_t numDedxDaughter() const;    // Number of dE/dX points for daughter

  UShort_t mParentGeantId;
  UShort_t mDaughterGeantId;
  Float_t  mDcaParentDaughter;
  Float_t  mDcaDaughterPrimaryVertex;
  Float_t  mDcaParentPrimaryVertex;
  Float_t  mHitDistanceParentDaughter;
  Float_t  mHitDistanceParentVertex;
  Float_t  mMinDeltaEnergy;
  Float_t  mDecayAngle;
  Float_t  mParentMomentumX;
  Float_t  mParentMomentumY;
  Float_t  mParentMomentumZ;
  Float_t  mParentMomentum;
  Short_t  mParentCharge;
  Float_t  mDaughterMomentumX;
  Float_t  mDaughterMomentumY;
  Float_t  mDaughterMomentumZ;
  Float_t  mDaughterMomentum;
  Short_t  mDaughterCharge;
  Float_t  mPositionX;
  Float_t  mPositionY;
  Float_t  mPositionZ;
  Float_t  mDecayLength;
  Float_t  mTransverseMomentum;
  Float_t  mTransverseMassKaon;
  Float_t  mTransverseMassPion;
  Float_t  mRapidityKaon;  
  Float_t  mRapidityPion;
  Float_t  mChi2Kink;
  Float_t  mClKink;
  Float_t  mChi2Parent;
  Float_t  mClParent;
  Float_t  mChi2Daughter;
  Float_t  mClDaughter;
  Float_t  mDedxParent;
  Float_t  mDedxDaughter;
  UShort_t mNumDedxParent;
  UShort_t mNumDedxDaughter;

private:
  void     findMinDeltaEnergy(StKinkVertex*);
  void     findDecayLength(StKinkVertex*);
  void     findTransverseMomentum();
  void     findTransverseMassKaon();
  void     findTransverseMassPion();
  void     findRapidityKaon();
  void     findRapidityPion();
  ClassDef(StKinkMuDst,3)
};

inline UShort_t StKinkMuDst::geantIdParent() const
               { return mParentGeantId; }
inline UShort_t StKinkMuDst::geantIdDaughter() const
               { return mDaughterGeantId; }
inline Float_t StKinkMuDst::dcaParentDaughter() const
               { return mDcaParentDaughter; }
inline Float_t StKinkMuDst::dcaDaughterPrimaryVertex() const
               { return mDcaDaughterPrimaryVertex; }
inline Float_t StKinkMuDst::dcaParentPrimaryVertex() const
               { return mDcaParentPrimaryVertex; }
inline Float_t StKinkMuDst::hitDistanceParentDaughter() const
               { return mHitDistanceParentDaughter; }
inline Float_t StKinkMuDst::hitDistanceParentVertex() const 
               { return mHitDistanceParentVertex; }
inline Float_t StKinkMuDst::mindE() const { return mMinDeltaEnergy; }
inline Float_t StKinkMuDst::decayAngle() const { return mDecayAngle; }
inline Float_t StKinkMuDst::parentMomentumX() const
               { return mParentMomentumX; }
inline Float_t StKinkMuDst::parentMomentumY() const
               { return mParentMomentumY; }
inline Float_t StKinkMuDst::parentMomentumZ() const
               { return mParentMomentumZ; }
inline Float_t StKinkMuDst::parentMomentum() const
               { return mParentMomentum; }
inline Short_t StKinkMuDst::parentCharge() const
               { return mParentCharge; }
inline Float_t StKinkMuDst::daughterMomentumX() const
               { return mDaughterMomentumX; }
inline Float_t StKinkMuDst::daughterMomentumY() const
               { return mDaughterMomentumY; }
inline Float_t StKinkMuDst::daughterMomentumZ() const
               { return mDaughterMomentumZ; }
inline Float_t StKinkMuDst::daughterMomentum() const
               { return mDaughterMomentum; }
inline Short_t StKinkMuDst::daughterCharge() const
               { return mDaughterCharge; }
inline Float_t StKinkMuDst::positionX() const { return mPositionX; }
inline Float_t StKinkMuDst::positionY() const { return mPositionY; }
inline Float_t StKinkMuDst::positionZ() const { return mPositionZ; }
inline Float_t StKinkMuDst::decayLength() const { return mDecayLength; } 
inline Float_t StKinkMuDst::transverseMomentum() const
               { return mTransverseMomentum; }
inline Float_t StKinkMuDst::transverseMassKaon() const
               { return mTransverseMassKaon; }
inline Float_t StKinkMuDst::transverseMassPion() const
               { return mTransverseMassPion; }
inline Float_t StKinkMuDst::rapidityKaon() const { return mRapidityKaon; }
inline Float_t StKinkMuDst::rapidityPion() const { return mRapidityPion; }
inline Float_t StKinkMuDst::chi2Kink()     const { return mChi2Kink; }
inline Float_t StKinkMuDst::clKink()       const { return mClKink; }
inline Float_t StKinkMuDst::chi2Parent()   const { return mChi2Parent; }
inline Float_t StKinkMuDst::clParent()     const { return mClParent; }
inline Float_t StKinkMuDst::chi2Daughter() const { return mChi2Daughter; }
inline Float_t StKinkMuDst::clDaughter()   const { return mClDaughter; }
inline Float_t StKinkMuDst::dedxParent()   const { return mDedxParent; }
inline Float_t StKinkMuDst::dedxDaughter() const { return mDedxDaughter; }
inline UShort_t StKinkMuDst::numDedxParent()   const { return mNumDedxParent; }
inline UShort_t StKinkMuDst::numDedxDaughter() const { return mNumDedxDaughter; }
#endif
