/***********************************************************************
 *
 * $Id: StKinkMuDst.hh,v 1.1 2000/03/30 00:18:08 genevb Exp $
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
 * Revision 1.1  2000/03/30 00:18:08  genevb
 * Introduction of StKinkMuDst
 *
 *
 ***********************************************************************/
#ifndef  STAR_StKinkMuDst
#define  STAR_StKinkMuDst
#include "TObject.h"

class StKinkVertex;

class StKinkMuDst : public TObject {
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
  Float_t  daughterMomentumX() const;
  Float_t  daughterMomentumY() const;
  Float_t  daughterMomentumZ() const;
  Float_t  positionX() const;
  Float_t  positionY() const;
  Float_t  positionZ() const;
  Float_t  decayLength() const;
  Float_t  transverseMomentum() const;
  Float_t  transverseMassKaon() const;
  Float_t  transverseMassPion() const;
  Float_t  rapidityKaon() const;
  Float_t  rapidityPion() const;

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
  Float_t  mDaughterMomentumX;
  Float_t  mDaughterMomentumY;
  Float_t  mDaughterMomentumZ;
  Float_t  mPositionX;
  Float_t  mPositionY;
  Float_t  mPositionZ;
  Float_t  mDecayLength;
  Float_t  mTransverseMomentum;
  Float_t  mTransverseMassKaon;
  Float_t  mTransverseMassPion;
  Float_t  mRapidityKaon;  
  Float_t  mRapidityPion;

private:
  void     findMinDeltaEnergy(StKinkVertex*);
  void     findDecayLength(StKinkVertex*);
  void     findTransverseMomentum();
  void     findTransverseMassKaon();
  void     findTransverseMassPion();
  void     findRapidityKaon();
  void     findRapidityPion();
  ClassDef(StKinkMuDst,1)
};

#endif
