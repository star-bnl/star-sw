/***********************************************************************
 *
 * $Id: StKinkBase.hh,v 3.2 2002/04/30 16:02:47 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 27-Apr-2001
 *
 ***********************************************************************
 *
 * Description: Kink micro dst object base class
 *              Used for StKinkMuDst (reconstructed), StKinkMc (Monte Carlo),
 *              and StKinkMc (Monte Carlo)
 *
 ***********************************************************************
 *
 * $Log: StKinkBase.hh,v $
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 *
 ***********************************************************************/
#ifndef  STAR_StKinkBase
#define  STAR_StKinkBase
#include "StKinkI.hh"
#include "StStrangeMuDst.hh"

class StKinkBase : public virtual StKinkI , public StStrangeMuDst {
public:
  StKinkBase() {}
  virtual ~StKinkBase() {}

  virtual Int_t    geantIdParent() const;
  virtual Int_t    geantIdDaughter() const;
  virtual Float_t  parentMomentumX() const;
  virtual Float_t  parentMomentumY() const;
  virtual Float_t  parentMomentumZ() const;
  virtual Float_t  parentPrimMomentumZ() const;
  virtual Float_t  parentPrimMomentumX() const;
  virtual Float_t  parentPrimMomentumY() const;
  virtual Float_t  daughterMomentumX() const;
  virtual Float_t  daughterMomentumY() const;
  virtual Float_t  daughterMomentumZ() const;
  virtual Float_t  positionX() const;
  virtual Float_t  positionY() const;
  virtual Float_t  positionZ() const;

protected:
  Int_t    mParentGeantId;
  Int_t    mDaughterGeantId;
  Float_t  mParentMomentumX;
  Float_t  mParentMomentumY;
  Float_t  mParentMomentumZ;
  Float_t  mParentPrimMomentumX;
  Float_t  mParentPrimMomentumY;
  Float_t  mParentPrimMomentumZ;
  Float_t  mDaughterMomentumX;
  Float_t  mDaughterMomentumY;
  Float_t  mDaughterMomentumZ;
  Float_t  mPositionX;
  Float_t  mPositionY;
  Float_t  mPositionZ;

// Corresponding ClassImp presently in StKinkMuDst.cc
ClassDef(StKinkBase,5)
};

inline Int_t StKinkBase::geantIdParent() const
             { return mParentGeantId; }
inline Int_t StKinkBase::geantIdDaughter() const
             { return mDaughterGeantId; }
inline Float_t StKinkBase::parentMomentumX() const
             { return mParentMomentumX; }
inline Float_t StKinkBase::parentMomentumY() const
             { return mParentMomentumY; }
inline Float_t StKinkBase::parentMomentumZ() const
             { return mParentMomentumZ; }
inline Float_t StKinkBase::parentPrimMomentumX() const
             { return mParentPrimMomentumX; }
inline Float_t StKinkBase::parentPrimMomentumY() const
             { return mParentPrimMomentumY; }
inline Float_t StKinkBase::parentPrimMomentumZ() const
             { return mParentPrimMomentumZ; }
inline Float_t StKinkBase::daughterMomentumX() const
             { return mDaughterMomentumX; }
inline Float_t StKinkBase::daughterMomentumY() const
             { return mDaughterMomentumY; }
inline Float_t StKinkBase::daughterMomentumZ() const
             { return mDaughterMomentumZ; }
inline Float_t StKinkBase::positionX() const
             { return mPositionX; }
inline Float_t StKinkBase::positionY() const
             { return mPositionY; }
inline Float_t StKinkBase::positionZ() const
             { return mPositionZ; }

#endif
