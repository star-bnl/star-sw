/*!
 * \class StKinkBase
 * \author Gene Van Buren, BNL, 27-Apr-2001
 *
 *              Kink micro dst object base class
 *              Used for StKinkMuDst (reconstructed), StKinkMc (Monte Carlo),
 *              and StXiMc (Monte Carlo)
 *
 */

#ifndef  STAR_StKinkBase
#define  STAR_StKinkBase
#include "StKinkI.hh"
#include "StStrangeMuDst.hh"

class StKinkBase : public StStrangeMuDst , public StKinkI {
public:
  StKinkBase() {}
  virtual ~StKinkBase() {}

  Int_t    geantIdParent() const;
  Int_t    geantIdDaughter() const;
  Float_t  parentMomentumX() const;
  Float_t  parentMomentumY() const;
  Float_t  parentMomentumZ() const;
  Float_t  parentPrimMomentumZ() const;
  Float_t  parentPrimMomentumX() const;
  Float_t  parentPrimMomentumY() const;
  Float_t  daughterMomentumX() const;
  Float_t  daughterMomentumY() const;
  Float_t  daughterMomentumZ() const;
  Float_t  positionX() const;
  Float_t  positionY() const;
  Float_t  positionZ() const;

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
ClassDef(StKinkBase,6)
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


/***********************************************************************
 * $Id: StKinkBase.hh,v 3.4 2003/10/20 17:20:18 perev Exp $
 * $Log: StKinkBase.hh,v $
 * Revision 3.4  2003/10/20 17:20:18  perev
 * Change the order of inheritance and increased version numbers
 *
 * Revision 3.3  2003/05/30 21:20:18  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 ***********************************************************************/
