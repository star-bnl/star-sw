/*!
 * \class StV0Mc
 * \author Gene Van Buren
 *
 *               Monte Carlo V0 micro dst class 
 *
 */

#ifndef  STAR_StV0Mc
#define  STAR_StV0Mc
#include "StV0I.hh"
#include "StStrangeMuDst.hh"

class StMcVertex;
class StMcTrack;

class StV0Mc : public StStrangeMuDst, public virtual StV0I {

public:
  StV0Mc();
  StV0Mc(StMcVertex*, StMcTrack*, StMcTrack*, StStrangeEvMuDst*);
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

  Int_t positiveSimTpcHits() const;
  Int_t positiveCommonTpcHits() const;
  Int_t negativeSimTpcHits() const;
  Int_t negativeCommonTpcHits() const;

  void SetHitInfoPositive(Int_t commonHits);
  void SetHitInfoNegative(Int_t commonHits);
  
  // Coordinates of decay vertex
  Float_t decayVertexV0X() const {return positionX();}
  Float_t decayVertexV0Y() const {return positionY();}
  Float_t decayVertexV0Z() const {return positionZ();}
  // Momentum components of pos. daughter
  Float_t momPosX() const {return positiveMomentumX();}
  Float_t momPosY() const {return positiveMomentumY();}
  Float_t momPosZ() const {return positiveMomentumZ();}
  // Momentum components of neg. daughter
  Float_t momNegX() const {return negativeMomentumX();}
  Float_t momNegY() const {return negativeMomentumY();}
  Float_t momNegZ() const {return negativeMomentumZ();}
  // Momentum components of V0
  Float_t momV0X() const {return parentMomentumX();}
  Float_t momV0Y() const {return parentMomentumY();}
  Float_t momV0Z() const {return parentMomentumZ();}

protected:
//  StStrangeEvMuDst *mEvent;       //!

  Int_t mPositiveSimTpcHits;
  Int_t mPositiveCommonTpcHits;
  
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
  ClassDef(StV0Mc,5)
};

inline Int_t StV0Mc::decayMode() const
            { return mDecayMode; }
inline void StV0Mc::SetHitInfoPositive(Int_t commonHits)
            { mPositiveCommonTpcHits = commonHits; }
inline Int_t StV0Mc::positiveCommonTpcHits() const
            { return mPositiveCommonTpcHits; }
inline Int_t StV0Mc::positiveSimTpcHits() const
            { return mPositiveSimTpcHits; }
inline void StV0Mc::SetHitInfoNegative(Int_t commonHits)
            { mNegativeCommonTpcHits = commonHits; }
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


/***********************************************************************
 * $Id: StV0Mc.hh,v 3.3 2003/10/20 17:20:18 perev Exp $
 * $Log: StV0Mc.hh,v $
 * Revision 3.3  2003/10/20 17:20:18  perev
 * Change the order of inheritance and increased version numbers
 *
 * Revision 3.2  2003/05/30 21:20:20  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:45  genevb
 * New version of Strangeness micro DST package
 *
 ***********************************************************************/
