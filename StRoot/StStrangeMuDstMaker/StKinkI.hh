/***********************************************************************
 *
 * $Id: StKinkI.hh,v 3.3 2002/04/30 16:02:47 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 27-Apr-2001
 *
 ***********************************************************************
 *
 * Description: Kink micro dst object interface class
 *              Used for StKinkMuDst (reconstructed), StKinkMc (Monte Carlo)
 *              and StXiMc (Monte Carlo)
 *
 ***********************************************************************
 *
 * $Log: StKinkI.hh,v $
 * Revision 3.3  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.2  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 *
 ***********************************************************************/
#ifndef  STAR_StKinkI
#define  STAR_StKinkI
#include "TROOT.h"

class StKinkI {
public:
  virtual ~StKinkI() {}

// ************************************************************************
// The following functions are useful in all Kink types
// ************************************************************************

  virtual Int_t   geantIdParent() const=0;
  virtual Int_t   geantIdDaughter() const=0;
  virtual Float_t parentMomentumX() const=0;     // Momenta at the decay vertex
  virtual Float_t parentMomentumY() const=0;
  virtual Float_t parentMomentumZ() const=0;
  virtual Float_t parentPrimMomentumX() const=0; // Momenta at the origin
  virtual Float_t parentPrimMomentumY() const=0;
  virtual Float_t parentPrimMomentumZ() const=0;
  virtual Float_t daughterMomentumX() const=0;
  virtual Float_t daughterMomentumY() const=0;
  virtual Float_t daughterMomentumZ() const=0;
  virtual Float_t positionX() const=0;
  virtual Float_t positionY() const=0;
  virtual Float_t positionZ() const=0;
  virtual Int_t   parentCharge()   const=0;
  virtual Int_t   daughterCharge() const=0;

// ************************************************************************
// The next few functions are presently used only by MC
// ************************************************************************

  virtual Int_t decayMode() const     {return 0;}
  virtual Int_t simTpcHits() const    {return 0;}
  virtual Int_t commonTpcHits() const {return 0;}

// ************************************************************************
// All of the functions from this point on are presently used only by MuDst
// ************************************************************************

  virtual Float_t  dcaParentDaughter() const        {return 0;}
  virtual Float_t  dcaDaughterPrimaryVertex() const {return -1;}
  virtual Float_t  dcaParentPrimaryVertex() const   {return -1;}

  virtual Float_t  hitDistanceParentDaughter() const {return 0;}
  virtual Float_t  hitDistanceParentVertex()   const {return 0;}
  virtual Float_t  mindE()                     const {return 0;}
  virtual Float_t  decayAngle()                const {return 0;}
  virtual Float_t  parentMomentum()            const {return 0;}
  virtual Float_t  daughterMomentum()          const {return 0;}
  virtual Float_t  decayLength()               const {return 0;}
  virtual Float_t  transverseMomentum()        const {return 0;}
  virtual Float_t  transverseMassKaon()        const {return 0;}
  virtual Float_t  transverseMassPion()        const {return 0;}
  virtual Float_t  rapidityKaon()              const {return 0;}
  virtual Float_t  rapidityPion()              const {return 0;}

  // Chi square of Kink 
  virtual Float_t  chi2Kink()        const {return 0;}
  // Confidence level of Kink
  virtual Float_t  clKink()          const {return 0;}
  // Chi square of parent
  virtual Float_t  chi2Parent()      const {return 0;}
  // Confidence level of parent
  virtual Float_t  clParent()        const {return 0;}
  // Chi square of daughter
  virtual Float_t  chi2Daughter()    const {return 0;}
  // Confidence level of daughter
  virtual Float_t  clDaughter()      const {return 0;}
  // dE/dX of parent
  virtual Float_t  dedxParent()      const {return 0;}
  // dE/dX of daughter
  virtual Float_t  dedxDaughter()    const {return 0;}
  // Error on mean of dE/dX of parent
  virtual Float_t  errDedxParent()      const {return 0;}
  // Error on mean of dE/dX of daughter
  virtual Float_t  errDedxDaughter()    const {return 0;}
  // Number of dE/dX points for parent
  virtual UShort_t numDedxParent()   const {return 0;}
  // Number of dE/dX points for daughter
  virtual UShort_t numDedxDaughter() const {return 0;}
  // Length of dE/dX track of parent
  virtual Float_t  lenDedxParent()      const {return 0;}
  // Length of dE/dX track of daughter
  virtual Float_t  lenDedxDaughter()    const {return 0;}
};


#endif
