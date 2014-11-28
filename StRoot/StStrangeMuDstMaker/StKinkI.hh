/*!
 * \class StKinkI
 * \author Gene Van Buren, BNL, 27-Apr-2001
 *
 *               Kink micro dst object interface class
 *               Used for StKinkMuDst (reconstructed), StKinkMc (Monte Carlo)
 *               and StXiMc (Monte Carlo)
 *
 */

#ifndef  STAR_StKinkI
#define  STAR_StKinkI
#include "TROOT.h"
#include "TMath.h"

class StKinkI {
public:
  virtual ~StKinkI() {}

// ************************************************************************
/// @name _________________FUNCTIONS_USEFUL_IN_ALL_KINK_TYPES_________________
// ************************************************************************
  
  //@{
  virtual Int_t   geantIdParent() const=0;
  virtual Int_t   geantIdDaughter() const=0;
  virtual Float_t parentMomentumX() const=0;     // Momenta at the decay vertex
  virtual Float_t parentMomentumY() const=0;
  virtual Float_t parentMomentumZ() const=0;
  virtual Float_t parentPrimMomentumX() const=0; // Momenta at the origin
  virtual Float_t parentPrimMomentumY() const=0;
  virtual Float_t parentPrimMomentumZ() const=0;
  virtual Float_t parentPrimPsi() const;
  virtual Float_t daughterMomentumX() const=0;
  virtual Float_t daughterMomentumY() const=0;
  virtual Float_t daughterMomentumZ() const=0;
  virtual Float_t positionX() const=0;
  virtual Float_t positionY() const=0;
  virtual Float_t positionZ() const=0;
  virtual Int_t   parentCharge()   const=0;
  virtual Int_t   daughterCharge() const=0;
  //@}

// ************************************************************************
/// @name _______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MC_______________
// ************************************************************************

  //@{
  /// Returns the decay process for the vertex,
  /// where the deacy modes are enumerated in ::decayModeType
  virtual Int_t decayMode() const     {return 0;}
  virtual Int_t simTpcHits() const    {return 0;}
  virtual Int_t commonTpcHits() const {return 0;}
  //@}

// ************************************************************************
/// @name ______________FUNCTIONS_PRESENTLY_USED_ONLY_BY_MuDst______________
// ************************************************************************

  //@{
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

  /// dE/dX of parent
  virtual Float_t  dedxParent()      const {return 0;}
  /// dE/dX of daughter
  virtual Float_t  dedxDaughter()    const {return 0;}
  /// Error on mean of dE/dX of parent
  virtual Float_t  errDedxParent()      const {return 0;}
  /// Error on mean of dE/dX of daughter
  virtual Float_t  errDedxDaughter()    const {return 0;}
  /// Number of dE/dX points for parent
  virtual UShort_t numDedxParent()   const {return 0;}
  /// Number of dE/dX points for daughter
  virtual UShort_t numDedxDaughter() const {return 0;}
  /// Length of dE/dX track of parent
  virtual Float_t  lenDedxParent()      const {return 0;}
  /// Length of dE/dX track of daughter
  virtual Float_t  lenDedxDaughter()    const {return 0;}
  /// Track id of parent
  virtual Int_t keyParent()     const {return 0;}
  /// Track id of daughter
  virtual Int_t keyDaughter()   const {return 0;}
  //@}

  /// @name DCAs
  //@{
  virtual Float_t  dcaParentDaughter() const        {return 0;}
  virtual Float_t  dcaDaughterPrimaryVertex() const {return -1;}
  virtual Float_t  dcaParentPrimaryVertex() const   {return -1;}
  //@}

  /// @name Fit/Finding properties
  //@{
  /// Chi square of Kink 
  virtual Float_t  chi2Kink()        const {return 0;}
  /// Confidence level of Kink
  virtual Float_t  clKink()          const {return 0;}
  /// Chi square of parent
  virtual Float_t  chi2Parent()      const {return 0;}
  /// Confidence level of parent
  virtual Float_t  clParent()        const {return 0;}
  /// Chi square of daughter
  virtual Float_t  chi2Daughter()    const {return 0;}
  /// Confidence level of daughter
  virtual Float_t  clDaughter()      const {return 0;}
  /// Set the parent as bad
  virtual void setParentBad() {}
  /// Set the daughter as bad
  virtual void setDaughterBad() {}
  /// Test whether either daughter is bad
  virtual Bool_t bad() const {return (chi2Parent()<0 || chi2Daughter()<0);}
  //@}
};

inline Float_t StKinkI::parentPrimPsi() const
             { return TMath::ATan2(parentPrimMomentumY(),
	                           parentPrimMomentumX()); }

#endif


/***********************************************************************
 * $Id: StKinkI.hh,v 3.7 2011/05/27 18:25:32 genevb Exp $
 * $Log: StKinkI.hh,v $
 * Revision 3.7  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 3.6  2008/07/10 16:16:54  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.5  2004/02/03 03:49:27  genevb
 * Added keys (IDs) for Kink parent and daughter
 *
 * Revision 3.4  2003/05/30 21:20:18  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.3  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.2  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 ***********************************************************************/
