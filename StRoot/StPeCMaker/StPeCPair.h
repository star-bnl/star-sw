//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCPair.h,v 1.2 2000/12/13 00:08:59 akio Exp $
// $Log: StPeCPair.h,v $
// Revision 1.2  2000/12/13 00:08:59  akio
// Added trigger sim and histograms
//
// Revision 1.1  2000/04/21 19:12:25  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCPair
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCPair 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCPair
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCPair_h
#define StPeCPair_h
#include "Rtypes.h"
#include "StPeCEnumerations.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"

class StPeCPair{

public:

                                  StPeCPair();
  virtual                         ~StPeCPair();

#ifndef __CINT__
                                  StPeCPair(StTrack* trk1, StTrack* trk2);
  void                            setTrack1(StTrack* trk);
  void                            setTrack2(StTrack* trk);
  StTrack*                        getTrack1();
  StTrack*                        getTrack2();
  StLorentzVectorF                getPair4Momentum(StPeCParticle pid) const;
#endif /*__CINT__*/
  Int_t                           sumCharge() const;
  Float_t                         sumPt() const;
  Float_t                         sumPz() const;
  Float_t                         mInv(StPeCParticle pid) const;
  Float_t                         openingAngle() const;
  Float_t                         cosThetaStar(StPeCParticle pid) const;

private:

#ifndef __CINT__
  StTrack*                        Track1; //!
  StTrack*                        Track2; //!
#endif /*__CINT__*/

  ClassDef(StPeCPair,1)
};

#endif





