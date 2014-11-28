//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCSpec.h,v 1.2 2001/02/21 20:54:24 yepes Exp $
// $Log: StPeCSpec.h,v $
// Revision 1.2  2001/02/21 20:54:24  yepes
// *** empty log message ***
//
// Revision 1.1  2000/12/13 yepes
//
//////////////////////////////////////////////////////////////////////
//
// StPeCSpec
//
// Mass Hypothesis for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCSpec_h
#define StPeCSpec_h
#include "Rtypes.h"
#include "TObject.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */

class StPeCSpec : public TObject {

public:
                             StPeCSpec();
  virtual                    ~StPeCSpec();

  Int_t                      pid  ;
  Float_t                    mInv ;
  Float_t                    cosThetaStar ;
  Float_t                    yRap ;
#ifndef __CINT__
  StLorentzVectorF           Mom4;
#endif /*__CINT__*/

  ClassDef(StPeCSpec,1)
};

#endif





