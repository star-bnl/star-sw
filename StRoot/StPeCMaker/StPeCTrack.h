//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrack.h,v 1.1 2001/02/12 21:16:12 yepes Exp $
// $Log: StPeCTrack.h,v $
// Revision 1.1  2001/02/12 21:16:12  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.1  2000/04/21 19:12:25  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCTrack
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCTrack 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCTrack
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCTrack_h
#define StPeCTrack_h
#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StPeCEnumerations.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"

class StPeCTrack : public TObject {

public:

                                  StPeCTrack();
  virtual                         ~StPeCTrack();

  void                            calculatePair4Momentum( ) ;
  Int_t                           fill ( ) ;
#ifndef __CINT__
                                  StPeCTrack ( Int_t _primary, StTrack *trk);
  void                            set        ( Int_t _primary, StTrack* trk);
#endif /*__CINT__*/
  Int_t                           key ;
  Int_t                           charge ;
  Bool_t                          primary ;
  Float_t                         pt ;
  Float_t                         eta ;
  Float_t                         psi ; 
  Float_t                         phi0 ;
  Float_t                         r0 ;
  Float_t                         z0 ;
  Float_t                         dedx ;
  Float_t                         nHits ;

  ClassDef(StPeCTrack,1)
};

#endif





