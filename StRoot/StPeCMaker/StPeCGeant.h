//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/18                      
// First Version of StPeCGeant 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCGeant
//
// Geant class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCGeant_h
#define StPeCGeant_h
#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */
#include "StPeCParticle.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

class St_g2t_track;

class StPeCGeant : public TObject {

public:

                         StPeCGeant();
  virtual                ~StPeCGeant();

  void                   clear (  ) ; 
#ifndef __CINT__
  Int_t                  fill  ( TDataSet* geant ) ; 
  Int_t                  fill  ( StMuDst * mu ) ; 
#endif /* __CINT__ */

  Float_t                gPt ;
  Float_t                gPz ;
  Float_t                gEta ;
  Float_t                gMass ;
  Float_t                gY ;
  Float_t                gPsi ;
  Float_t                gZVertex ;

  Int_t                  nPart ;
  TClonesArray          *pPart ;
  
  ClassDef(StPeCGeant,1)
};

#endif





