//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000      Meissner
// First Version of StPeCLumiEntry 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCLumiEntry
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCLumiEntry_h
#define StPeCLumiEntry_h
#include "Rtypes.h"
#include "TObject.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#include "StEvent.h"
#endif /* __CINT__ */
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"


class StPeCLumiEntry : public TObject {

public:

  
                                  StPeCLumiEntry();
  virtual                         ~StPeCLumiEntry();
  void  Clear(const char *opt="");
  

  Int_t runNr;    // run nr. 
  Int_t eventNr ; // event number 
  Int_t uTime;    // Unix  time 
  Int_t  triggerMask;  
  Int_t  triggerWord;  
  Int_t  triggerActionWord;  
  Float_t zdcEast; 
  Float_t zdcWest; 
  Float_t zdcSum; 
  // unattenuated signals 
  Float_t zdcEastUA; 
  Float_t zdcWestUA; 
  Float_t zdcSumUA; 

  Float_t ctbSum;  
  Int_t   ctbSumMips;  
  Int_t nPrimary;  
  Int_t nGlobal; 
  Int_t nhminus; 
  Float_t xVtx;  
  Float_t yVtx; 
  Float_t zVtx; 


#ifndef __CINT__
  Int_t     fill ( StEvent* event ); 
  Int_t     fill ( StMuDst* muDst );
#endif /*__CINT__*/
  

  // Last Item before closing bracket
  ClassDef(StPeCLumiEntry,1)
};

#endif





