//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrigger.h,v 1.1 2001/02/12 21:16:20 yepes Exp $
// $Log: StPeCTrigger.h,v $
// Revision 1.1  2001/02/12 21:16:20  yepes
// New version of StPeCMaker, lots of changes
//
//
// Revision 1.0  2000/12/11 Pablo Yepes
// First Version of StPeCTrigger
//
//////////////////////////////////////////////////////////////////////
//
// StPeCTrigger
//
// Event class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCTrigger_h
#define StPeCTrigger_h
#include "Rtypes.h"
#include "TObject.h"
#include "StPeCL0.h"
class StEvent ;

class StPeCTrigger: public TObject {

public:

  StPeCTrigger();
  ~StPeCTrigger();
  Int_t StPeCTrigger::process(StEvent *event);

  Bool_t FalkAkio ;
  Bool_t FalkAkioEW ;
  Bool_t EastAndWest ;
  Bool_t EastOrWest ;
  Bool_t p4_0 ;
  Bool_t p4_1 ;
  Int_t  p4 ; // p4 with swapt weigthts
  Int_t  p4c ;
  Int_t  p5 ;

  Int_t  ctbNE ; // hits in ctb North  East quadrant
  Int_t  ctbSE ; // hits in ctb South  East quadrant 
  Int_t  ctbBE ; // hits in ctb Top    East quadrant
  Int_t  ctbTE ; // hits in ctb Bottom East quadrant 

  Int_t  ctbNW ; // hits in ctb North  West quadrant
  Int_t  ctbSW ; // hits in ctb South  West quadrant 
  Int_t  ctbBW ; // hits in ctb Top    West quadrant
  Int_t  ctbTW ; // hits in ctb Bottom West quadrant 

  Int_t  mwcNE ; // hits in mwc North  East quadrant
  Int_t  mwcSE ; // hits in mwc South  East quadrant 
  Int_t  mwcBE ; // hits in mwc Top    East quadrant
  Int_t  mwcTE ; // hits in mwc Bottom East quadrant 

  Int_t  mwcNW ; // hits in mwc North  West quadrant
  Int_t  mwcSW ; // hits in mwc South  West quadrant 
  Int_t  mwcBW ; // hits in mwc Top    West quadrant
  Int_t  mwcTW ; // hits in mwc Bottom West quadrant 
  
  Int_t  ftpW  ; // hits in West Ftpc
  Int_t  ftpE  ; // hits in East Ftpc

  Int_t     nCtbHits ;
  Int_t     nMwcHits ;
  Float_t   ctbSum ;
  Float_t   mwcSum ;
  Float_t   zdcEast ;
  Float_t   zdcWest ;
  Float_t   zdcSum  ;

#ifndef __CINT__
  void  setInfoLevel ( Int_t in ) { infoLevel = in ; } ; 
  Int_t        infoLevel ;
  StPeCL0      *l0_2000 ;
  StPeCL0      *l0_2000Corrected ;
  StPeCL0      *l0Offline2001 ;
#endif /*__CINT__*/


  ClassDef(StPeCTrigger,1)
};

#endif





