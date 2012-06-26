//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrigger.h,v 1.7 2012/06/26 18:51:11 ramdebbe Exp $
// $Log: StPeCTrigger.h,v $
// Revision 1.7  2012/06/26 18:51:11  ramdebbe
// previous entry did not include actual changes
//
// Revision 1.6  2003/11/25 01:54:38  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.5  2002/12/16 23:04:02  yepes
// Field comes in KGauss and should be passed to routines in Teslas
// problem pointed out by Vladimir
//
// Revision 1.4  2002/03/19 22:23:54  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// Revision 1.3  2001/04/25 18:12:32  perev
// HPcorrs
//
// Revision 1.2  2001/02/21 20:54:25  yepes
// *** empty log message ***
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
#include "TClonesArray.h"
#include "StPeCL0.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
class StEvent ;

class StPeCTrigger: public TObject {

public:

  StPeCTrigger();
  ~StPeCTrigger();
  void  clear ( ) ;
  Int_t process(StEvent *event);
  Int_t process(StMuDst* mudst);

  Int_t  p4 ; // p4 with swapt weigthts
  Int_t  p4c ;
  Int_t  p5 ;

  Int_t runN;

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
  
  // attenuated 
  Float_t   zdcEast ;
  Float_t   zdcWest ;
  Float_t   zdcSum  ;
  // unattenuated 
  Float_t   zdcEastUA;
  Float_t   zdcWestUA;
  Float_t   zdcSumUA  ;
  //trigger word 
  Int_t     tw;

  // BBC 
  Float_t bbcAdcSumEastSm;
  Float_t bbcAdcSumWestSm;
  Float_t bbcAdcSumEastLg;
  Float_t bbcAdcSumWestLg;

  Int_t  bbcNHitEastSm;
  Int_t  bbcNHitWestSm;
  Int_t  bbcNHitEastLg;
  Int_t  bbcNHitWestLg;

  Int_t  bbcTacEast;
  Int_t  bbcTacWest;

  // trigger ids efficiency analysis 
  Int_t trg_3000;   // UPC
  Int_t trg_3001;   // UPC+ZDC
  Int_t trg_2001;   // Minbias
  Int_t trg_2004;   // Minbias



  
#ifndef __CINT__
  void  setInfoLevel ( Int_t in ) { infoLevel = in ; } ; 
  Int_t        infoLevel ;
  StPeCL0      *l0_2000 ;
  StPeCL0      *l0_2000Corrected ;
  StPeCL0      *l0Offline2001 ;
#endif /*__CINT__*/
private:
  TClonesArray  *ctbSlats ;


  ClassDef(StPeCTrigger,1)
};

#endif





