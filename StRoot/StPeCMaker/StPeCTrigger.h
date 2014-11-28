//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrigger.h,v 1.12 2014/04/25 20:00:13 ramdebbe Exp $
// $Log: StPeCTrigger.h,v $
// Revision 1.12  2014/04/25 20:00:13  ramdebbe
// added more triggers for run14
//
// Revision 1.11  2013/12/27 20:47:32  ramdebbe
// added a set method to select a trigger
//
// Revision 1.10  2013/10/28 14:18:24  ramdebbe
// added arrays to handle bbc and zdc information
//
// Revision 1.9  2013/01/24 15:45:04  ramdebbe
// added ZDC shower max information to output tree and bbc small tubes individual ADC. Returns UPC_Main trigger
//
// Revision 1.8  2012/07/03 19:37:55  ramdebbe
// raised ClassDef from 1 to 2
//
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
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEvent/StTriggerData.h"

class StEvent ;

class StPeCTrigger: public TObject {

public:

  StPeCTrigger();
  ~StPeCTrigger();
  void  clear ( ) ;
  Int_t process(StEvent *event, string triggerSel);
  Int_t process(StMuDst* mudst, string triggerSel);

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
  Int_t     nBTOFhits;
  Int_t     nBtofTriggerHits;
  Int_t     nPrimaryTracks;
  unsigned int bunchId;
  unsigned short lastDSM0;
  unsigned short lastDSM1;
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
  //zdc TDC information
  Float_t   zdcEastTDC ;
  Float_t   zdcWestTDC ;
  Float_t   zdcTimeDifference  ;
  //SMD information
  bool zdcSMDPresent;

  unsigned short zdcSMDEastH[8];


  unsigned short zdcSMDEastV[8];


  unsigned short zdcSMDWestH[8];


  unsigned short zdcSMDWestV[8];

  unsigned short zdcSMDHighestStripEastH;
  unsigned short zdcSMDHighestStripEastV;
  unsigned short zdcSMDHighestStripWestH;
  unsigned short zdcSMDHighestStripWestV;

  Float_t zdcCoincidenceRate;

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

  unsigned short bbcTacEast;
  unsigned short bbcTacWest;
  unsigned short bbcTimeDiff;

  unsigned short bbcTDCEast[36];
  unsigned short bbcTDCWest[36];
  unsigned short bbcADCEast[36];
  unsigned short bbcADCWest[36];




  // trigger ids efficiency analysis 
  Int_t trg_3000;   // UPC or ZDC_Mon
  Int_t trg_3001;   // UPC or ZDC_Mon
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


  ClassDef(StPeCTrigger,2)
};

#endif





