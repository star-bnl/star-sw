//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCTrigger.h,v 1.9 2013/01/24 15:45:04 ramdebbe Exp $
// $Log: StPeCTrigger.h,v $
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
  //zdc TDC information
  Float_t   zdcEastTDC ;
  Float_t   zdcWestTDC ;
  Float_t   zdcTimeDifference  ;
  //SMD information
  bool zdcSMDPresent;
  unsigned short zdcSMDEastH0;
  unsigned short zdcSMDEastH1;
  unsigned short zdcSMDEastH2;
  unsigned short zdcSMDEastH3;
  unsigned short zdcSMDEastH4;
  unsigned short zdcSMDEastH5;
  unsigned short zdcSMDEastH6;
  unsigned short zdcSMDEastH7;

  unsigned short zdcSMDEastV0;
  unsigned short zdcSMDEastV1;
  unsigned short zdcSMDEastV2;
  unsigned short zdcSMDEastV3;
  unsigned short zdcSMDEastV4;
  unsigned short zdcSMDEastV5;
  unsigned short zdcSMDEastV6;
  unsigned short zdcSMDEastV7;

  unsigned short zdcSMDWestH0;
  unsigned short zdcSMDWestH1;
  unsigned short zdcSMDWestH2;
  unsigned short zdcSMDWestH3;
  unsigned short zdcSMDWestH4;
  unsigned short zdcSMDWestH5;
  unsigned short zdcSMDWestH6;
  unsigned short zdcSMDWestH7;

  unsigned short zdcSMDWestV0;
  unsigned short zdcSMDWestV1;
  unsigned short zdcSMDWestV2;
  unsigned short zdcSMDWestV3;
  unsigned short zdcSMDWestV4;
  unsigned short zdcSMDWestV5;
  unsigned short zdcSMDWestV6;
  unsigned short zdcSMDWestV7;

  unsigned short zdcSMDHighestStripEastH;
  unsigned short zdcSMDHighestStripEastV;
  unsigned short zdcSMDHighestStripWestH;
  unsigned short zdcSMDHighestStripWestV;


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

  unsigned short bbcADCEastSmall_1;
  unsigned short bbcADCEastSmall_2;
  unsigned short bbcADCEastSmall_3;
  unsigned short bbcADCEastSmall_4;
  unsigned short bbcADCEastSmall_5;
  unsigned short bbcADCEastSmall_6;
  unsigned short bbcADCEastSmall_7;
  unsigned short bbcADCEastSmall_8;
  unsigned short bbcADCEastSmall_9;
  unsigned short bbcADCEastSmall_10;
  unsigned short bbcADCEastSmall_11;
  unsigned short bbcADCEastSmall_12;
  unsigned short bbcADCEastSmall_13;
  unsigned short bbcADCEastSmall_14;
  unsigned short bbcADCEastSmall_15;
  unsigned short bbcADCEastSmall_16;
  unsigned short bbcADCEastSmall_17;
  unsigned short bbcADCEastSmall_18;

  unsigned short bbcADCWestSmall_1;
  unsigned short bbcADCWestSmall_2;
  unsigned short bbcADCWestSmall_3;
  unsigned short bbcADCWestSmall_4;
  unsigned short bbcADCWestSmall_5;
  unsigned short bbcADCWestSmall_6;
  unsigned short bbcADCWestSmall_7;
  unsigned short bbcADCWestSmall_8;
  unsigned short bbcADCWestSmall_9;
  unsigned short bbcADCWestSmall_10;
  unsigned short bbcADCWestSmall_11;
  unsigned short bbcADCWestSmall_12;
  unsigned short bbcADCWestSmall_13;
  unsigned short bbcADCWestSmall_14;
  unsigned short bbcADCWestSmall_15;
  unsigned short bbcADCWestSmall_16;
  unsigned short bbcADCWestSmall_17;
  unsigned short bbcADCWestSmall_18;

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


  ClassDef(StPeCTrigger,2)
};

#endif





