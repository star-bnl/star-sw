#ifndef __StEEmcTimingMaker_h__
#define __StEEmcTimingMaker_h__

#include "StMaker.h"
#include <map>

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class TH1F;

class StEEmcTimingMaker : public StMaker
{

 public:
  StEEmcTimingMaker(const Char_t *name="timing");
  ~StEEmcTimingMaker(){ /* nada */ };

  Int_t Init();
  Int_t InitRun(Int_t run);

  Int_t Make();
  void  Clear(Option_t *opts="");

  Int_t Finish();

  void setTiming( Float_t tower_delay, Float_t mapmt_delay );
  void setRunNumber( Int_t run );

  void setTowerCuts( Int_t min, Int_t max );
  void setMapmtCuts( Int_t min, Int_t max );

  /// Do not fill histograms with ADC=0
  void supressZeroAdc();

  void dumpAsciiFile(const Char_t *fname="eemcTimingFile.dat" );
  void dumpPDF( const Char_t *fname="eemcTimingFile.pdf" );

 private:
 protected:
  
  Bool_t mSupressZero;

  Int_t   mRunNumber;
  Float_t mTowerDelay;
  Float_t mMapmtDelay;

  Int_t   mTotalYield;
  Int_t   mTowerCrateYield[ MaxTwCrates ];
  Int_t   mMapmtCrateYield[ MaxMapmtCrates ];

  Int_t   mTowerChanYield[ MaxTwCrates ][ MaxTwCrateCh ];
  Int_t   mMapmtChanYield[ MaxMapmtCrates ][ MaxMapmtCrateCh ];

  Float_t mTowerChanSlope[ MaxTwCrates ][ MaxTwCrateCh ];
  Float_t mMapmtChanSlope[ MaxMapmtCrates ][ MaxMapmtCrateCh ];

  Int_t   mTowerMin;
  Int_t   mTowerMax;
  Int_t   mMapmtMin;
  Int_t   mMapmtMax;

  TH1F *hTower[ MaxTwCrates ][ MaxTwCrateCh ];
  TH1F *hMapmt[ MaxMapmtCrates ][ MaxMapmtCrateCh ];

  TH1F *hCounter;

  ClassDef(StEEmcTimingMaker,1);

};

#endif
