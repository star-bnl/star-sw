#ifndef __StEEmcTimingMaker_h__
#define __StEEmcTimingMaker_h__

#include "StMaker.h"
#include <map>
#include "TString.h"

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include <vector>

class TH1F;
class TTree;

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

  /// Add a tower mask.  Count crates from 1, channels from 0
  void addTowerMask( Int_t cr, Int_t ch ){ mTowerMask[ cr-1 ][ ch ] = 1; }

  /// Add a mapmt mask.  Count crates from MinMapmtCrateID]
  void addMapmtMask( Int_t cr, Int_t ch ){ mMapmtMask[ cr-MinMapmtCrateID ][ ch ] = 1; }

  /// Do not fill histograms with ADC=0
  void supressZeroAdc();

  void dumpAsciiFile(const Char_t *fname="eemcTimingFile.dat" );
  void dumpPDF( const Char_t *fname="eemcTimingFile.pdf" );

  void setOutputFile( const Char_t *fname ){ mOutputFile=fname; }


  /// Process ready-made histograms from level 2
  void processFromL2( const Char_t *name, int nevents );



 private:
 protected:
  
  Bool_t mSupressZero;
  TString mOutputFile;
  TTree *mTree;

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

  Int_t mTowerMask[ MaxTwCrates ][ MaxTwCrateCh ];
  Int_t mMapmtMask[ MaxMapmtCrates ][ MaxMapmtCrateCh ];


  TH1F *hCounter;

  ClassDef(StEEmcTimingMaker,1);

};

#endif
