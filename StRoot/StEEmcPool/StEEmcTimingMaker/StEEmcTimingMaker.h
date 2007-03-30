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

 private:
 protected:
  
  Int_t   mRunNumber;
  Float_t mTowerDelay;
  Float_t mMapmtDelay;

  Int_t   mTotalYield;
  Int_t   mTowerCrateYield[ MaxTwCrates ];
  Int_t   mMapmtCrateYield[ MaxMapmtCrates ];

  Int_t   mTowerChanYield[ MaxTwCrates ][ MaxTwCrateCh ];
  Int_t   mMapmtChanYield[ MaxMapmtCrates ][ MaxMapmtCrateCh ];

  Int_t   mTowerMin;
  Int_t   mTowerMax;
  Int_t   mMapmtMin;
  Int_t   mMapmtMax;

  TH1F *hTower[ MaxTwCrates ][ MaxTwCrateCh ];
  TH1F *hMapmt[ MaxMapmtCrates ][ MaxMapmtCrateCh ];

  ClassDef(StEEmcTimingMaker,1);

};

#endif
