#ifndef STGMTDATA_H
#define STGMTDATA_H
#include "TObject.h"
#include <string.h>
const Int_t kMaxHits = 20000;
const Int_t kMaxCluster = 16;
const Int_t kMaxNgbhrChan = 5;
const Int_t kMaxTracks = 10000;

class StGmtData : public TObject {
 public:
  StGmtData() {memset(mBeg,0,mEnd-mBeg+1);}
  virtual ~StGmtData() {}
  Char_t mBeg[1]; // !
  Int_t  run, evt;
  Float_t  bField;
  Float_t vertexX, vertexY, vertexZ;
  Float_t  vpdVz;
  
  //raw hits
  Int_t  nGmtRawHits;
  Char_t arm[kMaxHits];
  Char_t apv[kMaxHits];
  Char_t layer[kMaxHits];
  Char_t tb[kMaxHits];
  Short_t channel[kMaxHits];
  Short_t coordNum[kMaxHits];
  Short_t adc[kMaxHits];
  Short_t pos[kMaxHits];
  Float_t ped[kMaxHits];
  Float_t pedDev[kMaxHits];
  
  //hits
  Int_t  nGmtCluster;
  Float_t  coordNX[kMaxCluster];
  Float_t  coordNY[kMaxCluster];
  Float_t  coordX[kMaxCluster];
  Float_t  coordY[kMaxCluster];
  Float_t  phi[kMaxCluster];
  Float_t  z[kMaxCluster];
  Short_t  maxCoordNX[kMaxCluster];
  Short_t  maxCoordNY[kMaxCluster];
  Short_t  maxAdcX[kMaxCluster];
  Short_t  maxAdcY[kMaxCluster];
  Char_t   module[kMaxCluster];
  Short_t  nStrHits[kMaxCluster];
  
  //tracks
  Int_t   ngTracks;
  Char_t  gq[kMaxTracks];
  Char_t  nMax[kMaxTracks];
  Char_t  nFit[kMaxTracks];
  Char_t  gmtMid[kMaxTracks];
  Float_t gpt[kMaxTracks];
  Float_t geta[kMaxTracks];
  Float_t gphi[kMaxTracks];
  Float_t localX[kMaxTracks];
  Float_t localY[kMaxTracks];
  Float_t localZ[kMaxTracks];
  Float_t gX[kMaxTracks];
  Float_t gY[kMaxTracks];
  Float_t gZ[kMaxTracks];
  Float_t gmtX[kMaxTracks];
  Float_t gmtY[kMaxTracks];
  Float_t gmtZ[kMaxTracks];
  Float_t gmtLocalX[kMaxTracks];
  Float_t gmtLocalY[kMaxTracks];
  Short_t gmtCoordNX[kMaxTracks];
  Short_t gmtCoordNY[kMaxTracks];
  Char_t gMatMod[kMaxTracks];
  Short_t gMatCoordNX[kMaxTracks];
  Short_t gMatCoordNY[kMaxTracks];
  Float_t gMatCoordX[kMaxTracks];
  Float_t gMatCoordY[kMaxTracks];
  Char_t                mEnd[1];        //!
  ClassDef(StGmtData,1)
};  
#endif
