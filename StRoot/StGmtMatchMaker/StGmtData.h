#ifndef STGMTDATA_H
#define STGMTDATA_H
  const Int_t kMaxHits = 20000;
  const Int_t kMaxCluster = 16;
  const Int_t kMaxNgbhrChan = 5;
  const Int_t kMaxTracks = 10000;

  struct StGmtData {
    Int_t  run, evt;
    //Int_t  trgId[kMaxTriggerIds];
    //Int_t  nTrgIds;
    //Int_t  trgId;
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
    //Short_t  adcCl[kMaxCluster][2000]; //subtracted peds, +- 5 channels of max adc
    //Short_t  chanCl[kMaxCluster][2000]; //0-127:X, 128-255:Y.
    //Char_t  tbCl[kMaxCluster][2000]; 

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

	//Char_t   isPrimary[kMaxTracks]; // 0, 1
	//Float_t ppt[kMaxTracks];
	//Float_t peta[kMaxTracks];
	//Float_t pphi[kMaxTracks];
	//Char_t  gnFtPts[kMaxTracks];
	//Char_t  gnDedxPts[kMaxTracks];
	//Float_t gdca[kMaxTracks];
	//Float_t gdedx[kMaxTracks];

  };  
#endif
