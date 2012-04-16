#ifndef _ST_FGT_GENERAL_BASE_MAKER_
#define _ST_FGT_GENERAL_BASE_MAKER_

#include "StMaker.h"
//#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <vector>
#include <TVector3.h>
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

#include "StRoot/StFgtUtil/StFgtConsts.h"
//#include "StRoot/StEvent/StFgtCollection.h"
/*
this class is basically just to fill general clusters from StEvent and MuDsts
*/
struct generalStrip
{
  generalStrip(){};
  generalStrip(Int_t mGeo, Int_t mPed, Int_t mPedErr, Int_t mSeedType, Double_t mCharge, Double_t mChargeUncert)
  {
    geoId=mGeo;
    ped=mPed;
    pedErr=mPedErr;
    seedType=mSeedType;
    charge=mCharge;
    chargeUncert=mChargeUncert;
  };
  Int_t geoId;
  Int_t ped;
  Int_t pedErr;
  Int_t seedType;
  Int_t adc[7]; //timebins
  Double_t charge;
  Double_t chargeUncert;
};
struct generalCluster
{
    generalCluster(){};
    generalCluster(Int_t csg, Char_t l, Double_t dZ, Double_t pP, Double_t pR, Double_t q, Double_t d, Double_t s, Int_t cs, Int_t cc)
    {
      centralStripGeoId=csg;
      layer=l;
      discZ=dZ;
      posPhi=pP;
      posR=pR;
      quad=q;
      disc=d;
      strip=s;
      clusterSize=cs;
      clusterCharge=cc;
      centerStripIdx=-1;
    };
    Int_t centralStripGeoId;
    Char_t layer;
    Double_t discZ;
    Double_t posPhi;
    Double_t posR;
    Short_t quad;
    Short_t disc;
    Short_t strip;
    Int_t clusterSize;
    Int_t clusterCharge;
  Int_t centerStripIdx;
};

class StFgtGeneralBase : public StMaker {
 public:
  StFgtGeneralBase(const Char_t* name="FgtGeneralBase");


  Int_t Make();
  Int_t fillFromStEvent();
  Int_t fillFromMuDst();

 protected:
  map<Int_t, Int_t> mapGeoId2Cluster;
  StFgtDb* mDb;
  vector<generalCluster> clustersD1;
  vector<generalCluster> clustersD2;
  vector<generalCluster> clustersD3;
  vector<generalCluster> clustersD4;
  vector<generalCluster> clustersD5;
  vector<generalCluster> clustersD6;
  vector<generalCluster>** pClusters;

  vector<generalStrip>* pStrips;


 private:   
  ClassDef(StFgtGeneralBase,1);
};
#endif
