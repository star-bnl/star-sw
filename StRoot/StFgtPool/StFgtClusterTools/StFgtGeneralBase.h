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
    maxAdc=-1;

  };
  Int_t geoId;
  Int_t ped;
  Int_t pedErr;
  Int_t seedType;
  Int_t adc[7]; //timebins
  Double_t charge;
  Double_t chargeUncert;
  Double_t maxAdc;

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
      maxAdcInt=-1;
      maxAdc=-1;
      hasMatch=false;
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
  Double_t maxAdc;
  Double_t maxAdcInt;
  Bool_t hasMatch;
};

class StFgtGeneralBase : public StMaker {
 public:
  StFgtGeneralBase(const Char_t* name="FgtGeneralBase");

  void doLooseClustering();
  //check if cluster has energy match in other layer
  void checkMatches();
  Int_t Make();
  Int_t Finish();
  Int_t fillFromStEvent();
  Int_t fillFromMuDst();

 protected:
   TH2D* chargeMaxAdcCorr;
   TH2D* chargeMaxAdcIntCorr;
   TH1D* hIpZEv;
   TH1D** hNumPulsesP;
   TH1D** hNumChargesP;
   TH1D** hNumPulsesR;
   TH1D** hNumChargesR;

  map<Int_t, Int_t> mapGeoId2Cluster;
  StFgtDb* mDb;
  Double_t vtxZ;
  Int_t vtxRank;
  Int_t evtNr;
  vector<generalCluster> clustersD1;
  vector<generalCluster> clustersD2;
  vector<generalCluster> clustersD3;
  vector<generalCluster> clustersD4;
  vector<generalCluster> clustersD5;
  vector<generalCluster> clustersD6;
  vector<generalCluster>** pClusters;
  vector<generalStrip>* pStrips;
  Bool_t validPulse(generalStrip& strip);
  void checkNumPulses();


 private:   
  ClassDef(StFgtGeneralBase,1);
};
#endif
