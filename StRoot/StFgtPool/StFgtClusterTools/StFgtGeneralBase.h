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


//#define COSMIC
class StFgtStrip;
class StFgtCollection;
class StFgtHit;
//class StFgtCollection;

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
  StFgtStrip* fgtStrip;
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
  generalCluster(Int_t csg, Char_t l, Double_t dZ, Double_t pP, Double_t pR, Double_t q, Double_t d, Double_t s, Int_t cs, Int_t cc, Float_t cUncert)
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
      seedType=kFgtSeedTypeNo;
      clusterChargeUncert=cUncert;
    };


  StFgtHit* fgtHit;
  Int_t seedType;
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
  Float_t clusterChargeUncert;
  Int_t centerStripIdx;
  Double_t maxAdc;
  Double_t maxAdcInt;
  Bool_t hasMatch;

};

class StFgtGeneralBase : public StMaker {
 public:
  Double_t vtxZ;
  Int_t vtxRank;
  Int_t mVertexNumber;
  StFgtGeneralBase(const Char_t* name="FgtGeneralBase");

  void fillFromEvent(Bool_t fillFromEv=true); //default, no, use mDsts
  Bool_t isCosmic()
  {
    return m_isCosmic;
  }
  void setCosmic(Bool_t iC)
  {
    m_isCosmic=iC;
  }
  void useEHTTrig(Bool_t useEHTTrig);

  void setVertexNumber(Int_t r=0);
  void doLooseClustering();
  //check if cluster has energy match in other layer
  void checkMatches();
  Int_t Make();
  Int_t Finish();
  Int_t fillFromStEvent(StFgtCollection* fgtCollectionPtr);
  Int_t fillFromMuDst(StFgtCollection&);
  void setChargeMatchCut(Float_t cut);

static float getLocDiscZ(int iD)
  {
#ifndef COSMIC
    return StFgtGeom::getDiscZ(iD);
#endif

    switch(iD)
      {
      case 0:
	return 0.0;
      case 1: 
	return 16.51;
      case 2:
	return 2.0*16.51;
      default:
	return -100;
      }
    return -100;

  }


  void SetEffDisk(Int_t disk)
  {
    m_effDisk=disk;
  }
  StFgtCollection* getFgtCollection()
    {
 return fgtCollection;
    }

  vector<generalCluster>** getClusters()
    {
      return pClusters;
    }

  vector<generalStrip>* getStrips()
    {
      return pStrips;
    }
  static Bool_t arePointsMatched(vector<generalCluster>::iterator  c1, vector<generalCluster>::iterator  c2);
   void SetFileBase(const Char_t* filebase);
 protected:

  Char_t fileBase[300];
enum evStatCodes
  {
    numEvents,
    numCluster,
    numCluSeed1,
    numCluSeed2,
    numCluSeed3
  };
 StFgtCollection* fgtCollection;
 Bool_t m_fillFromEvent;
 Bool_t m_isCosmic;
 Bool_t mUseEHTTrigs;
  //EHT1*L2EW == 430315
  //EHT1 == 430313
  //EHT0 == 430312
 static const Int_t trigID[3];

  TH1D* clusWChargeMatch;
  TH1D* clusWOChargeMatch;
  TH1D* evStatistics;
   TH2D* chargeMaxAdcCorr;
   TH2D* chargeMaxAdcIntCorr;
   TH1D* hIpZEv;
   TH1D** hNumPulsesP;
   TH1D** hNumChargesP;
   TH1D** hNumPulsesR;
   TH1D** hNumChargesR;
   TH1D** hNumClustersP;
   TH1D** hNumClustersR;



  map<Int_t, Int_t> mapGeoId2Cluster;
  StFgtDb* mDb;

  static Float_t chargeMatchCut;

  Int_t evtNr;
  Int_t m_effDisk;
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
   void doEvAssoc();

 private:   
  ClassDef(StFgtGeneralBase,1);
};
inline void StFgtGeneralBase::setChargeMatchCut(Float_t cut)
{chargeMatchCut=cut;};
inline void StFgtGeneralBase::setVertexNumber(Int_t r)
{
  mVertexNumber=r;
}
inline void StFgtGeneralBase::useEHTTrig(Bool_t useEHTTrig)
{
  mUseEHTTrigs=useEHTTrig;
}
#endif
