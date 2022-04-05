/*
 * StEmcOfflineCalibrationMaker.h
 * Update author: J. Kevin Adkins, University of Kentucky
 * June 15, 2014
 */

#ifndef STAR_StEmcOfflineCalibrationMaker
#define STAR_StEmcOfflineCalibrationMaker

class TFile;
class TTree;
class TH2F;

class StEmcOfflineCalibrationEvent;
class StEmcOfflineCalibrationTrack;
class StEmcOfflineCalibrationVertex;
class StEmcOfflineCalibrationTrigger;

class StMuDstMaker;
class StEmcADCtoEMaker;
class StTriggerSimuMaker;

class StBemcTables;
class StEmcGeom;
class StEmcPosition;
class StEmcCollection;
class StMuTrack;

#include "StMaker.h"

class StEmcOfflineCalibrationMaker : public StMaker
{
private:
  const char* filename;
  TFile* myFile;
  TTree* calibTree;
  TH2F *mapcheck, *towerSlopes, *preshowerSlopes, *smdeSlopes, *smdpSlopes;
  StEmcOfflineCalibrationEvent* myEvent;
  
  StMuDstMaker* muDstMaker;
  StEmcADCtoEMaker* mADCtoEMaker;
  StTriggerSimuMaker* emcTrigSimu;
  StBemcTables* mTables;
  StEmcPosition* mEmcPosition;
  StEmcCollection* mEmcCollection;
  StEmcGeom* mEmcGeom;
  StEmcGeom* mSmdEGeom;
  StEmcGeom* mSmdPGeom;

  vector<UInt_t> mbTriggers;
  vector<UInt_t> htTriggers;
  
  Int_t mHT0threshold;
  Int_t mHT1threshold;
  Int_t mHT2threshold;
  Int_t mHT3threshold;
  
  //tower info (0==BTOW, 1==BPRS, 2=BSMDE, 3=BSMDP)
  Int_t mADC[2][4800];
  Int_t mADCSmd[2][18000];
  Float_t mPedestal[2][4800];
  Float_t mPedRMS[2][4800];
  Float_t mPedestalSmd[2][18000];
  Float_t mPedRMSSmd[2][18000];
  Int_t	mStatus[2][4800];
  Int_t mStatusSmd[2][18000];
  UChar_t mCapacitor[4800]; //only for BPRS
  UChar_t mCapacitorSmd[2][18000];
  
  void getADCs(Int_t det); //1==BTOW, 2==BPRS, 3=BSMDE, 4=BSMDP
  pair<Int_t, pair<Float_t,Float_t> > getTrackTower(const StMuTrack* track, Bool_t useExitRadius=false, Int_t det=1);
  Float_t getTrackDeltaR(Float_t track_eta, Float_t track_phi, Int_t id);
  pair<Float_t, Float_t> getTrackDetaDphi(Float_t track_eta, Float_t track_phi, Int_t id, Int_t det);
  double highestNeighbor(Int_t id);
  
 public:
  StEmcOfflineCalibrationMaker(const char* name="btowCalibMaker", const char* file="test.root");
  virtual ~StEmcOfflineCalibrationMaker();
  
  virtual Int_t Init();
  virtual Int_t Make();	
  virtual Int_t InitRun(Int_t run);
  virtual Int_t Finish();
  virtual void Clear(Option_t* option="");
    
  void addHighTowerTrigger(UInt_t trigId);
  
  ClassDef(StEmcOfflineCalibrationMaker, 5);
};

#endif
