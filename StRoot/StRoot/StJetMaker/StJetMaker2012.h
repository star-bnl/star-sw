//******************************
//This is the 2012 version of Jet 
//Maker with off-axis cone under-
//-lying event study included.
//The unde-rlying event will be
//saved in the tree called "ue".
//The Maker is derivatived from
//the older 2009 version.
//
//Author: Zilong Chang
//User name: zchang
//Cyclotron Institute
//Texas A&M University
//******************************
#ifndef ST_JET_MAKER_2012_H
#define ST_JET_MAKER_2012_H

#include "StJetMaker2009.h"

#include "StSpinPool/StUeEvent/StUeOffAxisConesEvent.h"

class StOffAxisConesPars : public TObject{
 public:
 StOffAxisConesPars() : mConeR(0.5){
  }
  StOffAxisConesPars(double R){
    mConeR = R;
  }
  double coneRadius() const{ return mConeR;}
 private:
  double mConeR;
  ClassDef(StOffAxisConesPars, 0);
};

class StJetMaker2012 : public StJetMaker2009 {
public:
  StJetMaker2012(const char* name = "StJetMaker2012")
    :StJetMaker2009(name)
    , mFileUe(0)
    , mTreeUe(0)
  {
  }

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();
  int  Finish();

  // Setters
  void addUeBranch(const char* name, StOffAxisConesPars* pars);
  void setJetFileUe(const char* filename);

  // Getters

  TTree* treeUe();
  StUeOffAxisConesEvent* eventUe(const char* branchname);

protected:
  double DeltaR(double etaA, double phiA, double etaB, double phiB);
  double addJetUe(StProtoJet::FourVecList particles, StUeOffAxisConesEvent *ueEvent, const StProtoJet &jet, double radius = 0.5);

  struct StJetUeBranch{
  StJetUeBranch(const char* name, StOffAxisConesPars *pars) 
  : name(name)
  , uePars(pars)
    {
    }
    TString name;
    StOffAxisConesPars *uePars;
    vector<StUeOffAxisConesEvent*> eventUe;
  };

  vector<StJetUeBranch*> mJetUeBranches;

  TString mFileNameUe;
  TFile* mFileUe;
  TTree* mTreeUe;

  ClassDef(StJetMaker2012,0);
};
#endif
