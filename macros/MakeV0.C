#ifndef __CINT__
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StBFChain.h"
#include "StIOMaker.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
#else
class StBFChain;
#endif
//________________________________________________________________________________
void MakeV0(Int_t nevents=1000, const Char_t *parn = "K0s", Int_t RunID = 1)
{
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  gROOT->LoadMacro("bfc.C");
  TString Chain("AgML,TpcRS,bbcSim,btofsim,pxlFastSim,istSim,emcY2,eefs,TpxClu,y2014a,useXgeom,FieldOn,NoSsdIt"
		",gstarLib"
		",NoSvtIt,StiHftC,Idst,BAna,tpcDB,TpcHitMover,btofMatch,btofCalib,tags,emcY2,IdTruth,gstar"
		",Corr4,OSpaceZ2,OGridLeak3D"
		",StiCA,beamline,KFVertex,StiHftC,pxlFastSim,ssdfast,useXgeom,VFMCE,noRunco,noHistos,noTags");
  //		",muMc"); 
  Int_t iD = 0;
  TString ParName(parn);
  Int_t Npart = 10;
  if      (ParName == "K0s"  )      iD = 16;
  else if (ParName == "StK0s"  )   {iD = 16; Npart =   1;}
  else if (ParName == "K0l"  )      iD = 10;
  else if (ParName == "gamma")     {iD =  1; Npart = 100;}
  else if (ParName == "Lambda")     iD = 18;
  else if (ParName == "AntiLambda") iD = 26;
  if (! iD) {
    cout << "nonrecognized particle name " << parn << endl;
    return;
  }
  TString RootFile = ParName;
  TString MainFile = RootFile;
  MainFile += ".root";
  RootFile += "_"; RootFile += RunID; RootFile += ".MuDst.root";
  Chain += Form(",rung.%i",RunID);
  chain = bfc(0,Chain.Data(),0,RootFile.Data(),MainFile.Data());
  Double_t pTlow = 0.2;
  Double_t pThigh = 20;
  Double_t Ylow = -1;
  Double_t Yhigh = 1;
  Double_t Philow = 0;
  Double_t Phihigh = 2*TMath::Pi();
  Double_t Zlow = -20;
  Double_t Zhigh = 20;
  TString kine(Form("gkine %i %i %f %f %f %f %f %f %f %f",Npart,iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh));
  St_geant_Maker::instance()->Do(kine.Data());
  chain->EventLoop(nevents);
}
