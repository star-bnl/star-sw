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
//R__EXTERN StBFChain *chain;
#else
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
#endif
#if 0
class StBFChain;
StBFChain *chain = 0;
class TTree;
class StIOMaker;
#endif
#endif
//#define OLDdEdx
//________________________________________________________________________________
void doEvents_ChargeDist(Int_t nevents=1000,
	  const char *MainFile=
			 "/star/data08/reco/dAuMinBias/FullField/P03ih/2003/040/st_physics_4040004_raw_0010010.event.root") {
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
//     gSystem->Load("St_base");
//     gSystem->Load("StChain");
  }
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("in dEdxY2 StEvent debug");
  //  TString Chain("in,dEdxY2,StEvent,St_geom,tofrMatch,tofpMatch,tofCalib,AlignSectors");
  //  TString Chain("in,dEdxY2,magF,StEvent,AlignSectors,Corr4,OSpaceZ2");
  //  TString Chain("in,dEdxY2,magF,StEvent,St_geom,tofrMatch,tofpMatch,tofCalib,Corr4,OSpaceZ2");
  TString Chain("in,TpcHitMover,CorrX,OSpaceZ2,OGridLeak3D,dEdxY2,magF,StEvent,mysql,NoDefault,MuDST"); // ,analysis
  chain = bfc(-1,Chain.Data(),MainFile);
  gSystem->Load("StPass0CalibMaker");
  StSpaceChargeDistMaker* myMak = new StSpaceChargeDistMaker();
  myMak->SetThrows(0.01);
  //myMak->AcceptTrigger(9300); // accept zerobias triggers
  myMak->AcceptTrigger(-1); // accept all triggers
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  if (nevents >= 0)   chain->Init();
#if 0
  StIOMaker *inMk = (StIOMaker *) chain->GetMaker("inputStream");
  if (inMk) {
    inMk->SetIOMode("r");
    inMk->SetBranch("histBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("eventBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("runcoBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("dstBranch",0,"r");
  }
#endif
  chain->EventLoop(1,nevents);
}
