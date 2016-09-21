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
void dEdx(Int_t nevents=1000,
	  const char *MainFile=
	  "/star/data08/reco/dAuMinBias/FullField/P03ih/2003/040/st_physics_4040004_raw_0010010.event.root",
	  //	  "/star/data05/reco/FPDtbEMCproduction/FullField/P02gc/2002/023/st_physics_3023073_raw_0010.event.root",
	  //	  const char *MainFile="st_physics_2313002_raw_0010.event.root",
	  // "/star/data08/reco/central/DEV01a/2000/08/st_physics_1243014_raw_0001.dst.root",
	  const char* rootFile="", Int_t mode = 2)
{
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
  TString Chain("in,TpcHitMover,CorrX,OSpaceZ2,OGridLeak3D,dEdxY2,magF,StEvent,mysql,NoDefault"); // ,analysis
  TString RootFile(rootFile);
  if (RootFile == "") {
    RootFile = gSystem->BaseName(MainFile);
    RootFile.ReplaceAll(".event","");
  }
  chain = bfc(-1,Chain.Data(),MainFile,0,RootFile.Data());
  StdEdxY2Maker *dEdxY2 = (StdEdxY2Maker *) chain->GetMaker("dEdxY2"); 
  StMaker *tofCalib = chain->Maker("tofCalib");
  if (tofCalib) chain->AddAfter("tofCalib",dEdxY2);
  Int_t Mode = 0;
  if (mode%10 == 2) 
    SETBIT(Mode,StdEdxY2Maker::kCalibration);
  SETBIT(Mode,StdEdxY2Maker::kGASHISTOGRAMS);
  //    SETBIT(Mode,StdEdxY2Maker::kProbabilityPlot);
  //  SETBIT(Mode,StdEdxY2Maker::kZBGX);
  if ((mode/100)%10) 
    SETBIT(Mode,StdEdxY2Maker::kDoNotCorrectdEdx);
  if ((mode/1000)%10) SETBIT(Mode,StdEdxY2Maker::kMakeTree);
  SETBIT(Mode,StdEdxY2Maker::kPadSelection); 
  SETBIT(Mode,StdEdxY2Maker::kPadSelection);
  SETBIT(Mode,StdEdxY2Maker::kAlignment);
  //  SETBIT(Mode,StdEdxY2Maker::kMip);
  //  SETBIT(Mode,StdEdxY2Maker::kAdcHistos);
  //  SETBIT(Mode,StdEdxY2Maker::kXYZcheck);
  //  SETBIT(Mode,StdEdxY2Maker::kV0CrossCheck);
  //  SETBIT(Mode,StdEdxY2Maker::kSpaceChargeStudy);
  //  SETBIT(Mode,StdEdxY2Maker::kCORRELATION);
  if (Mode) {
    cout << " set dEdxY2 Mode " << Mode << " =======================================" << endl;
    dEdxY2->SetMode(Mode); 
  }
  if (! gROOT->IsBatch()) dEdxY2->SetDebug(1);
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  if (nevents >= 0)   chain->Init();
  StIOMaker *inMk = (StIOMaker *) chain->GetMaker("inputStream");
  if (inMk) {
    inMk->SetIOMode("r");
    inMk->SetBranch("histBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("eventBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("runcoBranch",0,"0");	//deactivate all branches
    inMk->SetBranch("dstBranch",0,"r");
  }
  chain->EventLoop(1,nevents);
}
