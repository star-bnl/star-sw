#if !defined(__CINT__) && !defined(__CLING__)
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
StBFChain * bfc(Int_t First, Int_t Last,const Char_t *Chain = "", // + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName=0);
StBFChain *bfc(Int_t First, const Char_t *Chain = "MC2016,20Muons,vmc,Rung.1",
 	       const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName = "");
//________________________________________________________________________________
void dEdx(Int_t nevents=1000,
	  const char *MainFile=	"/star/data08/reco/dAuMinBias/FullField/P03ih/2003/040/st_physics_4040004_raw_0010010.event.root",
	  const char* rootFile="", Int_t mode = 2, const Char_t *year = "y2020") {
  TString Year(year);
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  if (gClassTable->GetID("StTerminateNotified") < 0) {
    gSystem->Load("libStarRoot");
  }
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,TpcHitMover,OSpaceZ2,OGridLeakFull,dEdxY2,magF,StEvent,mysql,NoDefault");
  if        (Year.Contains("2019")) { Chain += ",CorrY"; // ,analysis to add OPr40 for y2019
  } else if (Year.Contains("2020")) { Chain += ",CorrY"; // ,analysis to add OPr40 for y2020
  } else if (Year.Contains("2005")) { Chain += ",SCEbyE,OGridLeak,OShortR,OSpaceZ2,";
  } else                            { Chain += ",CorrX"; // ,analysis to add OPr40 for <= 2018
  }
  TString STAR_VERSION(gSystem->Getenv("STAR_VERSION"));
  if (STAR_VERSION.BeginsWith("TFG") || STAR_VERSION.Contains("DEV2")) {
    Chain += ",quiet,ForcedX";
  } 
  //  Chain += ",CMuDst,picoWrite,noHistos,noRunco,-evout"; // For PicoDst
  TString RootFile(rootFile);
  if (RootFile == "") {
    RootFile = gSystem->BaseName(MainFile);
    RootFile.ReplaceAll(".event","");
  }
  //  if (RootFile.Contains("gstar",TString::IgnoreCase) ||
  //      RootFile.Contains("hijing",TString::IgnoreCase)) Chain += ",Simu";
  chain = bfc(-1,Chain.Data(),MainFile,0,RootFile.Data());
  StdEdxY2Maker *dEdxY2 = (StdEdxY2Maker *) chain->Maker("dEdxY2"); 
  if (dEdxY2) {
    Int_t tMin = 20000101;
    Int_t tMax = 20210101;
    for (Int_t y = 2000; y < 2026; y++) {
      TString Y;
      Y += y;
      //      cout << "Year = " << Year.Data() << "\tyear " << Y.Data() << endl;
      if (Year.Contains(Y)) {
	tMin = 10000*(y-1) + 1101;
	tMax = 10000* y    +  831;
	//	cout << "Year for year " << Y.Data() << "\ttMin = " << tMin << "\ttMax = " << tMax << endl;
	dEdxY2->RemAttr("tMin"); dEdxY2->SetAttr("tMin",tMin);
	dEdxY2->RemAttr("tMax"); dEdxY2->SetAttr("tMax",tMax);
	dEdxY2->PrintAttr();
	break;
      }
    }
  }
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
