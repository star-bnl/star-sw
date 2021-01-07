/*
  root.exe -q -b 'TpcAligner.C(1e8,"*event.root")' 
 */
#if !defined(__CINT__) && !defined(__CLING__)
#include "iostream.h"
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

class StBFChain;
class TTree;
StBFChain *chain;
class StIOMaker;
class StTpcAlignerMaker;
#endif
//#define OLDdEdx
//________________________________________________________________________________
void TpcAligner(Int_t nevents=1e8,
		const char *MainFile="*.event.root", 
		//		const char *MainFile="st_physics_19168025_raw_1500001.event.root", 
		//"st_laser_adc_19169021_raw_3500001.event.root",
		// "/star/data45/reco/productionMinBias/FullField/P04ij/2004/030/st_physics_5030012_raw_4010010.event.root",
		//	  "/star/data08/reco/dAuMinBias/FullField/P03ih/2003/040/st_physics_4040004_raw_0010010.event.root",
		//		const char* rootFile="TpcAlignerFullFieldOldHelix.root")
		const char *Tag = "", // "y2018,CorrX",
// 		const char *Tag = "y2014a,Corr4"
		const Char_t *output = 0) 
{
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Stu,ExB,OClock,OPr13,OTwist,OIFC,Corr3,OShortR,TpcHitMover,analysis"); // no BCorrection
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Stu,Corr4,TpcHitMover,analysis"); // 2D corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Stu,Corr4,-OBmap2D,-OTwist,OBmap,OSectorAlign,TpcHitMover,analysis"); // 3D corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Stu,Corr4,-OBmap2D,-OTwist,OBmap,TpcHitMover,analysis,TpcAligner"); // 3D corrections
  //  TString Chain("in,y2013_1,AgML,StarMagField,mysql,tpcDb,StEvent,Event,Stu,Corr4,analysis,StiCA,TpcAligner,TpcHitMover,Corr4,OSectorAlign,LaserIT,"); // No corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Event,Stu,analysis,StiCA,KFVertex,TpcAligner,TpcHitMover,OSpaceZ2,OGridLeakFull,BAna,Tree,CMuDst,"); // No corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Event,Stu,analysis,StiCA,KFVertex,TpcAligner,TpcHitMover,BAna,Tree,CMuDst,"); // No corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Event,Stu,analysis,StiCA,KFVertex,TpcAligner,TpcHitMover,BAna,Tree,"); // No corrections
  //  TString Chain("in,StarMagField,mysql,tpcDb,StEvent,Event,Stu,analysis,TpcAligner,OPr40,CorrX,useCDV,TpcHitMover,quiet,");// ,BAna,Tree,"); //StiCA,CMuDst,"); // No corrections
  TString Chain("in,StarMagField,mysql,tpcDb,detDb,StEvent,Event,Stu,analysis,TpcAligner,CorrY,OSpaceZ2,TpcHitMover,StiCA,");// ,BAna,Tree,"); //StiCA,CMuDst,"); // No corrections
  Chain += Tag;
  //  Chain += "P2019a";
  //  Chain += ",AgML,NoHistos,noTags,noRunco,NoDefault";
  Chain += ",NoHistos,noTags,noRunco,NoDefault,-evout,-hitfilt";
  TString rFile(output);
  if (rFile == "") {
    rFile = gSystem->BaseName(MainFile);
    rFile.ReplaceAll("*","");
    rFile.ReplaceAll(".event","");
    rFile.ReplaceAll(".root","");
    rFile.ReplaceAll("..",".");
    //    rFile += Tag;
    rFile.ReplaceAll(",","_");
#if 0
    rFile += "_";
    rFile += nevents;
#endif
    rFile.Strip();
    rFile += "Aligner.root";
  }
  bfc(-1,Chain.Data(),MainFile,0,rFile.Data());
  if (nevents < 0) return;
  //  StTpcAlignerMaker *aligner = (StTpcAlignerMaker *) chain->Maker("TpcAligner");
  //  aligner->SetMode(2); // take out misalignment
  //  aligner->SetDebug(12);
  if (nevents >= 0)   chain->Init();
//   StIOMaker *inMk = (StIOMaker *) chain->GetMaker("inputStream");
//   if (inMk) {
//     inMk->SetIOMode("r");
//     inMk->SetBranch("histBranch",0,"0");	//deactivate all branches
//     inMk->SetBranch("eventBranch",0,"0");	//deactivate all branches
//     inMk->SetBranch("runcoBranch",0,"0");	//deactivate all branches
//     inMk->SetBranch("dstBranch",0,"r");
//   }
  chain->EventLoop(nevents);
}
