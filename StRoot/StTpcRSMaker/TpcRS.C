#ifndef __CINT__
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TString.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StBFChain.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
//R__EXTERN StBFChain *chain;
#else
#define SETBIT(n,i)  ((n) |=  (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
class StBFChain;
StBFChain *chain;
class St_db_Maker;
St_db_Maker *dbMk = 0;
#endif
//________________________________________________________________________________
void TpcRS(Int_t First, Int_t Last, const Char_t *Run = "y2009,TpcRS",  
	   const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd", const Char_t *opt = "Bichsel", const Char_t *kuip = 0) {
  gROOT->LoadMacro("bfc.C"); 
  TString ChainOpt("");
  TString RootFile("");
  TString Opt(opt);
  TString RunOpt(Run);
  RunOpt.ToLower();
  //  ChainOpt = "MakeEvent,ITTF,ForceGeometry,NoSsdIt,NoSvtIt,Idst,VFMinuit,analysis,dEdxY2,";
  ChainOpt = "MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,VFMinuit,analysis,dEdxY2,NoHistos,NoRunco,";
  ChainOpt += "Corr4";// no dynamical distortion ! ,OSpaceZ2,OGridLeak3D,"; // check that StTpcRSMaker::kDistortion bit is set
  //  ChainOpt += "EvOut,MuDST,MiniMcMk,McTpcAna,IdTruth,useInTracker,-hitfilt,";
  ChainOpt += ",CMuDst,MiniMcMk,McTpcAna,IdTruth,useInTracker,";
  // ChainOpt += "MiniMcMk,IdTruth,useInTracker,-hitfilt,CMuDst,Tree,tags,evout,";
  if (RunOpt.Contains("fcf",TString::kIgnoreCase)) {
    ChainOpt += "tpl,tpcI,";
    RunOpt.ReplaceAll("TpcRS,","");
    RunOpt.ReplaceAll("trs,","");
  } else {
    ChainOpt += "tpcDB,TpcHitMover,TpxClu,";
  }
  //  Bool_t needAlias = kFALSE;
  TString FileIn(fileIn);
  if (FileIn == "") {
    ChainOpt += "gstar,"; RootFile += "gstar";
    if (RunOpt.Contains("hadr_of",TString::kIgnoreCase) ||
	   Opt.Contains("hadr_of",TString::kIgnoreCase)) ChainOpt += "hadr_off,";
    if (RunOpt.Contains("phys_of",TString::kIgnoreCase) ||
	   Opt.Contains("phys_of",TString::kIgnoreCase)) ChainOpt += "phys_off,";
    if (RunOpt.Contains("PhysicsOff",TString::kIgnoreCase) ||
	   Opt.Contains("PhysicsOff",TString::kIgnoreCase)) ChainOpt += "phys_off,";
    if (! RunOpt.Contains("Y200",TString::kIgnoreCase)) ChainOpt += "Y2011,";
    if      (Opt.Contains("FieldOff" ,TString::kIgnoreCase)) ChainOpt += "FieldOff,";
    else if (Opt.Contains("HalfField",TString::kIgnoreCase)) ChainOpt += "HalfField,";
    else                                                     ChainOpt += "FieldOn,";
  } else {
    RootFile = Form("%s",gSystem->BaseName(FileIn.Data())); 
    if (FileIn.Contains(".daq",TString::kIgnoreCase)) {
      ChainOpt += "in,TpxRaw,";
      RootFile.ReplaceAll(".daq","");
    } else {
      if (FileIn.Contains(".fz",TString::kIgnoreCase)) {
	ChainOpt += "fzin,";
	RootFile.ReplaceAll(".fzd","");
	RootFile.ReplaceAll(".fz","");
      } else {
	if (FileIn.Contains(".geant.root",TString::kIgnoreCase)) {
	  ChainOpt += "in,";
	  RootFile.ReplaceAll(".geant.root","");
	} else {
	  if (FileIn.Contains(".MuDst",TString::kIgnoreCase)) {
	    ChainOpt += "mtin,";
	    RootFile.ReplaceAll(".MuDst.root","");
	  }
	}
      }
    }
  }
  ChainOpt += RunOpt;
  RootFile += Form("_%s_%s_%i_%i",Run,Opt.Data(),First,Last);
  RootFile.ReplaceAll(",","_");
  if (RootFile.Contains(";")) {
    Int_t index = RootFile.Index(";");
    RootFile = RootFile(0,index);
  }
  RootFile += ".root";
  RootFile.ReplaceAll(" ","");
  cout << "ChainOpt : " << ChainOpt.Data() << "\tOuput file " << RootFile.Data() << endl;
  

  TString output = RootFile;
  output.ReplaceAll(".root","O.root");
  output.ReplaceAll("*","");
  if (Last < 0) {
    bfc(-1,ChainOpt.Data(),0,0,0);
    return;
  }
  bfc(-1,ChainOpt.Data(),fileIn,output.Data(),RootFile.Data());
  if (ChainOpt.Contains("TpcRS",TString::kIgnoreCase)) {
    StTpcRSMaker *tpcRS = (StTpcRSMaker *) chain->Maker("TpcRS");
    if (tpcRS) {
      //      if (needAlias) tpcRS->SetInput("geant","bfc/.make/inputStream/.make/inputStream_Root/.data/bfcTree/geantBranch");
      Int_t m_Mode = tpcRS->GetMode();
#if 1
      if (Opt.Contains("pai",TString::kIgnoreCase))     {SETBIT(m_Mode,StTpcRSMaker::kPAI); CLRBIT(m_Mode,StTpcRSMaker::kBICHSEL);}
      if (Opt.Contains("bichsel",TString::kIgnoreCase)) {SETBIT(m_Mode,StTpcRSMaker::kBICHSEL); CLRBIT(m_Mode,StTpcRSMaker::kPAI);}
#endif
      //      CLRBIT(m_Mode,StTpcRSMaker::kDistortion);  // Check that distorton are IN chain
      tpcRS->SetMode(m_Mode);
      //      tpcRS->SetDebug(13);
    }
  }
  else {
    if (ChainOpt.Contains("trs",TString::kIgnoreCase)) {
      StMaker *mk = chain->Maker("tpcDB"); // simulation mode
      if (! mk) return;
      mk->SetMode(1); 
    }
  }
  StMaker *dEdxY2 = chain->GetMaker("dEdxY2"); 
  if (dEdxY2) {
    StdEdxY2Maker *dEdx = (StdEdxY2Maker *) dEdxY2;
    Int_t mask = 0;
#if 0
    //     SETBIT(mask,StTpcdEdxCorrection::ktpcPressure); 
    //     SETBIT(mask,StTpcdEdxCorrection::kAdcCorrection); 
    //     SETBIT(mask,StTpcdEdxCorrection::kTpcSecRow); 
    //     SETBIT(mask,StTpcdEdxCorrection::kDrift);
    //     SETBIT(mask,StTpcdEdxCorrection::kzCorrection);
    //     SETBIT(mask,StTpcdEdxCorrection::kdXCorrection);
    //     SETBIT(mask,StTpcdEdxCorrection::kTpcdEdxCor);
    //     SETBIT(mask,StTpcdEdxCorrection::kTpcLengthCorrection);
    // from dEdx 06/20/04
    SETBIT(mask,StTpcdEdxCorrection::ktpcPressure); 
    //  SETBIT(mask,StTpcdEdxCorrection::ktpcMethaneIn); 
    //  SETBIT(mask,StTpcdEdxCorrection::ktpcGasTemperature); 
    //  SETBIT(mask,StTpcdEdxCorrection::ktpcWaterOut); 
    SETBIT(mask,StTpcdEdxCorrection::kAdcCorrection); 
    SETBIT(mask,StTpcdEdxCorrection::kTpcSecRow); 
    SETBIT(mask,StTpcdEdxCorrection::kDrift);
    SETBIT(mask,StTpcdEdxCorrection::kzCorrection);
    SETBIT(mask,StTpcdEdxCorrection::kdXCorrection);
    //  SETBIT(mask,StTpcdEdxCorrection::kTpcdEdxCor);
    //  SETBIT(mask,StTpcdEdxCorrection::kTpcLengthCorrection);
    SETBIT(mask,StTpcdEdxCorrection::kAdcCorrection);
    SETBIT(mask,StTpcdEdxCorrection::kTpcLast);
    //    SETBIT(Mode,StdEdxY2Maker::kOldClusterFinder); 
    //    SETBIT(Mode,StdEdxY2Maker::kDoNotCorrectdEdx);
#endif    
    Int_t Mode = 2;
    SETBIT(Mode,StdEdxY2Maker::kPadSelection); 
    SETBIT(Mode,StdEdxY2Maker::kCalibration);
    if (Mode) {
      cout << " set dEdxY2 Mode" << Mode << " =======================================" << endl;
      dEdx->SetMode(Mode); 
    }
    if (mask) {
      cout << " set dEdxY2 mask " << mask << " =======================================" << endl;
      dEdx->SetMask(mask); 
    }
  }
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) {
    cout << "Chain initiation has failed" << endl;
    chain->Fatal(initStat, "during Init()");
  }
  if (FileIn == "" && gClassTable->GetID("TGiant3") >= 0) {
    St_geant_Maker *geant = (St_geant_Maker *) chain->GetMakerInheritsFrom("St_geant_Maker");
    if (Opt.Contains("debug",TString::kIgnoreCase)) {
      geant->Do("debug on");
      geant->SetDebug(1);
      geant->Do("swit 1 2");
      geant->Do("swit 2 2");
    }
    //            NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
    //    geant->Do("gkine 100  14   0.1    10.  -1     1      0    6.28    0.    0.;");
    cout << Opt << endl;
    if (First > 0) {
      Int_t i = (gSystem->GetPid()+ First)%256;
      geant->Do(Form("rndm %i; rndm;",i));
    }
    if ( Opt.Contains("laser",TString::kIgnoreCase)) {
      gSystem->Load("gstar.so");
      geant->Do("call gstar");
      geant->Do("gkine 1 170   1   1  0   0   0  0    180.00    180.00;");
      geant->Do("gprint kine");
      geant->Do("gvert 0  54   0");
      geant->Do("mode TRAC prin 15");
    } else 
      if (Opt.Contains("pion",TString::kIgnoreCase)) 
	geant->Do("gkine 100  8   0.4     1.  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("1muon",TString::kIgnoreCase)) 
	geant->Do("gkine   1  6   0.4     1.  -.1     .1      0    0     -50.    50.;");
      else if (Opt.Contains("50muons1GeV",TString::kIgnoreCase)) 
	geant->Do("gkine  50  6   1.     1.  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("1000muons1GeV",TString::kIgnoreCase)) 
	geant->Do("gkine  1000  6   1.     1.  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("1alpha1GeV",TString::kIgnoreCase)) 
	geant->Do("gkine  1  47   1.     1.  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("50muons0.5GeV",TString::kIgnoreCase)) 
	geant->Do("gkine  50  6  0.5  0.5  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("deuteron",TString::kIgnoreCase)) 
	geant->Do("gkine 100 45   0.05  100.  -1     1      0    6.28    -50.    50.;");
      else if (Opt.Contains("proton",TString::kIgnoreCase)) // proton
	geant->Do("gkine 100 14   0.05   50.  -1     1      0    6.28    -50.    50.;");
    if (kuip) {
      TString Kuip(kuip);
      geant->Do(kuip);
    }
  }
  if (Last > 0)  chain->EventLoop(First,Last);
}
//________________________________________________________________________________
void TpcRS(Int_t Last=100,
	   const Char_t *Run = "y2009,TpcRS",//trs,fcf", // "TpcRS,fcf",
	   const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd",
	   //		 const Char_t *fileIn = 0,
	   //"/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd",
	   const Char_t *opt = "Bichsel", const Char_t *kuip = 0) {
  //  /star/data03/daq/2004/093/st_physics_adc_5093007_raw_2050001.daq
  //  /star/data03/daq/2004/fisyak/st_physics_adc_5114043_raw_2080001.daq
  // nofield /star/data03/daq/2004/076/st_physics_adc_5076061_raw_2060001.daq
  //                                   st_physics_adc_5076061_raw_4050001.daq
  TpcRS(1,Last,Run,fileIn,opt,kuip);
}
