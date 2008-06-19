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
#define SETBIT(n,i)  ((n) |= (1 << i))
class StBFChain;
StBFChain *chain;
class St_db_Maker;
St_db_Maker *dbMk = 0;
#endif
//________________________________________________________________________________
void TpcRS(Int_t First, Int_t NEvents, const Char_t *Run = "TpcRS,fcf",  
	   const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd", const Char_t *opt = "PAI") 
{
  gROOT->LoadMacro("bfc.C"); 
  TString ChainOpt("");
  TString RootFile("");
  TString Opt(opt);
  TString RunOpt(Run);
  RunOpt.ToLower();
  ChainOpt = "MakeEvent,ITTF,-SsdIt,-SvtIt,Idst,tpcI,VFMinuit,-EventQA,-EvOut,-dstout,analysis,dEdxY2,noHistos,";
  ChainOpt += "McAna,McTpcAna,IdTruth,useInTracker,";
  if (RunOpt.Contains("fcf",TString::kIgnoreCase)) {
    ChainOpt += "tpc_daq,";
    RunOpt.ReplaceAll("TpcRS,","");
    RunOpt.ReplaceAll("trs,","");
  } else {
    ChainOpt += "TpxRaw,TpxClu,-trs,-tcl,-fcf,-tpc_daq,-tfs,-St_tpc,";
  }
  
  TString FileIn(fileIn);
  if (FileIn == "") {
    ChainOpt += "gstar,Y2004,"; RootFile += "gstar_y4";
    if      (Opt.Contains("FieldOff" ,TString::kIgnoreCase)) ChainOpt += "FieldOff,";
    else if (Opt.Contains("HalfField",TString::kIgnoreCase)) ChainOpt += "HalfField,";
    else                                                     ChainOpt += "FieldOn,";
  }
  if (FileIn.Contains(".daq",TString::kIgnoreCase)) {
    ChainOpt += "in,";
    RootFile = Form("%s",gSystem->BaseName(FileIn.Data())); 
    RootFile.ReplaceAll(".daq","");
  } else {
    if (FileIn.Contains(".fz",TString::kIgnoreCase)) {
      RootFile = Form("%s",gSystem->BaseName(FileIn.Data())); 
      ChainOpt += "fzin,";
      RootFile.ReplaceAll(".fzd","");
      RootFile.ReplaceAll(".fz","");
    }
  }
  ChainOpt += RunOpt;
  RootFile += Form("_%s_%s_%i_%i.root",Run,Opt.Data(),First,NEvents);
  RootFile.ReplaceAll(",","_");
  cout << "ChainOpt : " << ChainOpt.Data() << "\tOuput file " << RootFile.Data() << endl;
  TString output = RootFile;
  output.ReplaceAll(".root","O.root");
  if (NEvents < 0) {
    bfc(-1,ChainOpt.Data(),0,0,0);
    return;
  }
  bfc(-1,ChainOpt.Data(),fileIn,output.Data(),RootFile.Data());
  if (ChainOpt.Contains("TpcRS",TString::kIgnoreCase)) {
    StTpcRSMaker *trs = (StTpcRSMaker *) chain->Maker("TpcRS");
    if (trs) {
      Int_t m_Mode = 0;
      if (Opt.Contains("pai",TString::kIgnoreCase))  SETBIT(m_Mode,StTpcRSMaker::kPAI); 
      if (Opt.Contains("bichsel",TString::kIgnoreCase))  SETBIT(m_Mode,StTpcRSMaker::kBICHSEL); 
      //    SETBIT(m_Mode,StTpcRSMaker::kGAIN); 
      //      SETBIT(m_Mode,StTpcRSMaker::kGAINO); 
      if (Opt.Contains("fcf",TString::kIgnoreCase)) SETBIT(m_Mode,StTpcRSMaker::kGAINO);
      SETBIT(m_Mode,StTpcRSMaker::kNONOISE);
      //    SETBIT(m_Mode,StTpcRSMaker::kPseudoPadRow);
      //    SETBIT(m_Mode,StTpcRSMaker::kPedestal);
      //    SETBIT(m_Mode,StTpcRSMaker::kAVERAGEPEDESTAL);
      //    SETBIT(m_Mode,StTpcRSMaker::kdEdxCorr);
      //    SETBIT(m_Mode,StTpcRSMaker::kTree);
      trs->SetMode(m_Mode);
      //      trs->SetDebug(112);
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
#endif    
    Int_t Mode = 2;
    //    SETBIT(Mode,StdEdxY2Maker::kOldClusterFinder); 
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
  if (FileIn == "") {
    St_geant_Maker *geant = (St_geant_Maker *) chain->GetMakerInheritsFrom("St_geant_Maker");
    //    geant->SetDebug(0);
    if (! fileIn)       {
      //            NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
      //    geant->Do("gkine 100  14   0.1    10.  -1     1      0    6.28    0.    0.;");
      cout << Opt << endl;
     if (Opt.Contains("PhysicsOff",TString::kIgnoreCase)) {
	geant->Do("DCAY 0");
	geant->Do("ANNI 0");
	geant->Do("BREM 0");
	geant->Do("COMP 0");
	geant->Do("HADR 0");
	geant->Do("MUNU 0");
	geant->Do("PAIR 0");
	geant->Do("PFIS 0");
	geant->Do("PHOT 0");
	geant->Do("RAYL 0");
	geant->Do("LOSS 4"); // no fluctuations 
	//    geant->Do("LOSS 1"); // with delta electron above dcute
	geant->Do("DRAY 0");
	geant->Do("MULS 1");
	geant->Do("STRA 0");
	//         CUTS   CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
	geant->Do("CUTS     1e-4   1e-4   .001   .001   .001  .001  .001  1e-4  .001   .001 50.e-6");
	//    geant->Do("gclose all");
	geant->Do("physi");
      }
      if ( Opt.Contains("laser",TString::kIgnoreCase)) {
	gSystem->Load("gstar.so");
	geant->Do("call gstar");
	geant->Do("gkine 1 170   1   1  0   0   0  0    180.00    180.00;");
	geant->Do("gprint kine");
	geant->Do("gvert 0  54   0");
	geant->Do("debug on");
	geant->Do("swit 1 2");
	geant->Do("swit 2 2");
	geant->Do("mode TRAC prin 15");
      } else 
	if (Opt.Contains("pion",TString::kIgnoreCase)) 
	  geant->Do("gkine 100  8   0.4     1.  -1     1      0    6.28    -20.    20.;");
	else if (Opt.Contains("1muon",TString::kIgnoreCase)) 
	  geant->Do("gkine   1  6   0.4     1.  -.1     .1      0    0     -20.    20.;");
	else if (Opt.Contains("50muons1GeV",TString::kIgnoreCase)) 
	  geant->Do("gkine  50  6   1.     1.  -1     1      0    6.28    -20.    20.;");
	else if (Opt.Contains("deuteron",TString::kIgnoreCase)) 
	  geant->Do("gkine 100 45   0.05  100.  -1     1      0    6.28    -20.    20.;");
	else // proton
	  geant->Do("gkine 100 14   0.05   50.  -1     1      0    6.28    -20.    20.;");
    }
  }
  if (NEvents > 0)  chain->EventLoop(First,NEvents);
}
//________________________________________________________________________________
void TpcRS(Int_t NEvents=100,
	   const Char_t *Run = "trs,fcf", // "TpcRS,fcf",
	   const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd",
	   //		 const Char_t *fileIn = 0,
	   //"/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd",
	   const Char_t *opt = "CheckFcF"
	   ) {
  //  /star/data03/daq/2004/093/st_physics_adc_5093007_raw_2050001.daq
  //  /star/data03/daq/2004/fisyak/st_physics_adc_5114043_raw_2080001.daq
  // nofield /star/data03/daq/2004/076/st_physics_adc_5076061_raw_2060001.daq
  //                                   st_physics_adc_5076061_raw_4050001.daq
  TpcRS(0,NEvents,Run,fileIn,opt);
}
