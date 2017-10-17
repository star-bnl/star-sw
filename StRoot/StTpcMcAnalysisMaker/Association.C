#ifndef __CINT__
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
#define SETBIT(n,i)  ((n) |= (1 << i))
class StBFChain;
StBFChain *chain;
class St_db_Maker;
St_db_Maker *dbMk = 0;
#endif

void Association(Int_t First, Int_t NEvents,	
		 const Char_t *Run = "trsMini,fcf", //"trs,fcf", 
		 const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd",
		 const Char_t *opt = "PAI"
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString ChainOpt("");
  TString RootFile("");
  TString Opt(opt);
  TString RunOpt(Run);
  RunOpt.ToLower();
  //  ChainOpt += "tpc,Cdst,Kalman,Event,Mc,-EventQA,-EvOut,debug,McAss,McAna,";
  //  ChainOpt += "tpc,Event,Mc,-EventQA,-EvOut,debug,McAss,McAna,";
  //  ChainOpt += "tpc,Cdst,Kalman,Event,Mc,-EventQA,-EvOut,debug,McAss,McTpcAna,";
  //  ChainOpt += ",MakeEvent,ITTF,-SsdIt,-SvtIt,Idst,tpcI,VFMinuit,McAna,-EventQA,-EvOut,-dstout,debug,McTpcAna,analysis,";
  ChainOpt = "MakeEvent,ITTF,-SsdIt,-SvtIt,Idst,tpcI,VFMinuit,McAna,-EventQA,-EvOut,-dstout,McTpcAna,analysis,";
  //  ChainOpt += "tpc,Cdst,Kalman,Event,Mc,-EventQA,-TpcHitMover,-EvOut,debug,";
  //  if (! fileIn) {ChainOpt += "gstar,Y2003X,"; RootFile += Form("gstar_test_%s_%i.root",Run,NEvents);}

  if (! fileIn) {
    ChainOpt += "gstar,Y2004,FieldOn,"; RootFile += "gstar_y4";
  } else {     
    TString FileIn(fileIn);
    RootFile = Form("%s",gSystem->BaseName(FileIn.Data())); 
    if (FileIn.Contains(".fz",TString::kIgnoreCase)) {
      ChainOpt += "fzin,Y2004,";
      RootFile.ReplaceAll(".fzd","");
      RootFile.ReplaceAll(".fz","");
    } else {
      ChainOpt += "in,tpc_daq,";
      RunOpt.ReplaceAll("trsmini,","");
      RunOpt.ReplaceAll("trs,","");
    }
    RootFile.ReplaceAll(".daq","");
  } 
  if (! ChainOpt.Contains("daq",TString::kIgnoreCase)) {
    if (Opt.Contains("FieldOff",TString::kIgnoreCase)) ChainOpt += "FieldOff,";
    if (Opt.Contains("HalfField",TString::kIgnoreCase)) ChainOpt += "HalfField,";
  }
  RootFile += Form("_%s_%s_%i_%i.root",Run,Opt.Data(),First,NEvents);
  ChainOpt += RunOpt;
  cout << "ChainOpt : " << ChainOpt.Data() << endl;
  TString output = RootFile;
  output.ReplaceAll(".root","O.root");
  if (NEvents < 0) {
    bfc(-1,ChainOpt.Data(),0,0,0);
    return;
  }
  bfc(-1,ChainOpt.Data(),fileIn,output.Data(),RootFile.Data());
  St_geant_Maker *geant = (St_geant_Maker *) chain->GetMakerInheritsFrom("St_geant_Maker");
  if (geant) {
    geant->SetDebug(0);
    //   if (ChainOpt.Contains("fcf",TString::kIgnoreCase)) {
    //     StRTSClientFCFMaker *fcf = (StRTSClientFCFMaker *) chain->Maker("tpc_hits");
    //     if (fcf) fcf->SetMode(0x1);
    //   }
    if (! fileIn)       {
    //            NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
    //    geant->Do("gkine 100  14   0.1    10.  -1     1      0    6.28    0.    0.;");
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
  if (ChainOpt.Contains("trsmini",TString::kIgnoreCase)) {
    StTrsMiniMaker *trs = (StTrsMiniMaker *) chain->Maker("TrsMini");
    if (trs) {
      Int_t m_Mode = 0;
      if (Opt.Contains("pai",TString::kIgnoreCase))  SETBIT(m_Mode,StTrsMiniMaker::kPAI); 
      if (Opt.Contains("bichsel",TString::kIgnoreCase))  SETBIT(m_Mode,StTrsMiniMaker::kBICHSEL); 
      //    SETBIT(m_Mode,StTrsMiniMaker::kGAIN); 
      //      SETBIT(m_Mode,StTrsMiniMaker::kGAINO); 
      if (Opt.Contains("fcf",TString::kIgnoreCase)) SETBIT(m_Mode,StTrsMiniMaker::kGAINO);
      SETBIT(m_Mode,StTrsMiniMaker::kNONOISE);
      //    SETBIT(m_Mode,StTrsMiniMaker::kPseudoPadRow);
      //    SETBIT(m_Mode,StTrsMiniMaker::kPedestal);
      //    SETBIT(m_Mode,StTrsMiniMaker::kAVERAGEPEDESTAL);
      //    SETBIT(m_Mode,StTrsMiniMaker::kdEdxCorr);
      //    SETBIT(m_Mode,StTrsMiniMaker::kTree);
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
  StAssociationMaker *ass = (StAssociationMaker *) chain->Maker("StAssociationMaker");
  if (ass) {
    ass->SetDebug(1);
    ass->useIdAssoc();
    ass->useInTracker();
  }
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) {
    cout << "Chain initiation has failed" << endl;
    chain->Fatal(initStat, "during Init()");
  }
  if (NEvents > 0)  chain->EventLoop(First,NEvents);
}
//________________________________________________________________________________
void Association(Int_t NEvents=100,
		 const Char_t *Run = "trs,fcf", // "trsMini,fcf",
		 const Char_t *fileIn = "/star/rcf/simu/rcf1207_01_225evts.fzd",
		 //		 const Char_t *fileIn = 0,
		 //"/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd",
		 const Char_t *opt = "CheckFcF"
		 ) {
  //  /star/data03/daq/2004/093/st_physics_adc_5093007_raw_2050001.daq
  //  /star/data03/daq/2004/fisyak/st_physics_adc_5114043_raw_2080001.daq
  // nofield /star/data03/daq/2004/076/st_physics_adc_5076061_raw_2060001.daq
  //                                   st_physics_adc_5076061_raw_4050001.daq
  Association(0,NEvents,Run,fileIn,opt);
}
