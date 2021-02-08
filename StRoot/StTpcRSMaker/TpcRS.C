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
void TpcRS(Int_t First, Int_t Last, const Char_t *Run = "y2011,TpcRS",  
	   const Char_t *fileIn = 0, const Char_t *opt = "Bichsel", const Char_t *kuip = 0,
	   const Char_t *fileOut = 0) {
  gROOT->LoadMacro("bfc.C"); 
  TString ChainOpt("");
  TString RootFile(fileOut);
  TString Opt(opt);
  TString RunOpt(Run);
  //  RunOpt.ToLower();
  //ChainOpt = "MakeEvent,ITTF,ForceGeometry,NoSsdIt,NoSvtIt,Idst,VFMinuit,analysis,dEdxY2,";
  if (! RunOpt.Contains("RC.y",TString::kIgnoreCase) &&  
      ! RunOpt.Contains("MC.y",TString::kIgnoreCase)) {
    ChainOpt = "MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,VFMinuit,analysis,dEdxY2,TpcHitMover,bigbig";
    //  ChainOpt += "Corr4";// no dynamical distortion ! ,OSpaceZ2,OGridLeak3D,"; // check that StTpcRSMaker::kDistortion bit is set
    //  ChainOpt += "EvOut,MuDST,MiniMcMk,McTpcAna,IdTruth,useInTracker,-hitfilt,";
    //  ChainOpt += ",CMuDst,MiniMcMk,IdTruth,useInTracker,tree,";
    //    ChainOpt += ",CMuDst,McAna,IdTruth,useInTracker,tree,KFVertex,xgeometry,";
    ChainOpt += ",CMuDst,IdTruth,useInTracker,tree,KFVertex,xgeometry,";
    // ChainOpt += ",tree,";
#if 0
    if (TString(gSystem->Getenv("STAR_VERSION")) == ".DEV2" ||
	TString(gSystem->Getenv("STAR_VERSION")) == "SL11d_embed") ChainOpt += "NoHistos,NoRunco,noTags,McTpcAna,";
    else                                                           ChainOpt += "tags,";
#endif
  }
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
  if (FileIn == "" && fileOut == 0) {
    if (RunOpt.Contains("pythia",TString::kIgnoreCase)) {
      RootFile += "pythia";
    } else if (RunOpt.Contains("hijing",TString::kIgnoreCase)) {
      RootFile += "hijing";
    } else {
      ChainOpt += "gstar,"; RootFile += "gstar";
    }
    if (RunOpt.Contains("hadr_of",TString::kIgnoreCase) ||
	   Opt.Contains("hadr_of",TString::kIgnoreCase)) ChainOpt += "hadr_off,";
    if (RunOpt.Contains("phys_of",TString::kIgnoreCase) ||
	   Opt.Contains("phys_of",TString::kIgnoreCase)) ChainOpt += "phys_off,";
    if (RunOpt.Contains("PhysicsOff",TString::kIgnoreCase) ||
	   Opt.Contains("PhysicsOff",TString::kIgnoreCase)) ChainOpt += "phys_off,";
    if (! RunOpt.Contains("Y20",TString::kIgnoreCase) &&
	! RunOpt.Contains("dev",TString::kIgnoreCase)
	) ChainOpt += "Y2011,";
    if      (Opt.Contains("FieldOff" ,TString::kIgnoreCase)) ChainOpt += "FieldOff,";
    else if (Opt.Contains("HalfField",TString::kIgnoreCase)) ChainOpt += "HalfField,";
    else                                                     ChainOpt += "FieldOn,";
  } else {
    RootFile += Form("%s",gSystem->BaseName(FileIn.Data())); 
    if        (FileIn.Contains(".daq",TString::kIgnoreCase)) {
      ChainOpt += "in,TpxRaw,";
      RootFile.ReplaceAll(".daq","");
    } else if (FileIn.Contains(".fz",TString::kIgnoreCase)) {
      ChainOpt += "fzin,";
      RootFile.ReplaceAll(".fzd","");
      RootFile.ReplaceAll(".fz","");
    } else if (FileIn.Contains(".nt",TString::kIgnoreCase)) {
      ChainOpt += "ntin,";
      RootFile.ReplaceAll(".nt","");
      RootFile.ReplaceAll(".","_");
    } else if (FileIn.Contains(".geant.root",TString::kIgnoreCase)) {
      ChainOpt += "in,";
      RootFile.ReplaceAll(".geant.root","");
    } else if (FileIn.Contains(".MuDst",TString::kIgnoreCase)) {
      ChainOpt += "mtin,";
      RootFile.ReplaceAll(".MuDst.root","");
    } else {
      if (gSystem->AccessPathName(FileIn.Data())) FileIn ="";
    }
  }
  ChainOpt += RunOpt;
  RootFile += Form("_%s_%i_%i",Opt.Data(),First,Last);
  RootFile.ReplaceAll(".root","");
  RootFile.ReplaceAll(",","_");
  if (RootFile.Contains(";")) {
    Int_t index = RootFile.Index(";");
    RootFile = RootFile(0,index);
  }
  RootFile += ".root";
  RootFile.ReplaceAll(" ","");
  cout << "ChainOpt : " << ChainOpt.Data() << "\tOuput file " << RootFile.Data() << endl;
  
  if (Last < 0) {
    bfc(-1,ChainOpt.Data(),0,0,0);
    return;
  }

  TString output = RootFile;
  output = RootFile;
  output.ReplaceAll(".root","O.root");
  output.ReplaceAll("*","");
  if (RunOpt.Contains("devT,",TString::kIgnoreCase)) ChainOpt += ",useXgeom";
  bfc(-1,ChainOpt.Data(),fileIn,output.Data(),RootFile.Data());
  if (ChainOpt.Contains("TpcRS",TString::kIgnoreCase)) {
    StTpcRSMaker *tpcRS = (StTpcRSMaker *) chain->Maker("TpcRS");
    if (tpcRS) {
      //      if (needAlias) tpcRS->SetInput("geant","bfc/.make/inputStream/.make/inputStream_Root/.data/bfcTree/geantBranch");
      Int_t m_Mode = tpcRS->GetMode();
      if (Opt.Contains("pai",TString::kIgnoreCase))         {SETBIT(m_Mode,StTpcRSMaker::kPAI); CLRBIT(m_Mode,StTpcRSMaker::kBICHSEL);}
      if (Opt.Contains("bichsel",TString::kIgnoreCase))     {SETBIT(m_Mode,StTpcRSMaker::kBICHSEL); CLRBIT(m_Mode,StTpcRSMaker::kPAI);}
      if (! ChainOpt.Contains("Corr",TString::kIgnoreCase)) {CLRBIT(m_Mode,StTpcRSMaker::kDistortion);}  // Check that distorton are IN chain
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
    //    if (TString(gSystem->Getenv("STAR_VERSION")) == ".DEV2") 
    SETBIT(Mode,StdEdxY2Maker::kZBGX);
    SETBIT(Mode,StdEdxY2Maker::kGASHISTOGRAMS);
    if (Mode) {
      dEdx->SetDebug(1);
      cout << " set dEdxY2 Mode" << Mode << " =======================================" << endl;
      dEdx->SetMode(Mode); 
    }
    if (mask) {
      cout << " set dEdxY2 mask " << mask << " =======================================" << endl;
      dEdx->SetMask(mask); 
    }
  }
  if (Last < 0) return;
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) {
    cout << "Chain initiation has failed" << endl;
    chain->Fatal(initStat, "during Init()");
  }
  if (gClassTable->GetID("TGiant3") >= 0) {
    St_geant_Maker *geant = (St_geant_Maker *) chain->GetMakerInheritsFrom("St_geant_Maker");
    if (FileIn == "" && 
	! RunOpt.Contains("pythia",TString::kIgnoreCase) && 
	! RunOpt.Contains("hijing",TString::kIgnoreCase)) {
      /** Setup interaction region
       **  mean and sigma for Vx Vy Vz
       **  0.31  -0.35  -1.4  and 0.91   0.77   33.68   respectively .
       
       XVERTEX =                                            0.31
       YVERTEX =                                           -0.35
       ZVERTEX =                                           -1.40
       
       XSIGMA  =                                            0.910
       YSIGMA  =                                            0.770
       ZSIGMA  =                                           36.680
       
       PTmin   =                                            0.000
       PTmax   =                                          100.000
       ETAmin  =                                           -4.500
       ETAmax  =                                           +2.500
       Zmin    =                                         -100.000
       Zmax    =                                         +100.000
       GVERTEX [XVERTEX] [YVERTEX] [ZVERTEX]
       GSPREAD [XSIGMA]  [YSIGMA]  [ZSIGMA]
       GKINE -1 0 [PTmin] [PTmax] [ETAmin] [ETAmax] 0.000 6.283 [Zmin] [Zmax]
      */
      geant->Do("GVERTEX 0.31 -0.35 -1.40");
      geant->Do("GSPREAD 0.910 0.770 36.680");
      geant->Do("GKINE -1 0 0. 100.0 -4.5 4.5 0.000 6.283 -100. 100.");
      if (Opt.Contains("debug",TString::kIgnoreCase)) {
	geant->Do("debug on");
	geant->SetDebug(1);
	geant->Do("swit 1 2");
	geant->Do("swit 2 2");
      }
      //                   NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
      //    geant->Do("gkine 100  14   0.1    10.  -1     1      0    6.28    0.    0.;");
      cout << Opt << endl;
      if (Opt.Contains(":")) {
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
      } else {
	Int_t    NTRACK = 100;
	Int_t    ID = 5;
	Double_t mass = 0.1057;
	Double_t bgMin  = 1e-2; // 3.5;// 1e2; // 1e-2;
	Double_t bgMax  = 1e5;  // 1e2;// 1e5;
	Double_t pTmin = mass*bgMin; if (pTmin <    0.01) pTmin =    0.01;
	Double_t pTmax = mass*bgMax; if (pTmax > 1000.00) pTmax = 1000.00;
	if      (Opt.Contains("muon",TString::kIgnoreCase))     {ID =  5;                 
	  if    (Opt.Contains("muon-",TString::kIgnoreCase))     ID =  6;}
	else if (Opt.Contains("electron",TString::kIgnoreCase)) {ID =  3; mass = 0.5110E-03;}
	else if (Opt.Contains("positron",TString::kIgnoreCase)) {ID =  2; mass = 0.5110E-03;}
	else if (Opt.Contains("pion",TString::kIgnoreCase))     {ID =  8; mass = 0.1396; 
	  if    (Opt.Contains("pion-",TString::kIgnoreCase))     ID =  9;}
	else if (Opt.Contains("kaon",TString::kIgnoreCase))     {ID = 11; mass = 0.4937; 
	  if    (Opt.Contains("kaon-",TString::kIgnoreCase))     ID = 12;}
	else if (Opt.Contains("proton",TString::kIgnoreCase))   {ID = 14; mass = 0.9383; 
	  if    (Opt.Contains("antiproton",TString::kIgnoreCase) ||
		 Opt.Contains("pbar",TString::kIgnoreCase)) ID = 15;}
	else if (Opt.Contains("deuteron",TString::kIgnoreCase)) {ID = 45; mass = 1.876;}
	else if (Opt.Contains("triton",TString::kIgnoreCase))   {ID = 46; mass = 2.80925;}
	else if (Opt.Contains("He3",TString::kIgnoreCase))      {ID = 49; mass = 2.80925;}
	else if (Opt.Contains("alpha",TString::kIgnoreCase))    {ID = 47; mass = 3.727;}
	else if (Opt.Contains("phi",TString::kIgnoreCase))      {ID = 10151; mass = 1.0194; NTRACK = 1; pTmin = 0.010, pTmax = 2.000;}
	if (Opt.Contains("MIP",TString::kIgnoreCase))           {pTmin = 0.2; pTmax = 0.5; bgMin = 3; bgMax = 4;}
	if (Opt.Contains("1GeV",TString::kIgnoreCase))          {pTmin = pTmax = 1.0;}
	if (Opt.Contains("0.5GeV",TString::kIgnoreCase))        {pTmin = pTmax = 0.5;}
	if (Opt.Contains("50",TString::kIgnoreCase))            {NTRACK =   50;}
	if (Opt.Contains("1000",TString::kIgnoreCase))          {NTRACK = 1000;}
	if (Opt.Contains("1muon",TString::kIgnoreCase))          NTRACK = 1;
	if (Opt.Contains("Single",TString::kIgnoreCase))         NTRACK = 1;
	if (Opt.Contains("LamXi2430",TString::kIgnoreCase))     {NTRACK = 50;   ID = 60002;  pTmin = 0.1; pTmax = 10.0;}
	if (RunOpt.Contains("gstarLib",TString::kIgnoreCase)) {geant->Do("call gstar");}
	TRandom3 R(0);
	Double_t bgMin10 = TMath::Log10(bgMin);
	Double_t bgMax10 = TMath::Log10(bgMax);
#if 1
	TString Kine(Form("gkine %i %i %f %f -2  2 0 %f -50 50;",NTRACK,ID,pTmin,pTmax,TMath::TwoPi()));
	cout << "Set kinematics: " << Kine.Data() << endl;
	geant->Do(Kine.Data());
	chain->EventLoop(First,Last);
#else
	for (Int_t ev = First; ev <= Last; ev++) {
	  Double_t pT = -1;
	  for (;;) {
	    Double_t bg10  = bgMin10 + (bgMax10 - bgMin10)*gRandom->Rndm();
	    Double_t bg    = TMath::Power(10., bg10);
	    pT = mass*bg;
	    if (pT < pTmin || pT > pTmax) continue;
	    break;
	  }
	  TString Kine(Form("gkine %i %i %f %f -2  2 0 %f -50 50;",NTRACK,ID,pT,pT,TMath::TwoPi()));
	  cout << "Set kinematics: " << Kine.Data() << endl;
	  geant->Do(Kine.Data());
	  if (chain->MakeEvent()) break;
	}
#endif
      }
      return;
    }
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
	   const Char_t *fileIn = 0,
	   const Char_t *opt = "Bichsel", const Char_t *kuip = 0,
	   const Char_t *fileOut = 0) {
  //  /star/data03/daq/2004/093/st_physics_adc_5093007_raw_2050001.daq
  //  /star/data03/daq/2004/fisyak/st_physics_adc_5114043_raw_2080001.daq
  // nofield /star/data03/daq/2004/076/st_physics_adc_5076061_raw_2060001.daq
  //                                   st_physics_adc_5076061_raw_4050001.daq
  TpcRS(1,Last,Run,fileIn,opt,kuip,fileOut);
}
