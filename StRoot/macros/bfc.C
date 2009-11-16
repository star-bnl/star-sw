//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.166.2.1 2009/10/23 18:17:14 didenko Exp $
//////////////////////////////////////////////////////////////////////////
class StBFChain;        
class StMessMgr;
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#endif
StBFChain    *chain=0; 
//_____________________________________________________________________
//____________    Chain examples ______________________________________
struct Chain_t {
  Char_t *Name;
  Char_t *Opts;
  Char_t *Input;
  Char_t *Comment;
};
const Chain_t Chains[] = {
  // tpc only 
  {"----"  ,"Simple test chains",                                           "Input File Name","Comment"},
  {"gstar","gstar,y2004c",0,"create y2004c geometry and simulate one event"},
  {"StEventIn" ,"in,StEvent,nodefault","/star/rcf/test/dev/daq_sl302/Fri/year_2001/central/st_physics_2327038_raw_0010.event.root","read a StEvent"},
  {"McEvOut4","VMC,Y2004,FieldON,McEvent,McEvOut","/star/data07/calib/fisyak/evgen/4prong.root","Simulate Event from ntuple fiel and write McEvent into 4prong.McEvent.root"},
  {"McEvIn4","in,StMcEvent,nodefault","4prong.McEvent.root","Read in McEvent"},
  {"trsz"  ,"fzin,Y2004,trs,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,miniMcMk", 
   "/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz",
   "Simplest chain with Trs and reading simulated events from zebra file"},
  {"trsc"  ,"gstar,Y2004,trs,SvtSlowSim,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,miniMcMk",0,"Trs+Svt with simulated event on fly"}, // root4star
  {"tfsc"  ,"gstar,Y2004,tfs,SvtSlowSim,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,miniMcMk",0,"Tfs+Svt with simulated event on fly"}, // root4star
  {"trscIT","gstar,Y2004,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem",0,
   "simulation on fly + IT reconstruction"}, // root4star
  {"----"  ,"VMC test chains (should be used with root.exe instead of root4star)",0,""},
  {"vmccIT","VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem,StarMagField,debug",0,"simulation of fly with VMC + slow digitization + IT reconstruction"},  // root.exe
  {"vmcctfsIT","VMC,Y2004,FieldON,MakeEvent,tfs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem,StarMagField,debug",0,"simulation of fly with VMC + slow digitization (excerp TPC) + IT reconstruction"},  // root.exe
  {"vmcctIT"  ,"VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem,StarMagField,debug","/star/simu/simu/gstardata/evgenRoot/evgen.3.root","Reading generated events from NTuple, VMC simulation + IT reconstruction"},
  {"vmcc4IT"  ,"VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem,StarMagField,debug","/star/data07/calib/fisyak/evgen/4prong.nt","Reading generated events from NTuple, VMC simulation + IT reconstruction"},
  {"vmcgIT"   ,"VMC,Y2004,FieldON,MakeEvent,tfs,tcl,tpcI,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem",0,""},
  {"vmctrsc","VMC,Y2004,FieldON,MakeEvent,tfs,tpc,Physics,Cdst,tags,Tree,EvOut,McEvOut,miniMcMk,StarMagField",
   "/star/data07/calib/fisyak/evgen/4prong.root",""},
  {"vmctfsc","VMC,Y2004,FieldON,MakeEvent,tfs,tpc,Physics,Cdst,tags,Tree,EvOut,McEvOut,miniMcMk,StarMagField",
   "/star/data07/calib/fisyak/evgen/4prong.root",""},
  {"vmc","VMC,Y2004,FieldON,StarMagField,McEvOut",0,"Simulation only"},
  {"vmcnt","VMC,Y2004x,FieldON,debug,phys_off,StarMagField,McEvOut","/star/simu/simu/gstardata/evgenRoot/evgen.3.root",""},
  {"vmcnt4","VMC,Y2004x,FieldON,debug,phys_off,StarMagField,McEvOut","/star/data07/calib/fisyak/evgen/4prong.root",""},
  {"ntin","ntin,y2004x,phys_off,paw,StarMagField,McEvOut","/star/simu/evgen/dau200/hijing_382/b0_20/minbias/evgen.3.nt",""},
  {"ntin4","ntin,y2004x,phys_off,paw,StarMagField,McEvOut","/star/data07/calib/fisyak/evgen/4prong.nt",""},
  // Test chains
  {"----","nightly test (dev) chains",0,""},
  {"year_1h_central"	,"p2000","/star/rcf/test/daq/2000/09/st_physics_1248022_raw_0001.daq",""},
  {"year_1h_hc_standard"	
   ,"trs,mdc3,v0,xi,big,EvOut,McEvOut,-dstout,fzin",
   "/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz",""},
  {"year_1h_minbias"	,"p2000","/star/rcf/test/daq/2000/08/st_physics_1229021_raw_0003.daq",""},
  {"year_2001_central"	,"P2001a,v0,xi,ZDCvtx,-dstout,CMuDst","/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq",""},
  {"year_2001_hc_highdensity"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,McOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/hadronic_on/Gstardata/hc_highdensity.16_evts.fz",""},
  {"year_2001_hc_lowdensity"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/lowdensity/year2001/hadronic_on/Gstardata/hc_lowdensity.400_evts.fz",""},
  {"year_2001_hc_standard"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/standard/year2001/hadronic_on/Gstardata/hc_standard.40_evts.fz",""},
  {"year_2001_minbias"	,"P2001a,v0,xi,ZDCvtx,-dstout,CMuDst","/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq",""},
  //    {"year_2001_ppl_minbias"	,"ppMDC4,v0,xi,fss","pds0200_04_12812evts.fzd; gfile B pds0200_01_28950evts.fzd;  mode SVTT back  22122; mode FTPC back 000; mode TPCE back 1881188; gback 188 188 0.0213 213. 2.5 ",""},
  {"year_2001_pp_minbias"	,"trs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/gstardata/pds0200_04_12812evts.fzd",""},
  {"year_2001_ppMinBias"	,"pp2001a,v0,xi,fpd,beamLine,est,-dstout,CMuDst","/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq",""},
  {"year_2003_dAuMinBiasIT"	,"DbV20040520,dau2003i,ITTF,SvtIT,-dstout,in","/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",""},
  {"year_2003_dAuMinBias"	,"DbV20040520,dau2003,v0,xi,l3onl,est,beamLine,-dstout,CMuDst","/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",""},
  {"year_2003_dau_minbias"	,"dAuMDCa,v0,xi,tofsim,Eefs,beamLine,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1197_05_5940evts.fzd",""},
  {"year_2003_ppMinBias"	,"pp2003,v0,xi,l3onl,beamLine,est,eemcD,-dstout,CMuDst","/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq",""},
  {"year_2004_AuAuMinBiasIT"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",""},
  {"year_2004_AuAuMinBias"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",""},
  {"year_2004_auau_minbiasIT"	,"trs,srs,fss,y2004,Idst,l0,tpcI,fcf,ftpc,Tree,SvtCL,svtDb,ITTF,Sti,genvtx,geant,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,McEvOut,big,fzin,MiniMcMk,SvtIt,clearmem","/star/rcf/simu/rcf1207_01_225evts.fzd",""},
  {"year_2004_auau_minbias"	,"trs,srs,fss,y2004,tpc,l0,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,EvOut,McEvOut,EventQA,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1207_01_225evts.fzd",""},
  {"year_2004_auau_minbias"	,"trs,SvtSlowSim,SvtD,fss,y2004,tpc,l0,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,EvOut,McEvOut,EventQA,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1207_01_225evts.fzd",""},
  {"year_2004_AuAu_prodHighIT"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",""},
  {"year_2004_AuAu_prodHigh"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",""},
  {"year_2004_AuAu_prodLowIT"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",""},
  {"year_2004_AuAu_prodLow"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",""},
  {"year_2004_prodPPIT"	        ,"pp2004,DbV20050312,beamLine,ITTF,-dstout","/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",""},
  {"year_2004_prodPP"	        ,"pp2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,beamLine,pmdRaw,CMuDst","/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",""},
  {"year_2005_CuCu200_HighTowerIT"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq",""},
  {"year_2005_CuCu200_HighTower"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq",""},
  {"year_2005_CuCu200_MinBiasIT"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq",""},
  {"year_2005_CuCu200_MinBias"	        ,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2",
                                                                                  "/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq",""},
  {"year_2005_CuCu22_MinBiasIT"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq",""},
  {"year_2005_CuCu22_MinBias"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq",""},
  {"year_2005_CuCu62_MinBiasIT"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq",""},
  {"year_2005_CuCu62_MinBias"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq",""}
};
Int_t NChains = sizeof(Chains)/sizeof(Chain_t);
//_________________ Prototypes _______________________________________________
void Usage();
void Load(const Char_t *options="");
void bfc(Int_t First, Int_t Last,
	 const Char_t *Chain="gstar,Y2004,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,miniMcMk,clearmem,StarMagField,FieldOn",	 
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
void bfc(Int_t Last, 
	 const Char_t *Chain="gstar,Y2004,tpcDb,trs,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,miniMcMk,StarMagField,FieldOn", 
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
void bfc (const Char_t *ChainShort="",Int_t  Last=1, const Char_t *infile=0, const Char_t *outfile=0, 
	  const Char_t *TreeFile=0);
//_____________________________________________________________________
void Load(const Char_t *options){
  cout << "Load system libraries" << endl;
  if (!TString(options).Contains("nodefault",TString::kIgnoreCase)) {
    if ( gClassTable->GetID("TGiant3") < 0) { // ! root4star
      cout << endl << "Load ";
      const Char_t *pgf77 = "libpgf77VMC";
      if (gSystem->DynamicPathName(pgf77,kTRUE) ) {
	gSystem->Load(pgf77); cout << " " << pgf77 << " + ";
      }
      gSystem->Load("libminicern"); cout << "libminicern";
      Char_t *mysql = "libmysqlclient";
      Char_t *libs[]  = {"", "/usr/mysql/lib/","/usr/lib/", 0};
      Int_t i = 0;
      while ((libs[i])) {
	TString lib(libs[i]);
	lib += mysql;
	if (gSystem->DynamicPathName(lib,kTRUE)) {
	  gSystem->Load(lib.Data()); cout << " + " << lib.Data() << endl;
	  break;
	}
	i++;
      }
    }
  }
  if (gClassTable->GetID("TTable")  < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("TRArray") < 0) gSystem->Load("StarRoot");//  TMemStat::PrintMem("load StarRoot");
  // Look up for the logger option
  Bool_t needLogger  = kFALSE;
  if (!TString(options).Contains("-logger",TString::kIgnoreCase)) {
    needLogger = kTRUE;              //  TMemStat::PrintMem("load log4cxx");
  }
  gSystem->Load("libSt_base");                                        //  TMemStat::PrintMem("load St_base");
  if (needLogger) {
    gSystem->Load("libStStarLogger.so");
    gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");      //  TMemStat::PrintMem("load StStarLogger");
  }
  gSystem->Load("libStChain");                                        //  TMemStat::PrintMem("load StChain");
  gSystem->Load("libStUtilities");                                    //  TMemStat::PrintMem("load StUtilities");
  gSystem->Load("libStBFChain");                                      //  TMemStat::PrintMem("load StBFChain");
  gSystem->Load("libStChallenger");                                   //  TMemStat::PrintMem("load StChallenger");
}
//_____________________________________________________________________
void bfc(Int_t First, Int_t Last,
	 const Char_t *Chain,
	 const Char_t *infile,
	 const Char_t *outfile,
	 const Char_t *TreeFile)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load(Chain);
  chain = new StBFChain(); cout << "Create chain " << chain->GetName() << endl;
  chain->SetDebug(1);
  if (Last < -3) return;
  chain->SetFlags(Chain);
  chain->Set_IO_Files(infile,outfile);
  if (TreeFile) chain->SetTFile(new TFile(TreeFile,"RECREATE"));
  gMessMgr->QAInfo() << Form("Process [First=%6i/Last=%6i/Total=%6i] Events",First,Last,Last-First+1) << endm;
  if (Last < -2) return;
  if (chain->Load() > kStOk) {
    gMessMgr->Error() << "Problems with loading of shared library(ies)" << endm;
    gSystem->Exit(1);
  }
  if (Last < -1) return;
  if (chain->Instantiate() > kStOk)  { 
    gMessMgr->Error() << "Problems with instantiation of Maker(s)" << endm;
    gSystem->Exit(1);
  }
#if 0
  // Insert your maker before "tpc_hits"
  Char_t *myMaker = "St_TLA_Maker";
  if (gClassTable->GetID(myMaker) < 0) {
	  gSystem->Load(myMaker);//  TString ts("load "; ts+=myMaker; TMemStat::PrintMem(ts.Data());
  }
  StMaker *myMk = chain->GetMaker(myMaker);
  if (myMk) delete myMk;
  myMk = chain->New(myMaker,"before");
  if (myMk) {
    Char_t *before = "tpc_hits";
    StMaker *tclmk = chain->GetMaker(before);
    if (tclmk) chain->AddBefore(before,myMk);
  }
  // Insert your maker after "tpc_hits"
  myMk = chain->New(myMaker,"after");
  if (myMk) {
    Char_t *after = "tpc_hits";
    StMaker *tclmk = chain->GetMaker(after);
    if (tclmk) chain->AddAfter(after,myMk);
  }
  // this block is meant as an example ONLY
  // The default values are set in StRoot/StPass0CalibMaker/ StTpcT0Maker 
  // constructor and are suitable for production. You can change it here
  // for test purposes.
  if (chain->GetOption("TpcT0")) {
    StTpcT0Maker *t0mk = (StTpcT0Maker *) chain->GetMaker("TpcT0");
    if (t0mk) t0mk->SetDesiredEntries(10);
  }
#endif
  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i\n",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", chain->GetCVS()) << endm;
  // Init the chain and all its makers
  Int_t iTotal = 0, iBad = 0;

  chain->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  chain->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker

  if (Last < 0) return;
  Int_t iInit = chain->Init();
  if (iInit >=  kStEOF) {chain->FatalErr(iInit,"on init"); return;}
  if (Last == 0) return;
  StEvtHddr *hd = (StEvtHddr*)chain->GetDataSet("EvtHddr");
  if (hd) hd->SetRunNumber(-2); // to be sure that InitRun calls at least once
    // skip if any
  chain->Skip(First-1);
  StMaker *treeMk = chain->GetMaker("outputStream");
  chain->EventLoop(First,Last,treeMk);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is finished at Date/Time %i/%i; Total events processed :%i and not completed: %i",
			       t.GetDate(),t.GetTime(),iTotal,iBad) << endm;
  }
}
//_____________________________________________________________________
void bfc(Int_t Last, 
	 const Char_t *Chain,
	 const Char_t *infile, 
	 const Char_t *outfile, 
	 const Char_t *TreeFile)
{
  bfc(1,Last,Chain,infile,outfile,TreeFile);
}
//_____________________________________________________________________
void bfc (const Char_t *ChainShort,
	  Int_t  Last,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile) {
  TString theChain(ChainShort);
  if (theChain == "") {
    Usage(); 
    cout << ("============= \t Examples =============\n");
    for (Int_t i = 0; i < NChains; i++) {
      TString aChain(Chains[i].Name);
      if (aChain == "----") {
	cout <<" ====================== " << Chains[i].Opts << " ====================== " << endl;
      } else {
	cout << "bfc.C(\"" << Chains[i].Name 
	     << "\",Last,input,output,TreeFile) =>" << Chains[i].Comment << "\t[Default] " << endl;
	cout << "\tbfc.C(1,\"" << Chains[i].Opts;
	if (Chains[i].Input)   cout << "\"," << Chains[i].Input;// << "[D]";
	cout << "\")" << endl;
      }
    }  
    gSystem->Exit(1);
  }
  theChain.ToLower();

  for (Int_t i = 0; i < NChains; i++) {
    TString name(Chains[i].Name);
    name.ToLower();
    if (name == theChain) {
      Char_t *inp = (Char_t *) infile;
      if (inp == 0) inp = Chains[i].Input;
      bfc(Last,Chains[i].Opts,inp,outfile,TreeFile);
      return;
    }
  }
}

//____________________________________________________________
void Usage() {
  Char_t *path  = "./StRoot/StBFChain:$STAR/StRoot/StBFChain";
  Char_t *rootf = "BigFullChain.h";
  Char_t *file = gSystem->Which(path,rootf,kReadPermission);
  if (file) {
    printf ("============= \tBigFullChain options  =============\n");
    TString cmd("cat ");
    cmd += file;
    gSystem->Exec(cmd);
  }
  printf ("============= \t U S A G E =============\n");
  printf ("bfc(Int_t First,Int_t Last,const Char_t *Chain,const Char_t *infile,const Char_t *outfile,const Char_t *TreeFile)\n");
  printf ("bfc(Int_t Last,const Char_t *Chain,const Char_t *infile,const Char_t *outfile,const Char_t *TreeFile)\n");
  printf ("bfc(const Char_t *ChainShort,Int_t Last,const Char_t *infile,const Char_t *outfile)\n");
  printf ("where\n");
  printf (" First     \t- First event to process\t(Default = 1)\n");
  printf (" Last      \t- Last  event to process\t(Default = 1)\n");
  printf (" Chain     \t- Chain specification   \t(without First &  Last: Default is \"\" which gives this message)\n");
  printf ("           \t                        \t with    First || Last: Default is \"gstar tfs\")\n");
  printf (" infile    \t- Name of Input file    \t(Default = 0, i.e. use preset file names depending on Chain)\n"); 
  printf (" outfile   \t- Name of Output file   \t(Default = 0, i.e. define Output file name from Input one)\n");
  printf (" outfile   \t- Name of Tree File     \t(Default = 0, i.e. define Output file name from Input one (tags TNtuple))\n");
  printf (" ChainShort\t- Short cut for chain   \t(Default = \"\" -> print out of this message)\n");
}
