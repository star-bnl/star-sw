//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.165 2005/08/29 22:44:14 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#else 
class StBFChain;        
#endif
StBFChain    *chain=0; 
void Usage();
//_____________________________________________________________________
void Load(const Char_t *options=0){
  cout << "Load system libraries" << endl;
  if ( gClassTable->GetID("TGiant3") < 0) { // ! root4star
    cout << endl << "Load ";
    if (gSystem->Getenv("PGI")) {
      gSystem->Load("libpgf77VMC"); cout << " libpgf77VMC + ";
    }
    gSystem->Load("libminicern"); cout << "libminicern";
    gSystem->Load("/usr/lib/libmysqlclient.so"); cout << " + /usr/lib/libmysqlclient.so" << endl;
  }
  if (gClassTable->GetID("TTable")  < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("TRArray") < 0) gSystem->Load("StarRoot");//  TMemStat::PrintMem("load StarRoot");
  // Look up for the logger option
  Bool_t needLogger  = kFALSE;
  if (!TString(options).Contains("-logger")) {
      needLogger = gSystem->Load("liblog4cxx.so"); 
      if (!needLogger) {
	needLogger = kTRUE;//         TMemStat::PrintMem("load log4cxx");
      }
      else {
         fprintf(stderr," Could not load log4cxx shared library\n");         
      }
  }
  gSystem->Load("St_base");//  TMemStat::PrintMem("load St_base");
  if (needLogger) {
    gSystem->Load("StStarLogger.so");
    //    gInterpreter->ProcessLine("StLoggerManager::StarLoggerInit();");//    TMemStat::PrintMem("load StStarLogger");
    gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");//    TMemStat::PrintMem("load StStarLogger");
 }
  gSystem->Load("StChain");//  TMemStat::PrintMem("load StChain");
  gSystem->Load("StUtilities");//  TMemStat::PrintMem("load StUtilities");
  gSystem->Load("StBFChain");//  TMemStat::PrintMem("load StBFChain");
  gSystem->Load("StChallenger");//  TMemStat::PrintMem("load StChallenger");
}
//_____________________________________________________________________
void bfc(const Int_t First, const Int_t Last,
	 const Char_t *Chain="gstar,Y2004,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem,StarMagField,FieldOn",
	 const Char_t *infile=0,
	 const Char_t *outfile=0,
	 const Char_t *TreeFile=0)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load(Chain);
  chain = new StBFChain();
  if (!chain) gSystem->Exit(1);
  chain->SetDebug(1);
  chain->SetFlags(Chain);
  chain->Set_IO_Files(infile,outfile);
  if (TreeFile) chain->SetTFile(new TFile(TreeFile,"RECREATE"));
  printf ("QAInfo:Process [First=%6i/Last=%6i/Total=%6i] Events\n",First,Last,Last-First+1);
  if (Last < -2) return;
  if (chain->Load() > kStOk) 
    {printf("Error:Problems with loading of shared library(ies)\n"); gSystem->Exit(1);}
  if (Last < -1) return;
  if (chain->Instantiate() > kStOk) 
    {printf("Error:Problems with instantiation of Maker(s)\n"); gSystem->Exit(1);}
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
    printf ("QAInfo:Run is started at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
  }
  printf ("QAInfo:Run on %s in %s\n",
	  gSystem->HostName(),
	  gSystem->WorkingDirectory());
  printf ("QAInfo: with %s\n", chain->GetCVS());
  
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
  printf ("QAInfo:Run completed ");
  gSystem->Exec("date");
  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time %i/%i; Total events processed :%i and not completed: %i\n",
	    t.GetDate(),t.GetTime(),iTotal,iBad);
  }
}
//_____________________________________________________________________
void bfc(const Int_t Last, 
	  //	  const Char_t *Chain="gstar,Y2004,tfs,srs,SvtSlowSim,tpc,Physics,Cdst,Kalman,tags,Tree,evout,miniMcMk,StarMagField,FieldOn",
	  const Char_t *Chain="gstar,Y2004,tpcDb,trs,tpc,Physics,Cdst,Kalman,tags,Tree,evout,miniMcMk,StarMagField,FieldOn",
	  const Char_t *infile=0, 
	  const Char_t *outfile=0, 
	  const Char_t *TreeFile=0)
{
  bfc(1,Last,Chain,infile,outfile,TreeFile);
}
//_____________________________________________________________________
void bfc (const Char_t *ChainShort="",
	  const Int_t  Last=1,
	  const Char_t *infile=0, 
	  const Char_t *outfile=0, 
	  const Char_t *TreeFile=0) {
  struct Chain_t {
    Char_t *Name;
    Char_t *Opts;
    Char_t *Input;
  };
  const Chain_t Chains[] = {
    // tpc only 
    {"trsz","fzin,Y2004,trs,tpc,Physics,Cdst,Kalman,tags,Tree,evout,miniMcMk",0},
    {"trsc" ,"gstar,Y2004,trs,SvtSlowSim,tpc,Physics,Cdst,Kalman,tags,Tree,evout,miniMcMk",0}, // root4star
    {"tfsc" ,"gstar,Y2004,tfs,SvtSlowSim,tpc,Physics,Cdst,Kalman,tags,Tree,evout,miniMcMk",0}, // root4star
    {"trsic",
     "gstar,Y2004,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem",0},  // root4star
    {"trsic","gstar,Y2004,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem",0}, // root4star
    {"vmcc","VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem,StarMagField,debug",0},  // root.exe
    {"vmcctfs","VMC,Y2004,FieldON,MakeEvent,tfs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem,StarMagField,debug",0},  // root.exe
    {"vmcct","VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem,StarMagField,debug","/star/simu/simu/gstardata/evgenRoot/evgen.3.root"},
    {"vmcc4","VMC,Y2004,FieldON,MakeEvent,trs,srs,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,svtIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem,StarMagField,debug","/star/data07/calib/fisyak/evgen/4prong.nt"},
    {"vmcg","VMC,Y2004,FieldON,MakeEvent,tfs,tcl,tpcI,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,evout,GeantOut,miniMcMk,clearmem",0},
    {"vmctrsc","VMC,Y2004,FieldON,MakeEvent,tfs,tpc,Physics,Cdst,tags,Tree,evout,miniMcMk,StarMagField",
     "/star/data07/calib/fisyak/evgen/4prong.root"},
    {"vmctfsc","VMC,Y2004,FieldON,MakeEvent,tfs,tpc,Physics,Cdst,tags,Tree,evout,miniMcMk,StarMagField",
     "/star/data07/calib/fisyak/evgen/4prong.root"},
    {"vmc","VMC,Y2004,FieldON,StarMagField",0},
    {"vmcnt","VMC,Y2004x,FieldON,debug,phys_off,StarMagField","/star/simu/simu/gstardata/evgenRoot/evgen.3.root"},
    {"vmcnt4","VMC,Y2004x,FieldON,debug,phys_off,StarMagField","/star/data07/calib/fisyak/evgen/4prong.root"},
    {"ntin","ntin,y2004x,phys_off,paw,StarMagField","/star/simu/evgen/dau200/hijing_382/b0_20/minbias/evgen.3.nt"},
    {"ntin4","ntin,y2004x,phys_off,paw,StarMagField","/star/data07/calib/fisyak/evgen/4prong.nt"},
    // Test chains
    {"year_1h_central"	,"p2000","/star/rcf/test/daq/2000/09/st_physics_1248022_raw_0001.daq"},
    {"year_1h_hc_standard"	
     ,"trs,mdc3,v0,xi,big,evout,-dstout,fzin",
     "/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz"},
    {"year_1h_minbias"	,"p2000","/star/rcf/test/daq/2000/08/st_physics_1229021_raw_0003.daq"},
    {"year_2001_central"	,"P2001a,v0,xi,ZDCvtx,-dstout,CMuDst","/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq"},
    {"year_2001_hc_highdensity"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/hadronic_on/Gstardata/hc_highdensity.16_evts.fz"},
    {"year_2001_hc_lowdensity"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/lowdensity/year2001/hadronic_on/Gstardata/hc_lowdensity.400_evts.fz"},
    {"year_2001_hc_standard"	,"trs,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/cocktail/hadronic/default/standard/year2001/hadronic_on/Gstardata/hc_standard.40_evts.fz"},
    {"year_2001_minbias"	,"P2001a,v0,xi,ZDCvtx,-dstout,CMuDst","/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq"},
    //    {"year_2001_ppl_minbias"	,"ppMDC4,v0,xi,fss","pds0200_04_12812evts.fzd; gfile B pds0200_01_28950evts.fzd;  mode SVTT back  22122; mode FTPC back 000; mode TPCE back 1881188; gback 188 188 0.0213 213. 2.5 "},
    {"year_2001_pp_minbias"	,"trs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDst,big,fzin","/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/gstardata/pds0200_04_12812evts.fzd"},
    {"year_2001_ppMinBias"	,"pp2001a,v0,xi,fpd,beamLine,est,-dstout,CMuDst","/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq"},
    {"year_2003_dAuMinBias"	,"DbV20040520,dau2003i,ITTF,SvtIT,-dstout,in","/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq"},
    {"year_2003_dAuMinBias"	,"DbV20040520, dau2003,v0,xi,l3onl,est,beamLine,-dstout,CMuDst","/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq"},
    {"year_2003_dAuMinBias"	,"DbV20040520,dau2003,v0,xi,l3onl,est,beamLine,-dstout,CMuDst","/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq"},
    {"year_2003_dau_minbias"	,"dAuMDCa,v0,xi,tofsim,Eefs,beamLine,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1197_05_5940evts.fzd"},
    {"year_2003_ppMinBias"	,"pp2003,v0,xi,l3onl,beamLine,est,eemcD,-dstout,CMuDst","/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq"},
    {"year_2004_AuAuMinBias"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq"},
    {"year_2004_AuAuMinBias"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq"},
    {"year_2004_auau_minbias"	,"trs,srs,fss,y2004,Idst,l0,tpcI,fcf,ftpc,Tree,SvtCL,svtDb,ITTF,Sti,genvtx,geant,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,fzin,MiniMcMk,SvtIt,clearmem","/star/rcf/simu/rcf1207_01_225evts.fzd"},
    {"year_2004_auau_minbias"	,"trs,srs,fss,y2004,tpc,l0,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,evout,EventQA,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1207_01_225evts.fzd"},
    {"year_2004_auau_minbias"	,"trs,SvtSlowSim,SvtD,fss,y2004,tpc,l0,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,evout,EventQA,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin","/star/rcf/simu/rcf1207_01_225evts.fzd"},
    {"year_2004_AuAu_prodHigh"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq"},
    {"year_2004_AuAu_prodHigh"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq"},
    {"year_2004_AuAu_prodLow"	,"P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst","/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq"},
    {"year_2004_AuAu_prodLow"	,"P2004,DbV20050312,ITTF,-dstout,pmdRaw","/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq"},
    {"year_2004_prodPP"	,"pp2004,DbV20050312,beamLine,ITTF,-dstout","/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq"},
    {"year_2004_prodPP"	,"pp2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,beamLine,pmdRaw,CMuDst","/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq"},
    {"year_2005_CuCu200_HighTower"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq"},
    {"year_2005_CuCu200_HighTower"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq"},
    {"year_2005_CuCu200_MinBias"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq"},
    {"year_2005_CuCu200_MinBias"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq"},
    {"year_2005_CuCu22_MinBias"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq"},
    {"year_2005_CuCu22_MinBias"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq"},
    {"year_2005_CuCu62_MinBias"	,"P2005,ToF,ITTF,pmdRaw,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq"},
    {"year_2005_CuCu62_MinBias"	,"P2005,ToF,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq"}
  };
  Int_t NChains = sizeof(Chains)/sizeof(Chain_t);
  TString Chain(ChainShort);
  if (Chain == "") {
    Usage(); 
    cout << ("============= \t Examples =============\n");
    for (Int_t i = 0; i < NChains; i++) {
      cout << "bfc(\"" << Chains[i].Name 
	   << "\",Last,input,output,TreeFile) => [Default] " << endl;
      cout << "\tbfc.C(1,\"" << Chains[i].Opts;
      if (Chains[i].Input)   cout << "\"," << Chains[i].Input;// << "[D]";
      cout << "\")" << endl;
    }  
    gSystem->Exit(1);
  }
  Chain.ToLower();

  for (Int_t i = 0; i < NChains; i++) {
    TString name(Chains[i].Name);
    name.ToLower();
    if (name == Chain) {
      Char_t *inp = (Char_t *) infile;
      if (inp == 0) inp = Chains[i].Input;
      bfc(Last,Chains[i].Opts,inp,outfile,TreeFile);
      return;
    }
  }
}

//____________________________________________________________
void Usage() {
#if 0
  printf ("============= \tImportant two changes:\n"
	  "              \tIt is required exact matching in Chain definition\n"
	  "              \tAll Chain options set in supplyed order\n"); 
  if (gClassTable->GetID("StBFChain") < 0) Load();
  chain = new StBFChain;
  chain->SetFlags("");
#endif
  printf ("============= \t U S A G E =============\n");
  printf ("bfc(Int_t First, Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile, Char_t *TreeFile)\n");
  printf ("bfc(Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile, Char_t *TreeFile)\n");
  printf ("bfc(Char_t *ChainShort, Int_t Last, Char_t *infile, Char_t *outfile)\n");
  printf ("where\n");
  printf (" First   \t- First event to process \t(Default = 1)\n");
  printf (" Last    \t- Last  event to process \t(Default = 1)\n");
  printf (" Chain   \t- Chain specification    \t(without First &  Last: Default is \"\" which gives this message)\n");
  printf ("         \t                         \t with    First || Last: Default is \"gstar tfs\")\n");
  printf (" infile  \t- Name of Input file     \t(Default = 0, i.e. use preset file names depending on Chain)\n"); 
  printf (" outfile \t- Name of Output file    \t(Default = 0, i.e. define Output file name from Input one)\n");
  printf ("ChainShort\t- Short cut for chain    \t(Default = "" -> print out of this message)\n");
}
