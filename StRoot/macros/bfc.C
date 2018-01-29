//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
// Modifications by J. Lauret, V, Prevotchikov, G.V. Buren, L. Didenko  //
//                  and V. Fine                                         //
//                                                                      //
// $Id: bfc.C,v 1.193 2018/01/29 20:18:12 smirnovd Exp $
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
#include "TROOT.h"
#endif
StBFChain    *chain=0; 
//_____________________________________________________________________
//_________________ Prototypes _______________________________________________
void Usage();
void Load(const Char_t *options="");
//TString defChain("y2010,gstar,Test.default.Fast.ITTF,NosvtIT,NossdIT,-sfs,-ssdFast");
TString defChain("y2010,gstar,Test.default.ITTF,NosvtIT,NossdIT,-sfs,-ssdFast,sdt20100107.110000");
void bfc(Int_t First, Int_t Last,const Char_t *Chain = defChain + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
//	 const Char_t *Chain="gstar,y2005h,MakeEvent,trs,sss,svt,ssd,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,ssdDb,svtIT,ssdIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,IdTruth,miniMcMk,StarMagField,FieldOn,McAna,Display",//,,NoSimuDb, display, //McQa, 
void bfc(Int_t Last, const Char_t *Chain = defChain,
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
	 //	 const Char_t *Chain="gstar,y2005h,tpcDb,trs,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,IdTruth,miniMcMk,StarMagField,FieldOn,McAna", // McQA
//_____________________________________________________________________
void Load(const Char_t *options)
{
  cout << "Load system libraries\t";
  int nodefault = TString(options).Contains("nodefault",TString::kIgnoreCase);


  if ( TString(gProgName)!="root4star") { // ! root4star
    if (!nodefault || TString(options).Contains("pgf77",TString::kIgnoreCase)) {
      const Char_t *pgf77 = "libpgf77VMC";
      if (gSystem->DynamicPathName(pgf77,kTRUE) ) {
	gSystem->Load(pgf77); cout << " " << pgf77 << " + ";
      }
    }
    if (!nodefault || TString(options).Contains("cern" ,TString::kIgnoreCase)) {
        gSystem->Load("libStarMiniCern"); 
        cout << "libStarMiniCern" ;
    }

    
    if (!nodefault || TString(options).Contains("mysql",TString::kIgnoreCase)) {
      Char_t *mysql = "libmysqlclient";
      //Char_t *mysql = "libmimerS"; // just to test it picks from OPTSTAR

      //
      // May use USE_64BITS - the x8664 work fine too
      //
      Char_t *libsLocal[]= {"",
	                    "$OPTSTAR/lib/",
			    "$OPTSTAR/lib/mysql/",
			    "/usr/lib/", 
			    "/usr/lib/mysql/", 
			    "/usr/mysql/lib/",
			    NULL}; 
      Char_t *libsGlbal[]= {"", 
			    "/usr/lib/", 
			    "/usr/lib/mysql/", 
			    "/usr/mysql/lib/",
			    "$OPTSTAR/lib/",
			    "$OPTSTAR/lib/mysql/",
			    NULL}; 

      Char_t **libs;

      if ( gSystem->Getenv("USE_LOCAL_MYSQL") ){
	libs = libsLocal;
      } else {
	libs = libsGlbal;
      }


      TString Arch( gSystem->GetBuildArch() );
      Bool_t i64 = kFALSE;
      if ( gSystem->Getenv("USE_64BITS")==1 || Arch.Contains("x8664")) i64 = kTRUE;

      Int_t i = 0;
      while ((libs[i])) {
	TString lib(libs[i]);
	//cout << "Found " << lib << endl;
	if (i64) lib.ReplaceAll("/lib","/lib64");
	lib += mysql;
	lib = gSystem->ExpandPathName(lib.Data());
	if (gSystem->DynamicPathName(lib,kTRUE)) {
	  gSystem->Load(lib.Data()); 
	  cout << " + " << mysql << " from " << lib.Data();
	  break;
	}
	i++;
      }
    }
    cout << endl;
  }
  gSystem->Load("libSt_base");                                        //  StMemStat::PrintMem("load St_base");
  // Look up for the logger option
  Bool_t needLogger  = kFALSE;
  if (gSystem->Load("liblog4cxx.so") >=  0) {             //  StMemStat::PrintMem("load log4cxx");
    cout << " + liblog4cxx.so";
    if(gSystem->Load("libStStarLogger.so") >= 0) {              //  StMemStat::PrintMem("load log4cxx");
      cout << " + libStStarLogger.so";
      gROOT->ProcessLine("StLoggerManager::StarLoggerInit();"); 
    }
  }
  //  gSystem->Load("libHtml");
  gSystem->Load("libStChain");                                        //  StMemStat::PrintMem("load StChain");
  gSystem->Load("libStUtilities");                                    //  StMemStat::PrintMem("load StUtilities");
  gSystem->Load("libStBFChain");                                      //  StMemStat::PrintMem("load StBFChain");
  cout << endl;
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
  TString tChain(Chain);
  chain->cd();
  chain->SetDebug(1);
  if (Last < -3) return;
  chain->SetFlags(Chain);
  if (tChain == "" || ! tChain.CompareTo("ittf",TString::kIgnoreCase)) Usage();
  gMessMgr->QAInfo() << Form("Process [First=%6i/Last=%6i/Total=%6i] Events",First,Last,Last-First+1) << endm;
  if (Last < -2) return;
  if (chain->Load() > kStOk) {
    gMessMgr->Error() << "Problems with loading of shared library(ies)" << endm;
    gSystem->Exit(1);
  }
  chain->Set_IO_Files(infile,outfile);
  if (TreeFile) chain->SetTFile(new TFile(TreeFile,"RECREATE"));

  if (Last < -1) return;
  if (chain->Instantiate() > kStOk)  { 
    gMessMgr->Error() << "Problems with instantiation of Maker(s)" << endm;
    gSystem->Exit(1);
  }
  StMaker::lsMakers(chain);
  if (Last < 0) return;
  StMaker *dbMk = chain->GetMaker("db");
  if (dbMk) dbMk->SetDebug(1);
#if 0
  // Insert your maker before "tpc_hits"
  Char_t *myMaker = "St_TLA_Maker";
  if (gClassTable->GetID(myMaker) < 0) {
	  gSystem->Load(myMaker);//  TString ts("load "; ts+=myMaker; StMemStat::PrintMem(ts.Data());
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
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", chain->GetCVS()) << endm;
  // Init the chain and all its makers
  TAttr::SetDebug(0);
  chain->SetAttr(".Privilege",0,"*"                ); 	  //All  makers are NOT priviliged
  chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	  //All IO makers are priviliged
  chain->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	  //It is also IO maker
  chain->SetAttr(".Privilege",1,"StTpcDbMaker::*"); 	  //It is also TpcDb maker to catch trips
  chain->SetAttr(".Privilege",1,"*::tpc_hits"); //May be allowed to act upon excessive events
  chain->SetAttr(".Privilege",1,"*::tpx_hits"); //May be allowed to act upon excessive events
  chain->SetAttr(".Privilege",1,"StTpcHitMover::*"); //May be allowed to act upon corrupt events
  chain->SetAttr(".Privilege",1,"*::tpcChain"); //May pass on messages from sub-makers
  chain->SetAttr(".Privilege",1,"StTriggerDataMaker::*"); //TriggerData could reject event based on corrupt triggers
  chain->SetAttr(".Privilege",1,"StEventMaker::*"); //May be allowed to act upon trigger IDs (filtering)
  Int_t iInit = chain->Init();
  if (iInit >=  kStEOF) {chain->FatalErr(iInit,"on init"); return;}
  if (Last == 0) return;
  StEvtHddr *hd = (StEvtHddr*)chain->GetDataSet("EvtHddr");
  if (hd) hd->SetRunNumber(-2); // to be sure that InitRun calls at least once
    // skip if any
  chain->EventLoop(First,Last,0);
  gMessMgr->QAInfo() << "Run completed " << endm;
}
//_____________________________________________________________________
void bfc(Int_t Last, 
	 const Char_t *Chain,
	 const Char_t *infile, 
	 const Char_t *outfile, 
	 const Char_t *TreeFile) {
  bfc(1,Last,Chain,infile,outfile,TreeFile);
}
//____________________________________________________________
void Usage() {
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
  gSystem->Exit(1);
}
//_____________________________________________________________________
void bfc(const Char_t *Chain="ittf") {
  bfc(-2,Chain);
}
