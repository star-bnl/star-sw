//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.168 2007/06/06 04:05:55 perev Exp $
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
#else
#endif
#define UseLogger
StBFChain    *chain=0; 
//_____________________________________________________________________
//_________________ Prototypes _______________________________________________
void Usage();
void Load(const Char_t *options="");
TString defChain("y2005e,Test.default.ITTF");
void bfc(Int_t First, Int_t Last,const Char_t *Chain = defChain + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
//	 const Char_t *Chain="gstar,y2005e,MakeEvent,trs,sss,svt,ssd,fss,bbcSim,emcY2,tpcI,fcf,ftpc,SvtCL,svtDb,ssdDb,svtIT,ssdIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,McEvOut,GeantOut,IdTruth,miniMcMk,StarMagField,FieldOn,McAna,Display",//,,NoSimuDb, display, //McQa, 
void bfc(Int_t Last, const Char_t *Chain = defChain,
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0);
	 //	 const Char_t *Chain="gstar,y2005e,tpcDb,trs,tpc,Physics,Cdst,Kalman,tags,Tree,EvOut,McEvOut,IdTruth,miniMcMk,StarMagField,FieldOn,McAna", // McQA
//_____________________________________________________________________
void Load(const Char_t *options){
  cout << "Load system libraries" << endl;
  if ( gClassTable->GetID("TGiant3") < 0) { // ! root4star
    cout << endl << "Load ";
    if (!TString(options).Contains("nodefault",TString::kIgnoreCase) || 
	 TString(options).Contains("pgf77",TString::kIgnoreCase)) {
      const Char_t *pgf77 = "libpgf77VMC";
      if (gSystem->DynamicPathName(pgf77,kTRUE) ) {
	gSystem->Load(pgf77); cout << " " << pgf77 << " + ";
      }
    }
    if (!TString(options).Contains("nodefault",TString::kIgnoreCase) || 
	TString(options).Contains("cern",TString::kIgnoreCase)) {
      gSystem->Load("libminicern"); cout << "libminicern";
    }
    if (!TString(options).Contains("nodefault",TString::kIgnoreCase) || 
	TString(options).Contains("mysql",TString::kIgnoreCase)) {
      Char_t *mysql = "libmysqlclient";
      Char_t *libs[]  = {"", "/usr/mysql/lib/","/usr/lib/", 0}; // "$ROOTSYS/mysql-4.1.20/lib/",
      //Char_t *libs[]  = {"/usr/lib/", 0};
      Int_t i = 0;
      while ((libs[i])) {
	TString lib(libs[i]);
	lib += mysql;
	lib = gSystem->ExpandPathName(lib.Data());
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
#ifdef UseLogger
  // Look up for the logger option
  Bool_t needLogger  = kFALSE;
  if (!TString(options).Contains("-logger",TString::kIgnoreCase)) {
    needLogger = gSystem->Load("liblog4cxx.so") <= 0;              //  TMemStat::PrintMem("load log4cxx");
  }
#endif
  gSystem->Load("libSt_base");                                        //  TMemStat::PrintMem("load St_base");
#ifdef UseLogger
  if (needLogger) {
    gSystem->Load("libStStarLogger.so");
    gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");      //  TMemStat::PrintMem("load StStarLogger");
  }
#endif
  gSystem->Load("libHtml");
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
  StMaker *dbMk = chain->GetMaker("db");
  if (dbMk) dbMk->SetDebug(1);
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
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
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
  chain->EventLoop(First,Last,0);
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
//_____________________________________________________________________
void bfc() {
  Usage(); 
  gSystem->Exit(1);
}
