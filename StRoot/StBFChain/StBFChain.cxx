// $Id: StBFChain.cxx,v 1.32 1999/11/15 23:46:22 fisyak Exp $
// $Log: StBFChain.cxx,v $
// Revision 1.32  1999/11/15 23:46:22  fisyak
// Separate time stamps and chains
//
// Revision 1.31  1999/11/13 02:33:37  fisyak
// Add dependence of ems from geant, eliminate geant Cint macros
//
// Revision 1.30  1999/11/13 00:26:13  fisyak
// Add UserDb option for Cint files
//
// Revision 1.29  1999/11/11 16:29:06  fisyak
// Clean up FileOut treatment
//
// Revision 1.28  1999/11/11 01:54:37  fisyak
// Fix Trs name
//
// Revision 1.27  1999/11/10 20:45:03  fisyak
// Fix Xi maker, remove dependence tpc_daq->xin
//
// Revision 1.26  1999/11/10 16:15:20  fisyak
// One more bug fix due to infile
//
// Revision 1.25  1999/11/10 00:10:52  fisyak
// Valery's fix for fzin
//
// Revision 1.24  1999/11/09 01:51:08  fisyak
// Fix Files, introduce tables and tls as separated option
//
// Revision 1.23  1999/11/08 16:45:41  fisyak
// Fix typo in tpc chain
//
// Revision 1.22  1999/11/07 02:26:22  fisyak
// Clean ups
//
// Revision 1.21  1999/11/05 16:20:20  fisyak
// change tls -> libtls
//
// Revision 1.20  1999/11/04 22:21:25  fisyak
// Reorganize chain as Table
//
// Revision 1.18  1999/10/28 01:57:16  fisyak
// Add ParseString
//
// Revision 1.17  1999/10/16 20:20:19  fisyak
// Allow to have xdf file for gstar option
//
// Revision 1.16  1999/10/14 14:43:25  fisyak
// Ad casts
//
// Revision 1.15  1999/10/14 14:23:17  fisyak
// Take out MySQL from chain
//
// Revision 1.14  1999/10/12 23:13:30  fisyak
// Add AddBefore and AddAfter methods
//
// Revision 1.13  1999/09/24 14:50:46  fisyak
// Write and Close TFile if any
//
// Revision 1.12  1999/09/24 01:22:51  fisyak
// Reduced Include Path
//
// Revision 1.11  1999/09/18 00:56:33  fisyak
// remove .root
//
// Revision 1.10  1999/09/18 00:36:46  fisyak
// remove .root extension
//
// Revision 1.9  1999/09/17 22:57:17  fisyak
// Add l3t to default chain
//
// Revision 1.8  1999/09/17 22:09:24  fisyak
// Fix typo
//
// Revision 1.7  1999/09/17 21:27:44  fisyak
// Add MySQL
//
// Revision 1.6  1999/09/12 23:08:12  fisyak
// Add return value in Finish()
//
// Revision 1.5  1999/09/12 23:02:43  fisyak
// Add closing xdf and TFile
//
// Revision 1.4  1999/09/12 21:23:04  fisyak
// Add multiple input files to chain
//
// Revision 1.3  1999/09/08 00:14:06  fisyak
// Add kReverseField option
//
// Revision 1.2  1999/09/03 01:01:30  fisyak
// remove includes for St_[V0,tcl,tpt]_Makers
//
// Revision 1.1  1999/09/02 16:14:42  fine
// new StBFChain library has been introduced to break dependences
//
// Revision 1.11  1999/08/25 19:49:45  fisyak
// Add geant Input for IO
//
// Revision 1.10  1999/08/13 01:12:24  fine
// StMaker::GetHist has been introduced
//
// Revision 1.9  1999/08/10 17:15:10  fisyak
// Add xin == in
//
// Revision 1.8  1999/08/10 17:10:51  fisyak
// Exprot EChainOptions into rootcint
//
// Revision 1.7  1999/08/07 19:42:21  fisyak
// use default ctor for St_tpcdaq_Maker
//
// Revision 1.6  1999/08/06 18:57:31  fisyak
// Put MinidaqMaker after TPC
//
// Revision 1.5  1999/08/06 14:26:37  fisyak
// put back xdf out option
//
// Revision 1.4  1999/08/05 17:10:27  fisyak
// Change strstr to ==
//
// Revision 1.3  1999/08/04 16:28:59  fisyak
// remove ambiguity between XI and XIN
//
// Revision 1.2  1999/07/30 01:00:29  fisyak
// Work around Herb's default ctor
//
// Revision 1.1  1999/07/29 01:05:22  fisyak
// move bfc to StBFChain
//

#include "TROOT.h"
#include "TString.h"
#include "TRegexp.h"
#include "TSystem.h"
#include "StBFChain.h"
#include "StEvtHddr.h"
#include "St_DataSet.h"
#include "StChain.h"
#include "St_XDFFile.h"
#include "StMagF.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StDbBroker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StMessMgr.h"
//_____________________________________________________________________
struct BfcItem {
  Char_t       *Key;      // nick name
  Char_t       *Name;     // maker name
  Char_t       *Chain;    // its chain
  Char_t       *Opts;     // required options
  Char_t       *Maker;    // required Makers
  Char_t       *Libs;     // libraries to be loaded
  Char_t       *Comment;  
  Bool_t        Flag;     // F/T to use it in chain
};
BfcItem BFC[] = {
  {"Key"         ,"Name","Chain","Opts"                                   ,"Maker","Libs","Comment",kFALSE},
  // time stamps
  {"SD97"        ,""  ,"","db"                                ,"","","Turn on 1997 test parameters",kFALSE},
  {"SD98"        ,""  ,"","db"                                ,"","","Turn on 1998 test parameters",kFALSE},
  {"Y1a"         ,""  ,"","db"                                  ,"","","Turn on Year 1a parameters",kFALSE},
  {"Y1b"         ,""  ,"","db"                                          ,"","","Year 1b parameters",kFALSE},
  {"Y1c"         ,""  ,"","db"                                  ,"","","Turn on Year 1c parameters",kFALSE},
  {"ES99"        ,""  ,"","db"          ,"","","Turn on 1999 engineering run simulation parameters",kFALSE},
  {"ER99"        ,""  ,"","db"           ,"","","Turn on 1999 engineering run real data parameters",kFALSE},
  {"Y1d"         ,""  ,"","db"                                  ,"","","Turn on Year 1d parameters",kFALSE},
  {"Y1e"         ,""  ,"","db"                                  ,"","","Turn on Year 1e parameters",kFALSE},
  {"Y2a"         ,""  ,"","db"                                  ,"","","Turn on Year 2a parameters",kFALSE},
  // chains
  {"doEvents","" ,""  ,"xin,event,analysis,FiledOn"                                       ,"","","",kFALSE},
  {"Kalman"      ,""  ,"","geant"                                                         ,"","","",kFALSE},
  {"dst"         ,""  ,"","global,dst,qa,event,analysis"                                  ,"","","",kFALSE},
  {"Cy1a"        ,""  ,"","y1a,tpc,ftpc,l0,l3t,dst,tree"                 ,"","","Turn on chain y1a",kFALSE},
  {"Cy1b"        ,""  ,"","y1b,ftpc,l0,l3t,ems,emc,rich,dst,tree"        ,"","","Turn on chain y1b",kFALSE},
  {"Cy1c"        ,""  ,"","y1c,tpc,ftpc,l0,l3t,dst,tree"                 ,"","","Turn on chain y1c",kFALSE},
  {"Cy1d"        ,""  ,"","y1d,tpc,global,dst,qa,event,analysis,tree"    ,"","","Turn on chain y1d",kFALSE},
  {"cy1e"        ,""  ,"","y1e,tpc,dst,tree"                             ,"","","Turn on chain y1e",kFALSE},
  {"Cy2a"        ,"","","y2a,tpc,svt,ftpc,l0,l3t,ems,emc,rich,dst,tree ,","","","Turn on chain y2a",kFALSE},
  {"Eval"        ,""  ,"","","",""                ,"Turn on evaluation switch for different makers",kFALSE},
  //Options
  {"off"         ,""  ,"","","",""                                        ,"Turn off default chain",kFALSE},
  {"db"          ,""  ,"","tables"               ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"calib"       ,""  ,"","tables"               ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"xin"         ,""  ,"",""              ,"StIOMaker","StIOMaker","Read [XDF|DAQ|ROOT] input file",kFALSE},
  {"gstar"       ,"" ,"","tables,geant","","","gstar for 10 muon track with pT = 10 GeV in |eta|<1",kFALSE}, 
  {"tdaq"        ,""  ,"","er99,xin,tpc_daq"                                              ,"","","",kFALSE},  
  {"fzin"        ,""  ,"","geant","" ,""                                      ,"read gstar fz-file",kFALSE},
  {"util"        ,""  ,"","","","StAnalysisUtilities",                   "Load StAnalysisUtilities",kFALSE},
  {"tables"      ,""  ,"","",""                                     ,"St_Tables","Load Star Tables",kFALSE},
  {"tls"         ,""  ,"","",""                                                        ,"libtls","",kFALSE},
  {"SCL"         ,""  ,"","","","StarClassLibrary",                         "Load StarClassLibrary",kFALSE},
  {"FieldOn"     ,""  ,"","-FieldOff,-HalfField,-ReverseField","StMagFC","StMagF"  ,"Nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","-FieldOn,-HalfField,-ReverseField","StMagFC","StMagF" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","-FieldOn,-FieldOff,-ReverseField","StMagFC","StMagF","Half Field option",kFALSE},
  {"ReverseField",""  ,"","-FieldOn,-FieldOff,-HalfField","StMagFC","StMagF","Reverse Field option",kFALSE},
  // Makers
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"geant","geant","","tables","St_geant_Maker","geometry,St_g2r,St_geant_Maker","initailize geant",kFALSE}, 
  {"tpc"         ,"tpc","","tables,tls,db,-tss,-trs,tcl,tpt"                ,"StMaker","StChain","",kFALSE},
  {"tss"         ,"tpc_raw","tpc","tls,-trs"               ,"St_tss_Maker","St_tpc,St_tss_Maker","",kFALSE},  
  {"Trs"         ,"","tpc","scl,-tss,tpc_daq"                         ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"miniDAQ"     ,"tpc_raw","tpc","xin,FieldOff,SD97,Eval"    ,"StMinidaqMaker","StMinidaqMaker","",kFALSE}, 
  {"tpc_daq"     ,"tpc_raw","tpc",""                        ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,""  ,"","tpc,-trs,-tss","",""     ,"use tfs       (no St_[tss_ and no Trs]Maker)",kFALSE},
  {"tcl"         ,"tpc_hits","tpc","tables,tls"            ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpc","tables,tls"          ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"laser"       ,"tpc_tracks","tpc","tdaq,tpc,-tpt"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},  
  {"svt"         ,"svt"     ,""     ,"tables,srs,stk"                       ,"StMaker","StChain","",kFALSE},
  {"srs"         ,"svt_hits","svt","tls"                   ,"St_srs_Maker","St_svt,St_srs_Maker","",kFALSE},
  {"stk"         ,"svt_tracks","svt","tls"                 ,"St_stk_Maker","St_svt,St_stk_Maker","",kFALSE},
  {"Ftpc"        ,"ftpc"  ,"","tables,-fss,fcl,fpt"                         ,"StMaker","StChain","",kFALSE},
  {"fss"         ,"ftpc_raw","ftpc","SCL"                 ,"St_fss_Maker","St_ftpc,St_fss_Maker","",kFALSE},
  {"Fcl"         ,"ftpc_hits","ftpc","SCL"    ,"StFtpcClusterMaker","St_ftpc,StFtpcClusterMaker","",kFALSE},
  {"fpt"         ,"ftpc_tracks","ftpc","SCL"              ,"St_fpt_Maker","St_ftpc,St_fpt_Maker","",kFALSE},
  {"emc"         ,"emc","","geant,tables,ems,emh"                           ,"StMaker","StChain","",kFALSE},
  {"ems"         ,"emc_raw","emc","geant,tables"           ,"St_ems_Maker","St_emc,St_ems_Maker","",kFALSE},
  {"emh"         ,"emc_hits","emc","geant,tables"          ,"St_emc_Maker","St_emc,St_emc_Maker","",kFALSE},
  {"l0"          ,"l0","","tables,ctf,mwc,trg"                              ,"StMaker","StChain","",kFALSE}, 
  {"ctf"         ,"ctf","l0","tables"                      ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE}, 
  {"mwc"         ,"mwc","l0","tables"                      ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE}, 
  {"trg"         ,"trg","l0","tables"                      ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"l3t"         ,"l3Tracks","","tables"                    ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"rich"        ,"rch","","tables"                                   ,"StRchMaker","StRchMaker","",kFALSE},
  {"global"      ,"global","","tables,Match,primary,v0,xi,kink,dst"         ,"StMaker","StChain","",kFALSE},
  {"Match"       ,"match","global","SCL,tables,tls","StMatchMaker"
                                                                ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Primary"     ,"primary","global","SCL,tables,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"V0"          ,"v0","global","SCL,tables,tls"    ,"StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"          ,"xi","global","SCL,tables,tls"    ,"StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"        ,"kink","global","SCL,tables,tls","StKinkMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"dst"         ,"dst","global","SCL,tables,tls","St_dst_Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Event"       ,"StEventMaker","","tables,SCL","StEventMaker","St_Tables,StEvent,StEventMaker","",kFALSE},
  {"analysis"    ,"analysis","","SCL"               ,"StAnalysisMaker","StEvent,StAnalysisMaker","",kFALSE},
  {"QA"          ,"QA","","tables,SCL"                    ,"St_QA_Maker","St_Tables,St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","tables",""         ,"StQACosmicMaker","St_Tables,StQACosmicMaker","",kFALSE},
  {"AllEvent"    ,""  ,"",""                                   ,"","","Write whole event to StTree",kFALSE},
  {"St_geom"     ,"","",""       ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"    ,"EventDisplay","","St_geom"        ,"StEventDisplayMaker","StEventDisplayMaker","",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                                ,"","","Set debug flag",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},  
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE}, 
  {"Tree"        ,""  ,"",""                                        ,"StTreeMaker","StTreeMaker","",kFALSE}
};
Int_t NoChainOptions = sizeof (BFC)/sizeof (BfcItem);
StFile  *setFiles= 0;
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
Bool_t   ChainFlagSet = kFALSE;
class StEvent;
StEvent *Event;
class StIOMaker; StIOMaker *inpMk=0;     
class St_geant_Maker; St_geant_Maker *geant   = 0;   
class St_db_Maker;    St_db_Maker    *dbMk    = 0; St_db_Maker *calibMk = 0;
class StTreeMaker;    StTreeMaker    *treeMk  = 0;

ClassImp(StBFChain)

//_____________________________________________________________________________
StBFChain::StBFChain(const char *name):StChain(name),xdf_out(0){}
//_____________________________________________________________________________
StBFChain::~StBFChain(){
  Finish();
}
//_____________________________________________________________________________
Int_t StBFChain::Load() 
{
  Int_t i, j, iok;
  for (i = 1; i< NoChainOptions; i++) {// Load Libraries if any
    if (BFC[i].Flag) {
      if (strlen(BFC[i].Libs) > 0) { 
	TString *Libs[80];
	Int_t NParsed = ParseString(BFC[i].Libs,Libs);
	const Char_t *lib = gSystem->GetLibraries(); 
	for (j=0;j<=NParsed;j++) {
	  TString libe(*Libs[j]);
	  libe.Append(".");
	  if (!strstr(lib,libe.Data())) {
	    iok = gSystem->Load(Libs[j]->Data());
	    if (iok < 0)  {
	      gMessMgr->QAInfo() << "\tproblem with loading\t" << Libs[j]->Data() << endm;
	      gMessMgr->QAInfo() << "\tswitch off\t" << BFC[i].Key << "\t !!!!!" << endm;
	      BFC[i].Flag = kFALSE;
	      break;
	    }
	    else          gMessMgr->QAInfo() << "\tLibrary " << Libs[j]->Data() << "\tis loaded" << endm;
	  }
	}
      }
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
Int_t StBFChain::Instantiate() 
{
  Int_t i;
  for (i = 1; i< NoChainOptions; i++) {// Instantiate Makers if any
    if (BFC[i].Flag) {
      if (strlen(BFC[i].Maker) > 0 && !(strlen(BFC[i].Name) > 0 && GetMaker(BFC[i].Name))) { 
	TString maker(BFC[i].Maker);
	TString Key(BFC[i].Key);
	Key.ToLower();
	StMaker *saveMk = 0;
	if (strlen(BFC[i].Chain) > 0) {
	  StMaker *chain = GetMaker(BFC[i].Chain);
	  if (chain) saveMk = chain->cd();
	}
	if (maker == "StMagFC") {
	  Float_t Scale = 1.0;
	  TString FieldName("STAR Normal field");
	  if (GetOption("FieldOff"))     {Scale = 0.00002; FieldName = "STAR no field";}
	  if (GetOption("HalfField"))    {Scale = 0.5;     FieldName = "STAR Normal field";}
	  if (GetOption("ReverseField")) {Scale = - Scale; FieldName += " Reverse";}
	  new StMagFC("field",FieldName.Data(),Scale);
	  continue;
	}
	if (maker == "St_db_Maker") {
	  if (Key == "calib") {
	    const char *calibDB = "$STAR_ROOT/calib";
	    calibMk = new St_db_Maker("calib",calibDB);
	  }
	  else {
	    const char *mainDB = "$STAR/StDb/params";
	          char *userDB = 0;
		  TString STAR("$STAR");
		  gSystem->ExpandPathName(STAR);
	          TString PWD(gSystem->pwd());
                  if (STAR != PWD && gSystem->AccessPathName("./StDb/params") == 0)  userDB = "./StDb/params";
	    //DbInit from StDbBroker.so checks that mysql db1 server is accessible
	    //  if (StDbBroker::DbInit("params")==0) mainDB = "MySQL:params";
	    gMessMgr->QAInfo() << " Main DataBase == " << mainDB << endm;  
	    if (userDB) gMessMgr->QAInfo() << " User DataBase == " << PWD.Data() << "/" << userDB << endm;  
	    dbMk = new St_db_Maker("db",mainDB,userDB);
	    if (dbMk) {
	      BFC[i].Name = (Char_t *) dbMk->GetName();
	      SetDbOptions();
	    }
	  }
	  continue;
	}
	if (maker == "StIOMaker" && setFiles) {
	  inpMk = new StIOMaker("inputStream","r",setFiles);
	  if (inpMk) {
	    BFC[i].Name = (Char_t *) inpMk->GetName();
	    SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
	    SetInput("geant",".make/inputStream/.make/inputStream_XDF/.data/event/geant/Event");
	  }
	  continue;
	}
	if (maker == "StTreeMaker" && FileOut) {
	  treeMk = new StTreeMaker("tree",FileOut->Data());
	  if (treeMk) {
	    BFC[i].Name = (Char_t *) treeMk->GetName();
	    treeMk->SetIOMode("w");
	    if (GetOption("dst"))      treeMk->IntoBranch("dstBranch","dst");
	    else {
	      if (GetOption("global")) treeMk->IntoBranch("globalBranch","global/.data");
	    }
	    if (GetOption("Event"))    treeMk->IntoBranch("eventBranch","StEvent");
	    if (GetOption("AllEvent")) SetTreeOptions();
	    treeMk->SetBranch("histBranch");
	    continue;
	  }
	}
	if (Key == "geant") {
	  geant = new St_geant_Maker("geant");
	  if (geant) {
	    BFC[i].Name = (Char_t *) geant->GetName();
	    if (!GetOption("fzin") && !GetOption("gstar")) {
	      geant->SetNwGEANT(5000000);
	      geant->SetActive(kFALSE);
	    }
	    else     geant->SetNwGEANT(10000000);
	    SetGeantOptions();
	  }
	  continue;
	}
	StMaker *mk = 0;
	if (strlen(BFC[i].Name) > 0) mk = New(BFC[i].Maker,BFC[i].Name);
	else  {
	  mk = New(BFC[i].Maker);
	  if (mk) BFC[i].Name = (Char_t *) mk->GetName();
	}
	if (mk && maker == "St_dst_Maker") SetInput("dst",".make/dst/.data/dst");
	if (mk && maker == "St_tpcdaq_Maker") {
	  if (GetOption("Trs")) mk->SetMode(1); // trs
	  else                  mk->SetMode(0); // daq
	}
	if (saveMk) saveMk->cd();
      }
    }
  }
  if (XdfFile) xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
  //  PrintQAInfo();
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef GetOption("trg")
  if (GetOption("MakeDoc")) MakeDoc();
  if (GetOption("Debug")) SetDEBUG();
  return kStOk;
}
Int_t StBFChain::Finish()
{
  SafeDelete (xdf_out);
  if (m_TFile) {m_TFile->Write(); m_TFile->Close(); SafeDelete (m_TFile);}
  return StMaker::Finish();
}
//_____________________________________________________________________
Int_t StBFChain::AddAB (const Char_t *mkname,const StMaker *maker,const Int_t Opt) {
  if (! maker || strlen(mkname) == 0) return kStErr;
  StMaker *parent = maker->GetParentMaker();
  if (parent) {
    TList   *list    = parent->GetMakeList();
    list->Remove((StMaker *)maker);
  }
  StMaker *mk      = GetMaker(mkname);      if (!mk)     return kStErr;
  parent  = mk->GetParentMaker();  if (!parent) return kStErr;
  TList   *list    = parent->GetMakeList(); if (!list)   return kStErr;
  if (Opt > 0) list->AddAfter (mk,(StMaker*)maker);
  else         list->AddBefore(mk,(StMaker*)maker);
  return kStOk;
}
//_____________________________________________________________________
Int_t StBFChain::ParseString (const TString &tChain, TString *Opt[]) {
  Int_t nParsed = -1;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  TString Tag, opt, nopt;
  while ( (begin < tChain.Length()) && (index != kNPOS) ) { 
    // loop over given Chain options 
    index = tChain.Index(separator,&end,begin);
    if (index >= 0) Opt[++nParsed] = new TString(tChain(index,end));
    begin += end+1;
  }
  return nParsed;
}
//_____________________________________________________________________
Int_t StBFChain::kOpt (const Char_t *tag) const {
  TString *Tag = new TString(tag);
  Int_t kO = kOpt(Tag);
  delete Tag;
  return kO;
}
//_____________________________________________________________________
Int_t StBFChain::kOpt (const TString *tag) const {
  TString Tag = *tag;
  Tag.ToLower();
  if       (Tag == "in") Tag = "xin";
  TString opt, nopt;
  for (Int_t i = 1; i< NoChainOptions; i++) {
    opt = TString(BFC[i].Key);
    opt.ToLower();
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
  }
  printf ("Option %s has been not recognized\n", Tag.Data());
  return 0;
}//_____________________________________________________________________
void StBFChain::SetOption(const Int_t k) {// set all off
  if (k > 0 && !BFC[k].Flag) {
    //    printf ("SetOption: %s %i",BFC[k].Key,k);
    BFC[k].Flag = kTRUE;
    printf (" Switch On  %s\n", BFC[k].Key);
    if (strlen(BFC[k].Opts) > 0) {
      TString *Opt[80];
      Int_t NParsed = ParseString(BFC[k].Opts,Opt);
      Int_t i;
      for (i=0;i<=NParsed;i++) SetOption(Opt[i]);
    }
  }
  else {
    if (k < 0 && BFC[-k].Flag) {
      //      printf ("SetOption: %s %i",BFC[-k].Key,k);
      BFC[-k].Flag = kFALSE;
      printf (" Switch Off %s\n", BFC[-k].Key);
    }
    else return;
  }
}
//_____________________________________________________________________
Bool_t StBFChain::GetOption(const Int_t k) {
  return (k>0 && k <NoChainOptions) ? BFC[k].Flag : kFALSE;
}
//_____________________________________________________________________________
void StBFChain::SetFlags(const Char_t *Chain, Bool_t Force)
{
  if (ChainFlagSet && !Force) return;
  Int_t k;
  if (!Chain || !strlen(Chain)) {
    printf ("\tPossible Chain Options are: \n"); 
    for (k=0;k<NoChainOptions;k++)
      printf (" %2d:[-]%-12s:%-12s:%-6s:%-12s :%s :%s :%s\n"
	      ,k,BFC[k].Key,BFC[k].Name,BFC[k].Chain,BFC[k].Opts,BFC[k].Maker,BFC[k].Libs,BFC[k].Comment);
    return; 
  }         
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  printf ("==============================================\n");
  gMessMgr->QAInfo() << "============= You are in " << STAR_VERSION.Data() << " ===============" << endm;
  TString tChain(Chain);
  gMessMgr->QAInfo() << "Requested chain is :\t" << tChain.Data() << endm;
  tChain.ToLower(); //printf ("Chain %s\n",tChain.Data());
  TString *Opt[80];
  Int_t NParsed = ParseString(tChain,Opt);
  Int_t i;
  for (i=0;i<=NParsed;i++){
    Int_t kgo = kOpt(*Opt[i]);
    if (kgo != 0) SetOption(kgo);
  }
  // Check flags consistency   
  if (!GetOption("fzin") && !GetOption("geant") &&
      !GetOption("xin") && !GetOption("gstar") &&!GetOption("tdaq")) {
    SetOption("fzin");
    SetOption("geant");
  }
  if (!GetOption("geant") && !GetOption("FieldOff") && 
      !GetOption("HalfField") && !GetOption("FieldOn")) { 
    SetOption("FieldOn"); 
  }
  if (!GetOption("global") && 
      (GetOption("Match") || GetOption("Primary") || GetOption("V0") ||
       GetOption("Xi")    || GetOption("Kink"))) SetOption("global");
  if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval"); 
  //  SetOption("-emc");
  //  SetOption("-Kink");
  // Print set values
  for (k = 1; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      gMessMgr->QAInfo() << "================== " << k << "\t" << BFC[k].Key << "\tis ON \t:" << BFC[k].Comment << endm;
    }
  }
  ChainFlagSet = kTRUE;
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  // define input file
  Char_t *Infile  = (Char_t *) infile;
  if (!Infile) {
    if (GetOption("miniDAQ")) {
      Infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
      //Infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
      printf ("Use default input file %s for %s \n",Infile,"miniDAQ");
    }
    else {
      if (GetOption("fzin")) {
	if (GetOption("Y1b")) Infile = "/disk0/star/test/venus412/b0_3/year_1b/psc0050_01_40evts.fzd";
	else {
	  if (GetOption("Y2a"))  Infile = "/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fzd";
	  else                  Infile ="/disk1/star/test/psc0049_08_40evts.fzd";
	}
	printf ("Use default input file %s for %s \n",Infile,"fzin");
      }
      else {
	if (GetOption("doEvents")){
	  Infile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
	  printf ("Use default input file %s for %s \n",Infile,"doEvent");
	}
	else { 
	  if (!GetOption("gstar")) {
	    Infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
	    printf ("Use default input file %s for %s \n",Infile,"xin");
	  }
	}
      }
    }
  }
  if (Infile) {
    InFile = new TString(Infile);
    if (Infile && !GetOption("fzin")) {
      setFiles= new StFile();
      InFile = new TString(Infile);
      TString *Files[80];
      Int_t NParsed = ParseString((const TString )*InFile,Files);
      Int_t i;
      for (i=0;i<=NParsed;i++) {
	if (!strstr(Files[i]->Data(),"*") &&
	    gSystem->AccessPathName(Files[i]->Data())) {// file does not exist
	  printf (" *** NO FILE: %s, exit!\n", Files[i]->Data());
	  gSystem->Exit(1); 
	}
	else setFiles->AddFile(Files[i]->Data());
      }
    }
  }
  if (InFile) gMessMgr->QAInfo() << "Input file name = " << InFile->Data() << endm;
  if (outfile)               FileOut = new TString(outfile);
  else {
    if (GetOption("gstar"))  FileOut = new TString("gtrack");
    else {
      if (InFile) {
	FileOut = new TString(gSystem->BaseName(InFile->Data()));
	FileOut->ReplaceAll("*","");
	FileOut->ReplaceAll("..",".");
	FileOut->ReplaceAll(".daq","");
	FileOut->ReplaceAll(".fzd","");
	FileOut->ReplaceAll(".fz","");
	FileOut->ReplaceAll(".xdf","");
	FileOut->Strip();
      }
    }
  }
  if (FileOut)  gMessMgr->QAInfo() << "Output root file name " <<  FileOut->Data() << endm;
  if (GetOption("xout") && FileOut) {
    XdfFile = new TString(FileOut->Data());
    XdfFile->Append(".dst.xdf");
    gMessMgr->QAInfo() << "Open output xdf file  = " << XdfFile->Data() <<  "++++++++++++++++++++++" << endm;
  }
  //    gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::SetGeantOptions(){
  if (geant) {
    SetInput("geant",".make/geant/.data");
    if (GetOption("Higz"))  geant->SetIwtype(1);
    if (!GetOption("fzin")) {
      if (GetOption("SD97") || GetOption("SD98") || GetOption("Y1a") || 
	  GetOption("ES99") || GetOption("ER99")) geant->LoadGeometry("detp geometry YEAR_1A"); 
      else {
	if (GetOption("Y1b"))                     geant->LoadGeometry("detp geometry YEAR_1B");
	else {
	  if (GetOption("Y1c"))                   geant->LoadGeometry("detp geometry YEAR_1C");
	  else {
	    if (GetOption("ES99"))                geant->LoadGeometry("detp geometry YEAR_1A");
	    else {
	      if (GetOption("ER99"))              geant->LoadGeometry("detp geometry YEAR_1A");
	      else {
		if (GetOption("Y2a"))             geant->LoadGeometry("detp geometry YEAR_2A");
		else                              geant->LoadGeometry("detp geometry YEAR_2A");
	      }
	    }
	  }
	}
      }
      if (GetOption("gstar")) {
	geant->Do("subevent 0;");
	// gkine #particles partid ptrange yrange phirange vertexrange 
	geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  0. 0.;");
	geant->Do("mode g2tm prin 1;");
	//  geant->Do("next;");
	//  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
	if (GetOption("Debug")) geant->Do("debug on;");
	geant->Do("swit 2 3;");
      }
    }
    else {
      if (geant->SetInputFile(InFile->Data()) > kStOK) {
	printf ("File %s cannot be opened. Exit! \n",InFile->Data());
	gSystem->Exit(1);
      }
    }
  }
}
//_____________________________________________________________________
void StBFChain::SetDbOptions(){
        if (GetOption("SD97")) dbMk->SetDateTime("sd97");
  else {if (GetOption("SD98")) dbMk->SetDateTime("sd98");
  else {if (GetOption("Y1a"))  dbMk->SetDateTime("year_1a");
  else {if (GetOption("Y1b"))  dbMk->SetDateTime("year_1b");
  else {if (GetOption("Y1c"))  dbMk->SetDateTime("year_1c");
  else {if (GetOption("ES99")) dbMk->SetDateTime("es99");
  else {if (GetOption("ER99")) dbMk->SetDateTime("er99");
  else {if (GetOption("Y1d"))  dbMk->SetDateTime("year_1d");
  else {if (GetOption("Y1e"))  dbMk->SetDateTime("year_1e");
  else {if (GetOption("Y2a"))  dbMk->SetDateTime("year_2a");
  else                         dbMk->SetDateTime("year_2a");}}}}}}}}}
  gMessMgr->QAInfo() << "db Maker set time = " << dbMk->GetDateTime().GetDate() 
		   << dbMk->GetDateTime().GetTime() << endm;
}
//_____________________________________________________________________
void StBFChain::SetTreeOptions(){
  if (geant) {
    treeMk->IntoBranch("geantBranch","geant");
    treeMk->IntoBranch("geantBranch","geant/.data/particle");
    treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
  }
  if (GetOption("fss"))    treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
  if (GetOption("ems"))    treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
  if (GetOption("tss") || GetOption("Trs"))
    treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
  if (GetOption("tcl"))    treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
  if (GetOption("tpt"))    treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
  if (GetOption("trg"))    treeMk->IntoBranch("trgBranch","ctf mwc trg");
  if (GetOption("l3t"))    treeMk->IntoBranch("l3tBranch","l3Tracks");
}
