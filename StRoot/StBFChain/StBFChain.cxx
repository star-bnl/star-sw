// $Id: StBFChain.cxx,v 1.17 1999/10/16 20:20:19 fisyak Exp $
// $Log: StBFChain.cxx,v $
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
//_____________________________________________________________________
Char_t  *ChainOptions[] = {
  "FIRST"
 ,"SD97"   ,"SD98"   ,"Y1a"    ,"Y1b"    ,"Y1c"
 ,"ES99"   ,"ER99"   ,"Y1d"    ,"Y1e"    ,"Y2a"
 ,"Eval"   ,"OFF"    ,"XIN"    ,"XOUT"   ,"GSTAR"
 ,"TDAQ"   ,"FZIN"   ,"GEANT"  ,"UTIL"   
 ,"FieldOn","FieldOff","HalfField","ReverseField"
 ,"TPC"    ,"TSS"    ,"TRS"    ,"MINIDAQ","TFS"    ,"TCL"    ,"TPT"
 ,"SVT"    ,"SRS"    ,"STK"
 ,"FTPC"   ,"FSS"    ,"FCL"    ,"FTP"
 ,"EMS"    ,"EMC"
 ,"TRG"    ,"CTF"    ,"MWC"    ,"L3T"
 ,"RICH"
 ,"GLOBAL" ,"MATCH"  ,"PRIMARY","V0"     ,"XI"     ,"KINK"
 ,"DST"    ,"EVENT"  ,"ANALYSIS","QA"
 ,"TREE"   ,"AllEvent","DISPLAY","LAST"
 ,"DEFAULT"
 ,"MakeDoc","DEBUG"  ,"HIGZ"
};
Int_t NoChainOptions = sizeof (ChainOptions)/sizeof (Char_t *);
Bool_t *ChainFlags   = new Bool_t [NoChainOptions]; 
Char_t *ChainComments[] = {
  "Nothing to comment",
  "Turn on Year 1a geometry and 1997 test parameters (and corresponding Makers)",
  "Turn on Year 1a geometry and 1998 test parameters (and corresponding Makers)",
  "Turn on Year 1a geometry (and corresponding Makers)",
  "Turn on Year 1b geometry (and corresponding Makers)",
  "Turn on Year 1c geometry (and corresponding Makers)",
  "Turn on Year 1a geometry and 1999 engineering run simulation parameters",
  "Turn on Year 1a geometry and 1999 engineering run real data parameters",
  "Turn on Year 1d geometry (and corresponding Makers)",
  "Turn on Year 1e geometry (and corresponding Makers)",
  "Turn on Year 2a geometry (and corresponding Makers)",
  "Turn on evaluation switch for different makers",
  "Turn off default chain",
  "Read [XDF|DAQ|ROOT] input file",
  "Write dst to XDF file",
  "Run gstar for 10 muon track with pT = 10 GeV in |eta|<1",
  "TPC DAQ chain",
  "read GSTAR fz-file",
  "initailize GEANT",
  "Load StAnalysisUtilities",
  "Use nominal STAR field",
  "No Field option",
  "Half Field option",
  "Reverse Field option",
  "TPC            \tin Chain (St_[tcl_+tpt]_Maker)",
  "St_tss_Maker   \tin Chain",
  "StTrsMaker     \tin Chain",
  "StMinidaqMaker \tin Chain",
  "use TFS \t(no St_[tss_ and no Trs]Maker)",
  "St_tcl_Maker   \tin Chain",
  "St_tpt_Maker   \tin Chain",
  "SVT            \tin Chain (St_[srs+stk]_Maker)",
  "St_srs_Maker   \tin Chain",
  "St_stk_Maker   \tin Chain",
  "FTPC           \tin Chain (St_[fcl+fpt]_Maker)",
  "St_fss_Maker   \tin Chain",
  "St_fcl_Maker   \tin Chain",
  "St_fpt_Maker   \tin Chain",
  "St_ems_Maker   \tin Chain",
  "St_emc_Maker   \tin Chain",
  "Trigger        \tin Chain (St_[ctf+mwc+trg]_Maker)",
  "St_ctf_Maker   \tin Chain",
  "St_mwc_Maker   \tin Chain",
  "St_l3t_Maker   \tin Chain",
  "StRchMaker     \tin Chain",
  "GLOBAL         \tin Chain (St[Match+Primary+V0+Xi+Kink]Maker)",
  "StMatchMaker   \tin Chain",
  "StPrimaryMaker \tin Chain",
  "StV0Maker      \tin Chain",
  "StXiMaker      \tin Chain",
  "StKinkMaker    \tin Chain",
  "St_dst_Maker   \tin Chain",
  "StEventMaker   \tin Chain",
  "StAnalysisMaker\tin Chain",
  "St_QA_Maker    \tin Chain",
  "StTreeMaker    \tin Chain",
  "Write whole event to StTree",
  "StEventDisplayMaker \tin Chain",
  "Nothing to comment",
  "Default has been set",
  "Make HTML documentation for the given Chain",
  "Set debug flag",
  "Pop HIGZ window"
};
StFile  *setFiles= 0;
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
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
void StBFChain::SetFlags(const Char_t *Chain )
{
  Int_t k;
  for (k = kFIRST;k<NoChainOptions;k++)  ChainFlags[k] = kFALSE;
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  printf ("==============================================\n");
  printf ("QAInfo:============= You are in %s ===============\n",STAR_VERSION.Data());
  if (!Chain || !strlen(Chain)) {
    printf ("============= \tPossible Chain Options are: \n"); 
    for (k=kFIRST;k<NoChainOptions;k++)
      printf ("============ %2d\t:[-]%-8s\t:%s \n",k,ChainOptions[k],ChainComments[k]);
    return;
  }
  TString tChain(Chain);
  printf ("QAInfo:Requested chain is :\t%s\n",tChain.Data());
  tChain.ToLower(); //printf ("Chain %s\n",tChain.Data());
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  TString Tag, opt, nopt;
  while ( (begin < tChain.Length()) && (index != kNPOS) ) { // loop over given Chain options 
    index = tChain.Index(separator,&end,begin);
    if (index >= 0) Tag = tChain(index,end);
    begin += end+1;
    if (index >=0) {
      Int_t kgo = 0;
      for (k = kFIRST+1; k<NoChainOptions; k++) {
	opt = TString(ChainOptions[k]);
	opt.ToLower();
	nopt = TString("-");
	nopt += opt;
	if       (Tag == "in") Tag = "xin";
	if       (Tag ==  opt) kgo =  k;
	else {if (Tag == nopt) kgo = -k;}
	if (kgo) {SetOption(kgo); break;}
      }
      if (!kgo) printf ("Option %s has been not recognized\n", Tag.Data());
    }
  }
  // Check flags consistency   
  if (!GetOption(kFZIN) && !GetOption(kGEANT) &&
      !GetOption(kXIN) && !GetOption(kGSTAR) &&!GetOption(kTDAQ)) {
    SetOption(kFZIN);
    SetOption(kGEANT);
  }
  if (!GetOption(kGEANT) && !GetOption(kFieldOff) && 
      !GetOption(kHalfField) && !GetOption(kFieldOn)) { 
    SetOption(kFieldOn ); 
  }
  if (!GetOption(kGLOBAL) && 
      (GetOption(kMATCH) || GetOption(kPRIMARY) || GetOption(kV0) ||
       GetOption(kXI)    || GetOption(kKINK))) SetOption(kGLOBAL);
  if (!GetOption(kEval) && GetOption(kAllEvent))  SetOption(kEval); 
  //  SetOption(-kEMC);
  //  SetOption(-kKINK);
  // Print set values
  for (k = kFIRST; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      printf ("QAInfo:================== %2d \t%s      \tis ON \t:%s \n",k,ChainOptions[k],ChainComments[k]);
    }
  }
  //  gSystem->Exit(1);
}
//_____________________________________________________________________________
Int_t StBFChain::Load() 
{
  gSystem->Load("StarClassLibrary");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  if (kUTIL) gSystem->Load("StAnalysisUtilities");
  //gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  if (GetOption(kFieldOff) || 
      GetOption(kFieldOn)  || 
      GetOption(kHalfField)||  
      GetOption(kReverseField))
       gSystem->Load("StMagF");
  else gSystem->Load("geometry");
  if (GetOption(kXIN)) gSystem->Load("StIOMaker");
  if (GetOption(kGEANT))  {
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
  }

  if (GetOption(kTPC)) {
    gSystem->Load("St_tpc");
    if (GetOption(kTCL)) gSystem->Load("St_tcl_Maker");
    if (GetOption(kTPT)) gSystem->Load("St_tpt_Maker");
    if (GetOption(kTDAQ) || GetOption(kTRS)) {
       if (GetOption(kTRS)) gSystem->Load("StTrsMaker"); 
	gSystem->Load("StDaqLib");
	gSystem->Load("St_tpcdaq_Maker");
    }
    else {
      if (GetOption(kTSS)) gSystem->Load("St_tss_Maker");
    }
  }
  if (GetOption(kMINIDAQ)) {
    gSystem->Load("StMinidaqMaker");
  }
  if (GetOption(kFTPC)) {
    gSystem->Load("St_ftpc");
    if (GetOption(kFSS)) gSystem->Load("St_fss_Maker");
    if (GetOption(kFCL)) gSystem->Load("St_fcl_Maker");
    if (GetOption(kFPT)) gSystem->Load("St_fpt_Maker");
  }
  if (GetOption(kEMS) || GetOption(kEMC)) {
    gSystem->Load("St_emc");
    if (GetOption(kEMS)) gSystem->Load("St_ems_Maker");
    if (GetOption(kEMC)) gSystem->Load("St_emc_Maker");
  }
  if (GetOption(kTRG)) {
    if (GetOption(kCTF)) {
      gSystem->Load("St_ctf");
      gSystem->Load("St_ctf_Maker");
    }
    if (GetOption(kMWC)) {
      gSystem->Load("St_mwc");
      gSystem->Load("St_mwc_Maker");
    }
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
  }
  if (GetOption(kL3T)){
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
  }
  if (GetOption(kRICH)) {
    gSystem->Load("StRchMaker");
  }
  if (GetOption(kSVT) || GetOption(kGLOBAL)) {
    gSystem->Load("St_svt");
    if (GetOption(kSRS)) gSystem->Load("St_srs_Maker");
    if (GetOption(kSTK)) gSystem->Load("St_stk_Maker");
  }
  if (GetOption(kGLOBAL)) {
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    if (GetOption(kEVENT)) {
      gSystem->Load("StEvent");
      gSystem->Load("StEventMaker");
      if (GetOption(kANALYSIS)) gSystem->Load("StAnalysisMaker");
    }
    gSystem->Load("St_QA_Maker");
  }
  if (GetOption(kDISPLAY)) {
    gSystem->Load("St_geom_Maker");
    gSystem->Load("StEventDisplayMaker");
  }
  if (GetOption(kTREE)) gSystem->Load("StTreeMaker");

  //  gSystem->Exit(1);
  // Instantiate makers
  //  Create the makers to be called by the current chain
  if (GetOption(kXIN) && setFiles) {
    inpMk = new StIOMaker("inputStream","r",setFiles);
    if (inpMk) {
      SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
      SetInput("geant",".make/inputStream/.make/inputStream_XDF/.data/event/geant/Event");
    }
  }
  if (GetOption(kGEANT))  {
    geant = new St_geant_Maker("geant");
    if (geant) {
      geant->SetNwGEANT(10000000);
      SetInput("geant",".make/geant/.data");
      if (GetOption(kHIGZ))  geant->SetIwtype(1);
      if (GetOption(kGSTAR)) {
	if (GetOption(kSD97) || GetOption(kSD98) || GetOption(kY1a) || 
	    GetOption(kES99) || GetOption(kER99)) geant->LoadGeometry("detp geometry YEAR_1A");
	else {
	  if (GetOption(kY1b))                     geant->LoadGeometry("detp geometry YEAR_1B");
	  else {
	    if (GetOption(kY1c))                   geant->LoadGeometry("detp geometry YEAR_1C");
	    else {
	      if (GetOption(kES99))                geant->LoadGeometry("detp geometry YEAR_1A");
	      else {
		if (GetOption(kER99))              geant->LoadGeometry("detp geometry YEAR_1A");
		else {
		  if (GetOption(kY2a))             geant->LoadGeometry("detp geometry YEAR_2A");
		  else                              geant->LoadGeometry("detp geometry YEAR_2A");
		}
	      }
	    }
	  }
	}
	geant->Do("subevent 0;");
	// gkine #particles partid ptrange yrange phirange vertexrange 
	geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  0. 0.;");
	geant->Do("mode g2tm prin 1;");
	//  geant->Do("next;");
	//  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
	if (GetOption(kDEBUG)) geant->Do("debug on;");
	geant->Do("swit 2 3;");
	// geant->LoadGeometry("detp geometry GetOption(kFieldOn) field_off");
      }
      else {
	if (GetOption(kFZIN)) {
	  if (geant->SetInputFile(InFile->Data()) > kStOK) {
	    printf ("File %s cannot be opened. Exit! \n",InFile->Data());
	    gSystem->Exit(1);
	  }
	}
      }
    }
  }
  const char *mainDB = "$STAR/StDb/params";
  //DbInit from StDbBroker.so checks that mysql db1 server is accessible
  //  if (StDbBroker::DbInit("params")==0) mainDB = "MySQL:params";
  printf ("QAInfo: Main DataBase == %s\n",mainDB);  
  dbMk = new St_db_Maker("db",mainDB);
  if (GetOption(kSD97)) { dbMk->SetDateTime("sd97");}
  if (GetOption(kSD98)) { dbMk->SetDateTime("sd98");}
  if (GetOption(kY1a))  { dbMk->SetDateTime("year_1a");}
  if (GetOption(kY1b))  { dbMk->SetDateTime("year_1b");}
  if (GetOption(kY1c))  { dbMk->SetDateTime("year_1c");}
  if (GetOption(kES99)) { dbMk->SetDateTime("es99");}
  if (GetOption(kER99)) { dbMk->SetDateTime("er99");}
  if (GetOption(kY1d))  { dbMk->SetDateTime("year_1d");}
  if (GetOption(kY1e))  { dbMk->SetDateTime("year_1e");}
  if (GetOption(kY2a))  { dbMk->SetDateTime("year_2a");}
  printf ("QAInfo:db Maker set time = %d %d \n",dbMk->GetDateTime().GetDate(),
	  dbMk->GetDateTime().GetTime());
  const char *calibDB = "$STAR_ROOT/calib";
  calibMk = new St_db_Maker("calib",calibDB);
  if (GetOption(kFieldOff) ||
      GetOption(kHalfField)||
      GetOption(kFieldOn)  ||
      GetOption(kReverseField)) {
    Float_t Scale = 1.0;
    TString FieldName("STAR Normal field");
    if (GetOption(kFieldOff))     {Scale = 0.00002; FieldName = "STAR no field";}
    if (GetOption(kHalfField))    {Scale = 0.5;     FieldName = "STAR Normal field";}
    if (GetOption(kReverseField)) {Scale = - Scale; FieldName += " Reverse";}
    new StMagFC("field",FieldName.Data(),Scale);
  }
  if (GetOption(kMINIDAQ)) New("StMinidaqMaker","tpc_raw");
  else {
    //  S I M U L A T I O N  or D A Q
    if (GetOption(kTDAQ))  {
      StMaker *tpcdaq = New("St_tpcdaq_Maker","tpc_raw"); 
      tpcdaq->SetMode(0); // daq
    }
    else {
      if (GetOption(kTRS)) {//		trs
	New("StTrsMaker");
        StMaker *tpcdaq = New("St_tpcdaq_Maker","tpc_raw");
	tpcdaq->SetMode(1); // trs
      }
      else { 
	if (GetOption(kTSS)) New("St_tss_Maker","tpc_raw");
      }
    }
  }
  if (GetOption(kFSS)) New("St_fss_Maker","ftpc_raw");
  if (GetOption(kEMS)) New("St_ems_Maker","emc_raw" );
  if (GetOption(kEMC)) New("St_emc_Maker","emc_hits"); 
  // L O C A L    R E C O N S T R U C T I O
  if (GetOption(kTCL)) New ("St_tcl_Maker","tpc_hits");
  if (GetOption(kSRS)) New("St_srs_Maker","svt_hits");
  if (GetOption(kFCL)) New("St_fcl_Maker","ftpc_hits");  
  // T R A C K I N G
  if (GetOption(kTPT)) New("St_tpt_Maker","tpc_tracks");
  if (GetOption(kSTK)) New("St_stk_Maker","svt_tracks");
  if (GetOption(kFPT)) New("St_fpt_Maker","ftpc_tracks");
  // T R I G G E R
  if (GetOption(kTRG)) {
    if (GetOption(kCTF)) New("St_ctf_Maker","ctf");
    if (GetOption(kMWC)) New("St_mwc_Maker","mwc");
                         New("St_trg_Maker","trg");
  }
  // G L O B A L chain
  if (GetOption(kGLOBAL)) {//		global
    StMaker *glbMk = new StMaker("global");
    //    SetInput("dst",".make/global/.data/dst");
    if (glbMk) {
      StMaker *saveMK = glbMk->cd();
      if (GetOption(kMATCH))   New("StMatchMaker");
      if (GetOption(kPRIMARY)) New("StPrimaryMaker");
      if (GetOption(kV0))      New("StV0Maker");
      if (GetOption(kXI))      New("StXiMaker");
      if (GetOption(kKINK))    New("StKinkMaker");
      saveMK->cd();
    }
  }
  if (GetOption(kL3T)) New("St_l3t_Maker","l3Tracks"); 
  if (GetOption(kGLOBAL)) {
  //		dst
    if (GetOption(kDST) && New("St_dst_Maker","dst")) SetInput("dst",".make/dst/.data/dst");
    if (GetOption(kEVENT)){New("StEventMaker");
      if (GetOption(kANALYSIS)) New("StAnalysisMaker");
    }
    if (GetOption(kQA)) New("St_QA_Maker");  
  }
  if (GetOption(kDISPLAY)) {
    New("St_geom_Maker"); // this maker open its own TFile !!
    New("StEventDisplayMaker");
  }  
  
  if (GetOption(kTREE)) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut->Data());
    if (treeMk) {
      treeMk->SetIOMode("w");
      if (GetOption(kDST)) {
	//  treeMk->SetBranch("dstBranch",FileOut->Data());
	treeMk->IntoBranch("dstBranch","dst");
	//      TString hist(FileOut);
	//      hist.ReplaceAll(".root",".hist.root");
	//      treeMk->SetBranch("histBranch",hist.Data());
      }
      else if (GetOption(kGLOBAL)) {
	//  treeMk->SetBranch("globalBranch",FileOut->Data());
	treeMk->IntoBranch("globalBranch","global/.data");
      }
      if (GetOption(kEVENT)){
	//  treeMk->SetBranch("EventBranch",FileOut->Data());
	treeMk->IntoBranch("eventBranch","StEvent");
      }
      if (GetOption(kAllEvent)) {
	if (geant) {
	  treeMk->IntoBranch("geantBranch","geant");
	  //  treeMk->SetBranch("geantBranch",FileOut->Data());
	  treeMk->IntoBranch("geantBranch","geant/.data/particle");
	  treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
	}
	if (GetOption(kFSS)) {
	  //  treeMk->SetBranch("ftpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
	}
	if (GetOption(kEMS)) {
	  //  treeMk->SetBranch("emc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
	}
	if (GetOption(kTSS) || GetOption(kTRS)) { 
	  //  treeMk->SetBranch("tpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
	}
	if (GetOption(kTCL)) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
	if (GetOption(kTPT)) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
	if (GetOption(kTRG)) {
	  //  treeMk->SetBranch("trgBranch",FileOut->Data());
	  treeMk->IntoBranch("trgBranch","ctf mwc trg");
	}
	if (GetOption(kL3T)) {
	  //  treeMk->SetBranch("l3TBranch",FileOut->Data());
	  treeMk->IntoBranch("l3tBranch","l3Tracks");
	}
      }      
      treeMk->SetBranch("histBranch");
    }
  }
  if (XdfFile) xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef GetOption(kTRG)
  if (GetOption(kMakeDoc)) MakeDoc();
  if (GetOption(kDEBUG)) SetDEBUG();
  return kStOk;
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  // define input file
  Char_t *Infile  = (Char_t *) infile;
  Char_t *Outfile = (Char_t *) outfile;
  if (!Infile) {
    if (GetOption(kMINIDAQ)) {
      Infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
      //Infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
      printf ("Use default input file %s for %s \n",Infile,ChainOptions[kMINIDAQ]);
    }
    else {
      if (GetOption(kFZIN)) {
	if (GetOption(kY1b)) Infile = "/disk0/star/test/venus412/b0_3/year_1b/psc0050_01_40evts.fzd";
	else {
	  if (GetOption(kY2a))  Infile = "/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fzd";
	  else                  Infile ="/disk1/star/test/psc0049_08_40evts.fzd";
	}
	printf ("Use default input file %s for %s \n",Infile,ChainOptions[kFZIN]);
      }
      else { 
	if (!GetOption(kGSTAR)) {
	  Infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
	  printf ("Use default input file %s for %s \n",Infile,ChainOptions[kXIN]);
	}
      }
    }
  }
  if (Infile) {
    InFile = new TString(Infile);
    if (!strstr(InFile->Data(),"*") &&
	gSystem->AccessPathName(InFile->Data())) {// file does not exist
      printf (" *** NO FILE: %s, exit!\n", InFile->Data());
      gSystem->Exit(1); 
    }
    if (!GetOption(kFZIN)) {
      setFiles= new StFile();
      setFiles->AddFile(InFile->Data());
    }
  }
  if (GetOption(kGSTAR)) {
    if (!Outfile) FileOut = new TString("gtrack");
    else          FileOut = new TString(Outfile);
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
  }
  else {
    if (Outfile) FileOut = new TString(Outfile);
    else {
      FileOut = new TString(gSystem->BaseName(InFile->Data()));
      FileOut->ReplaceAll("*","");
      FileOut->ReplaceAll("..",".");
      FileOut->ReplaceAll(".daq","");
      FileOut->ReplaceAll(".fzd","");
      FileOut->ReplaceAll(".fz","");
      FileOut->ReplaceAll(".xdf","");
      FileOut->Strip();
    }
    printf ("==============================================\n");
    printf ("QAInfo:Input file name = %s\n"
	   ,InFile->Data());
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
  }
  if (GetOption(kXOUT) && FileOut) {
    XdfFile = new TString(FileOut->Data());
    XdfFile->Append(".dst.xdf");
    printf ("QAInfo:Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
  }
  //    gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::SetOption(int k){// set all OFF

  if (k > 0 && !ChainFlags[k]) {
    //    printf ("SetOption: %s %i",ChainOptions[k],k);
    ChainFlags[k] = kTRUE;
    printf (" Switch On  %s\n", ChainOptions[k]);
  }
  else {
    if (k < 0 && ChainFlags[-k]) {
      //      printf ("SetOption: %s %i",ChainOptions[-k],k);
      ChainFlags[-k] = kFALSE;
      printf (" Switch Off %s\n", ChainOptions[-k]);
    }
    else return;
  }
  switch (k) {
  case kFZIN:
    SetOption(kGEANT);
    break;
  case kGLOBAL:
    SetOption(kMATCH);
    SetOption(kPRIMARY);
    SetOption(kV0);
    SetOption(kXI);
    SetOption(kKINK);
    break;
  case -kGLOBAL:
    SetOption(-kMATCH);
    SetOption(-kPRIMARY);
    SetOption(-kV0);
    SetOption(-kXI);
    SetOption(-kKINK);
    break;
  case kTPC:
    SetOption(kTCL);
    SetOption(kTPT);
    break;
  case -kTPC:
    SetOption(-kTCL);
    SetOption(-kTPT);
    break;
  case kSVT:
    SetOption(kSRS);
    SetOption(kSTK);
    break;
  case -kSVT:
    SetOption(-kSRS);
    SetOption(-kSTK);
    break;
  case kFTPC:
    SetOption(kFCL);
    SetOption(kFPT);
    break;
  case -kFTPC:
    SetOption(-kFCL);
    SetOption(-kFPT);
    break;
  case kTRG:
    SetOption(kCTF);
    SetOption(kMWC);
    break;
  case -kTRG:
    SetOption(-kCTF);
    SetOption(-kMWC);
    break;
  case kOFF:
    SetOption(kDEFAULT);
    break;
  case kMINIDAQ:
    if (!GetOption(kDEFAULT)) {
      SetOption(kXIN);
      SetOption(kFieldOff);
      SetOption(kSD97);
      SetOption(kEval);
    }
    break;
  case kTDAQ:
    SetOption(kER99);
    SetOption(kXIN);
    break;
  case  kY1b:
    if (!GetOption(kDEFAULT)) {
      SetOption(kEMS);
      SetOption(kEMC);
      SetOption(kRICH);
    }
  case  kY1a:
  case  kY1c:
    if (!GetOption(kDEFAULT)) {
      SetOption(kFTPC);
      SetOption(kTRG);
      SetOption(kL3T);
    }
  case kSD97:
  case kSD98:
  case kER99:
  case kES99:
    if (!GetOption(kDEFAULT)) {
      SetOption(kDEFAULT);
      SetOption(kTPC);
      SetOption(kGLOBAL);
      SetOption(kDST);
      SetOption(kQA);
      SetOption(kEVENT);
      SetOption(kANALYSIS);
      SetOption(kTREE);
    }
    break;
  case  kY2a:
    if (!GetOption(kDEFAULT)) {
      SetOption(kDEFAULT);
      SetOption(kSVT);
      SetOption(kEMS);
      SetOption(kEMC);
      SetOption(kTPC);
      SetOption(kFTPC);
      SetOption(kTRG);
      SetOption(kGLOBAL);
      SetOption(kL3T);
      SetOption(kDST);
      SetOption(kQA);
      SetOption(kEVENT);
      SetOption(kANALYSIS);
      SetOption(kTREE);
    }
    break;
  case kGSTAR:
    if (!GetOption(kDEFAULT)) {
      printf ("QAInfo:no setup defined ==> use Y2a\n");
      SetOption(kY2a);
    }
    SetOption(kGEANT);
    break;
  default:
    break;
  }
}
//_____________________________________________________________________
Bool_t StBFChain::GetOption(Int_t k) {return (k>0 && k <=NoChainOptions) ? ChainFlags[k] : kFALSE;}
//_____________________________________________________________________
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
  if (Opt > 0) list->AddAfter ((StMaker*)maker,mk);
  else         list->AddBefore((StMaker*)maker,mk);
  return kStOk;
}
