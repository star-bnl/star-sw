// $Id: StBFChain.cxx,v 1.11 1999/08/25 19:49:45 fisyak Exp $
// $Log: StBFChain.cxx,v $
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
#include "StRoot/St_base/St_DataSet.h"
#include "StRoot/StChain/StChain.h"
#include "StRoot/xdf2root/St_XDFFile.h"
#include "StRoot/StMagF/StMagF.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StRoot/St_db_Maker/St_db_Maker.h"
#include "StRoot/St_dst_Maker/StV0Maker.h"
#include "StRoot/St_tcl_Maker/St_tcl_Maker.h"
#include "StRoot/St_tpt_Maker/St_tpt_Maker.h"
#include "StRoot/StTreeMaker/StTreeMaker.h"
#include "StRoot/StIOMaker/StIOMaker.h"

//_____________________________________________________________________
Char_t  *ChainOptions[] = {
  "FIRST"
 ,"SD97"   ,"SD98"   ,"Y1a"    ,"Y1b"    ,"Y1c"
 ,"ES99"   ,"ER99"   ,"Y1d"    ,"Y1e"    ,"Y2a"
 ,"Eval"   ,"OFF"    ,"XIN"    ,"XOUT"   ,"GSTAR"
 ,"TDAQ"   ,"FZIN"   ,"GEANT"
 ,"FieldOn","FieldOff","HalfField"
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
  "Read XDF input file with g2t",
  "Write dst to XDF file",
  "Run gstar for 10 muon track with pT = 10 GeV in |eta|<1",
  "TPC DAQ chain",
  "read GSTAR fz-file",
  "initailize GEANT",
  "Use nominal STAR field",
  "No Field option",
  "Half Field option",
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
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
class StEvent;
StEvent *Event;
class StIOMaker; StIOMaker *inpMk=0;     
class St_geant_Maker; St_geant_Maker *geant   = 0;   
class St_db_Maker;    St_db_Maker    *dbMk    = 0; St_db_Maker *calibMk = 0;
class StMagFC;         StMagFC        *field   = 0;           
class St_tcl_Maker;   St_tcl_Maker   *tclMk   = 0;     
class StV0Maker;      StV0Maker      *v0Mk    = 0;
class StTreeMaker;    StTreeMaker    *treeMk  = 0;

ClassImp(StBFChain)

//_____________________________________________________________________________
StBFChain::StBFChain(const char *name):StChain(name),xdf_out(0){}
//_____________________________________________________________________________
StBFChain::~StBFChain(){}
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
  if (!ChainFlags[kFZIN] && !ChainFlags[kGEANT] &&
      !ChainFlags[kXIN] && !ChainFlags[kGSTAR] &&!ChainFlags[kTDAQ]) {
    SetOption(kFZIN);
    SetOption(kGEANT);
  }
  if (!ChainFlags[kGEANT] && !ChainFlags[kFieldOff] && 
      !ChainFlags[kHalfField] && !ChainFlags[kFieldOn]) { 
    SetOption(kFieldOn ); 
  }
  if (!ChainFlags[kGLOBAL] && 
      (ChainFlags[kMATCH] || ChainFlags[kPRIMARY] || ChainFlags[kV0] ||
       ChainFlags[kXI]    || ChainFlags[kKINK])) SetOption(kGLOBAL);
  if (!ChainFlags[kEval] && ChainFlags[kAllEvent])  SetOption(kEval); 
  //  SetOption(-kL3T);
  SetOption(-kEMC);
  //  SetOption(-kKINK);
  // Print set values
  for (k = kFIRST; k<NoChainOptions;k++) {
    if (ChainFlags[k]) {
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
  //gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_db_Maker");
  if (ChainFlags[kFieldOff] || ChainFlags[kFieldOn] || ChainFlags[kHalfField])
       gSystem->Load("StMagF");
  else gSystem->Load("geometry");
  if (ChainFlags[kXIN]) gSystem->Load("StIOMaker");
  if (ChainFlags[kGEANT])  {
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
  }

  if (ChainFlags[kTPC]) {
    gSystem->Load("St_tpc");
    if (ChainFlags[kTCL]) gSystem->Load("St_tcl_Maker");
    if (ChainFlags[kTPT]) gSystem->Load("St_tpt_Maker");
    if (ChainFlags[kTDAQ] || ChainFlags[kTRS]) {
       if (ChainFlags[kTRS]) gSystem->Load("StTrsMaker"); 
	gSystem->Load("StDaqLib");
	gSystem->Load("St_tpcdaq_Maker");
    }
    else {
      if (ChainFlags[kTSS]) gSystem->Load("St_tss_Maker");
    }
  }
  if (ChainFlags[kMINIDAQ]) {
    gSystem->Load("StMinidaqMaker");
  }
  if (ChainFlags[kFTPC]) {
    gSystem->Load("St_ftpc");
    if (ChainFlags[kFSS]) gSystem->Load("St_fss_Maker");
    if (ChainFlags[kFCL]) gSystem->Load("St_fcl_Maker");
    if (ChainFlags[kFPT]) gSystem->Load("St_fpt_Maker");
  }
  if (ChainFlags[kEMS] || ChainFlags[kEMC]) {
    gSystem->Load("St_emc");
    if (ChainFlags[kEMS]) gSystem->Load("St_ems_Maker");
    if (ChainFlags[kEMC]) gSystem->Load("St_emc_Maker");
  }
  if (ChainFlags[kTRG]) {
    if (ChainFlags[kCTF]) {
      gSystem->Load("St_ctf");
      gSystem->Load("St_ctf_Maker");
    }
    if (ChainFlags[kMWC]) {
      gSystem->Load("St_mwc");
      gSystem->Load("St_mwc_Maker");
    }
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
  }
  if (ChainFlags[kL3T]){
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
  }
  if (ChainFlags[kRICH]) {
    gSystem->Load("StRchMaker");
  }
  if (ChainFlags[kSVT] || ChainFlags[kGLOBAL]) {
    gSystem->Load("St_svt");
    if (ChainFlags[kSRS]) gSystem->Load("St_srs_Maker");
    if (ChainFlags[kSTK]) gSystem->Load("St_stk_Maker");
  }
  if (ChainFlags[kGLOBAL]) {
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    if (ChainFlags[kEVENT]) {
      gSystem->Load("StEvent");
      gSystem->Load("StEventMaker");
      if (ChainFlags[kANALYSIS]) gSystem->Load("StAnalysisMaker");
    }
    gSystem->Load("St_QA_Maker");
  }
  if (ChainFlags[kDISPLAY]) {
    gSystem->Load("St_geom_Maker");
    gSystem->Load("StEventDisplayMaker");
  }
  if (ChainFlags[kTREE]) gSystem->Load("StTreeMaker");

  //  gSystem->Exit(1);
  // Instantiate makers
  if (ChainFlags[kDEBUG]) SetDebug();
  //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb/params";
  dbMk = new St_db_Maker("db",mainDB);
  if (dbMk && ChainFlags[kDEBUG]) dbMk->SetDebug();
  if (ChainFlags[kSD97]) { dbMk->SetDateTime("sd97");}
  if (ChainFlags[kSD98]) { dbMk->SetDateTime("sd98");}
  if (ChainFlags[kY1a])  { dbMk->SetDateTime("year_1a");}
  if (ChainFlags[kY1b])  { dbMk->SetDateTime("year_1b");}
  if (ChainFlags[kY1c])  { dbMk->SetDateTime("year_1c");}
  if (ChainFlags[kES99]) { dbMk->SetDateTime("es99");}
  if (ChainFlags[kER99]) { dbMk->SetDateTime("er99");}
  if (ChainFlags[kY1d])  { dbMk->SetDateTime("year_1d");}
  if (ChainFlags[kY1e])  { dbMk->SetDateTime("year_1e");}
  if (ChainFlags[kY2a])  { dbMk->SetDateTime("year_2a");}
  printf ("QAInfo:db Maker set time = %d %d \n",dbMk->GetDateTime().GetDate(),
	  dbMk->GetDateTime().GetTime());
  const char *calibDB = "$STAR_ROOT/calib";
  calibMk = new St_db_Maker("calib",calibDB);
  if (calibMk && ChainFlags[kDEBUG]) calibMk->SetDebug();  
  if (ChainFlags[kFieldOff]) field   = new StMagFC("field","STAR no field",0.00002);
  if (ChainFlags[kFieldOn])  field   = new StMagFC("field","STAR Normal field",1.);
  if (ChainFlags[kHalfField])field   = new StMagFC("field","STAR Half field",0.5);
  if (ChainFlags[kXIN]) {
    StIOMaker *inpMk = new StIOMaker("inputStream","r",InFile->Data());
    if (inpMk) {
      if (ChainFlags[kDEBUG]) inpMk->SetDebug();
      SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
      SetInput("geant",".make/inputStream/.make/inputStream_XDF/.data/event/geant/Event");
    }
  }
  if (ChainFlags[kGEANT])  {
    geant = new St_geant_Maker("geant");
    if (geant) {
      geant->SetNwGEANT(10000000);
      SetInput("geant",".make/geant/.data");
      if (ChainFlags[kHIGZ])  geant->SetIwtype(1);
      if (ChainFlags[kDEBUG]) geant->SetDebug();
      if (ChainFlags[kGSTAR]) {
	if (ChainFlags[kSD97] || ChainFlags[kSD98] || ChainFlags[kY1a] || 
	    ChainFlags[kES99] || ChainFlags[kER99]) geant->LoadGeometry("detp geometry YEAR_1A");
	else {
	  if (ChainFlags[kY1b])                     geant->LoadGeometry("detp geometry YEAR_1B");
	  else {
	    if (ChainFlags[kY1c])                   geant->LoadGeometry("detp geometry YEAR_1C");
	    else {
	      if (ChainFlags[kES99])                geant->LoadGeometry("detp geometry YEAR_1A");
	      else {
		if (ChainFlags[kER99])              geant->LoadGeometry("detp geometry YEAR_1A");
		else {
		  if (ChainFlags[kY2a])             geant->LoadGeometry("detp geometry YEAR_2A");
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
	if (ChainFlags[kDEBUG]) geant->Do("debug on;");
	geant->Do("swit 2 3;");
	// geant->LoadGeometry("detp geometry ChainFlags[kFieldOn] field_off");
      }
      else {
	if (ChainFlags[kFZIN]) {
	  if (geant->SetInputFile(InFile->Data()) > kStOK) {
	    printf ("File %s cannot be opened. Exit! \n",InFile->Data());
	    gSystem->Exit(1);
	  }
	}
      }
    }
  }
  
  if (ChainFlags[kMINIDAQ]) {
    // defined for ChainFlags[kMINIDAQ]
    New("StMinidaqMaker","tpc_raw");
  }
  else {
    //  S I M U L A T I O N  or D A Q
    if (ChainFlags[kTDAQ])  {
      StMaker *tpcdaq = New("St_tpcdaq_Maker","tpc_raw"); 
      tpcdaq->SetMode(0); // daq
    }
    else {
      if (ChainFlags[kTRS]) {//		trs
	New("StTrsMaker");
        StMaker *tpcdaq = New("St_tpcdaq_Maker","tpc_raw");
	tpcdaq->SetMode(1); // trs
      }
      else { 
	if (ChainFlags[kTSS]) {//		tss
	  StMaker *tssMk 	= New("St_tss_Maker","tpc_raw");
	  if (tssMk && ChainFlags[kDEBUG]) tssMk->SetDebug();
	}
      }
    }
  }
  if (ChainFlags[kFSS]) {//		fss
    StMaker *fssMk 	= New("St_fss_Maker","ftpc_raw");
    if (fssMk && ChainFlags[kDEBUG]) fssMk->SetDebug();
  }
  
  if (ChainFlags[kEMS]) {//		emc
    StMaker *emsMk = New("St_ems_Maker","emc_raw" );
    if (emsMk && ChainFlags[kDEBUG]) emsMk->SetDebug();
  }
  if (ChainFlags[kEMC]) {
    StMaker *emcMk = New("St_emc_Maker","emc_hits");
    if (emcMk && ChainFlags[kDEBUG]) emcMk->SetDebug();
  }
  // L O C A L    R E C O N S T R U C T I O
  if (ChainFlags[kTCL]) {//		tcl
    tclMk = new St_tcl_Maker("tpc_hits");
    if (tclMk) {
      if (ChainFlags[kEval]) {//Additional switches
	tclMk->tclPixTransOn(); //Turn on flat adcxyz table
	tclMk->tclEvalOn(); //Turn on the hit finder evaluation
      }
      if (ChainFlags[kDEBUG]) tclMk->SetDebug();
    }
  }
  if (ChainFlags[kSRS]) {//		svt
    StMaker *srsMk 	= New("St_srs_Maker","svt_hits");
    if (srsMk && ChainFlags[kDEBUG]) srsMk->SetDebug();  
  }
  if(ChainFlags[kFCL]){//		fcl
    StMaker *fclMk 	= New("St_fcl_Maker","ftpc_hits");  
    if (fclMk && ChainFlags[kDEBUG]) fclMk->SetDebug();
  }
  // T R A C K I N G
  if (ChainFlags[kTPT]) {
    St_tpt_Maker *tptMk = (St_tpt_Maker *) New("St_tpt_Maker","tpc_tracks");
    if (tptMk) {
      if (ChainFlags[kMINIDAQ]) {
	tptMk->Set_final(kTRUE);// Turn on the final ntuple.
      }
      if (ChainFlags[kEval]) {//Additional switches
	tptMk->tteEvalOn();   //Turn on the tpc evaluation
	tptMk->tptResOn();    // Turn on the residual table
      }
      if (ChainFlags[kDEBUG]) tptMk->SetDebug();
    }
  }
  if (ChainFlags[kSTK]) {//		stk
    StMaker *stkMk 	= New("St_stk_Maker","svt_tracks");
    if (stkMk && ChainFlags[kDEBUG]) stkMk->SetDebug();
  }
  if (ChainFlags[kFPT]){//		fpt
    StMaker *fptMk 	= New("St_fpt_Maker","ftpc_tracks");
    if (fptMk && ChainFlags[kDEBUG]) fptMk->SetDebug();
  }
  // T R I G G E R
  if (ChainFlags[kTRG]) {
    if (ChainFlags[kCTF]) New("St_ctf_Maker","ctf");
    if (ChainFlags[kMWC]) New("St_mwc_Maker","mwc");
    New("St_trg_Maker","trg");
  }
  // G L O B A L chain
  if (ChainFlags[kGLOBAL]) {//		global
    StMaker *glbMk = new StMaker("global");
    //    SetInput("dst",".make/global/.data/dst");
    if (glbMk) {
      if (ChainFlags[kDEBUG]) glbMk->SetDebug();
      StMaker *saveMK = glbMk->cd();
      if (ChainFlags[kMATCH])   New("StMatchMaker");
      if (ChainFlags[kPRIMARY]) New("StPrimaryMaker");
      if (ChainFlags[kV0])      v0Mk      = new StV0Maker();
      if (ChainFlags[kXI])      New("StXiMaker");
      if (ChainFlags[kKINK])    New("StKinkMaker");
      saveMK->cd();
      if (v0Mk && ChainFlags[kEval]) {//Additional switches 
	v0Mk->ev0EvalOn();   //Turn on the ev0 evaluation  
      }
    }
  }
  if (ChainFlags[kL3T]) {//		l3t
    StMaker *l3tMk  = New("St_l3t_Maker","l3Tracks");
    if (l3tMk && ChainFlags[kDEBUG]) l3tMk->SetDebug();
  }
  if (ChainFlags[kGLOBAL]) {
  //		dst
    if (ChainFlags[kDST]){
      StMaker *dstMk = New("St_dst_Maker","dst");
      if (dstMk) {
	SetInput("dst",".make/dst/.data/dst");
	if (ChainFlags[kDEBUG]) dstMk->SetDebug();
      }
    }
    if (ChainFlags[kEVENT]){
      New("StEventMaker");
      if (ChainFlags[kANALYSIS]) {
	StMaker *anaMk = New("StAnalysisMaker");
	if (anaMk && ChainFlags[kDEBUG]) anaMk->SetDebug(0);
      }
    }
    if (ChainFlags[kQA]) New("St_QA_Maker");  
  }
  if (ChainFlags[kDISPLAY]) {
    New("St_geom_Maker"); // this maker open its own TFile !!
    New("StEventDisplayMaker");
  }  
  
  if (ChainFlags[kTREE]) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut->Data());
    if (treeMk) {
      treeMk->SetIOMode("w");
      if (ChainFlags[kDEBUG]) treeMk->SetDebug();
      if (ChainFlags[kDST]) {
	//  treeMk->SetBranch("dstBranch",FileOut->Data());
	treeMk->IntoBranch("dstBranch","dst");
	//      TString hist(FileOut);
	//      hist.ReplaceAll(".root",".hist.root");
	//      treeMk->SetBranch("histBranch",hist.Data());
      }
      else if (ChainFlags[kGLOBAL]) {
	//  treeMk->SetBranch("globalBranch",FileOut->Data());
	treeMk->IntoBranch("globalBranch","global/.data");
      }
      if (ChainFlags[kEVENT]){
	//  treeMk->SetBranch("EventBranch",FileOut->Data());
	treeMk->IntoBranch("eventBranch","StEvent");
      }
      if (ChainFlags[kAllEvent]) {
	if (geant) {
	  treeMk->IntoBranch("geantBranch","geant");
	  //  treeMk->SetBranch("geantBranch",FileOut->Data());
	  treeMk->IntoBranch("geantBranch","geant/.data/particle");
	  treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
	}
	if (ChainFlags[kFSS]) {
	  //  treeMk->SetBranch("ftpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
	}
	if (ChainFlags[kEMS]) {
	  //  treeMk->SetBranch("emc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
	}
	if (ChainFlags[kTSS] || ChainFlags[kTRS]) { 
	  //  treeMk->SetBranch("tpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
	}
	if (ChainFlags[kTCL]) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
	if (ChainFlags[kTPT]) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
	if (ChainFlags[kTRG]) {
	  //  treeMk->SetBranch("trgBranch",FileOut->Data());
	  treeMk->IntoBranch("trgBranch","ctf mwc trg");
	}
	if (ChainFlags[kL3T]) {
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
  // Create HTML docs of all Maker's inv#ifdef ChainFlags[kTRG]
  if (ChainFlags[kMakeDoc]) MakeDoc();
  return kStOk;
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  // define input file
  Char_t *Infile  = (Char_t *) infile;
  Char_t *Outfile = (Char_t *) outfile;
  if (!Infile) {
    if (ChainFlags[kMINIDAQ]) {
      Infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
      //Infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
      printf ("Use default input file %s for %s \n",Infile,ChainOptions[kMINIDAQ]);
    }
    else {
      if (ChainFlags[kFZIN]) {
	if (ChainFlags[kY1b]) Infile = "/disk0/star/test/venus412/b0_3/year_1b/psc0050_01_40evts.fzd";
	else {
	  if (ChainFlags[kY2a]) Infile = "/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fzd";
	  else                  Infile ="/disk1/star/test/psc0049_08_40evts.fzd";
	}
	printf ("Use default input file %s for %s \n",Infile,ChainOptions[kFZIN]);
      }
      else { 
	if (!ChainFlags[kGSTAR]) {
	  Infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
	  printf ("Use default input file %s for %s \n",Infile,ChainOptions[kXIN]);
	}
      }
    }
  }
  if (Infile) {
    InFile = new TString(Infile);
    if (gSystem->AccessPathName(InFile->Data())) {// file does not exist
      printf (" *** NO FILE: %s, exit!\n", InFile->Data());
      gSystem->Exit(1); 
    }
  }
  if (ChainFlags[kGSTAR]) {
    if (!Outfile) FileOut = new TString("gtrack.root");
    else          FileOut = new TString(Outfile);
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
  }
  else {
    if (Outfile) FileOut = new TString(Outfile);
    else {
      FileOut = new TString(gSystem->BaseName(InFile->Data()));
      FileOut->ReplaceAll(".daq",".root");
      FileOut->ReplaceAll(".fzd",".root");
      FileOut->ReplaceAll(".fz",".root");
      FileOut->ReplaceAll(".xdf",".root");
      FileOut->Strip();
    }
    printf ("==============================================\n");
    printf ("QAInfo:Input file name = %s\n"
	   ,InFile->Data());
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
    if (ChainFlags[kXOUT]) {
      XdfFile = new TString(FileOut->Data());
      XdfFile->ReplaceAll(".root",".dst.xdf");
      printf ("QAInfo:Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
    }
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
    if (!ChainFlags[kDEFAULT]) {
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
    if (!ChainFlags[kDEFAULT]) {
      SetOption(kEMS);
      SetOption(kEMC);
      SetOption(kRICH);
    }
  case  kY1a:
  case  kY1c:
    if (!ChainFlags[kDEFAULT]) {
      SetOption(kFTPC);
      SetOption(kTRG);
    }
  case kSD97:
  case kSD98:
  case kER99:
  case kES99:
    if (!ChainFlags[kDEFAULT]) {
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
    if (!ChainFlags[kDEFAULT]) {
      SetOption(kDEFAULT);
      SetOption(kSVT);
      SetOption(kEMS);
      SetOption(kEMC);
      SetOption(kTPC);
      SetOption(kFTPC);
      SetOption(kTRG);
      SetOption(kGLOBAL);
      SetOption(kDST);
      SetOption(kQA);
      SetOption(kEVENT);
      SetOption(kANALYSIS);
      SetOption(kTREE);
    }
    break;
  case kGSTAR:
    if (!ChainFlags[kDEFAULT]) {
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
