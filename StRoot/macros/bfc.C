//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.82 1999/07/09 18:57:18 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include "TBrowser.h"
#include "TString.h"
#include "TSystem.h"
#include "St_XDFFile.h"
#endif
TBrowser *b = 0;
class StChain;        
StChain  *chain=0;
class StEvent;
StEvent *Event;
class St_xdfin_Maker; St_xdfin_Maker *xdfMk=0;     
class St_XDFFile;     St_XDFFile     *xdf_out = 0; 
class St_geant_Maker; St_geant_Maker *geant   = 0;   
class St_db_Maker;    St_db_Maker    *dbMk    = 0; St_db_Maker    *dbMktpc = 0;
class StMagF;         StMagF         *field   = 0;           
class St_fss_Maker;   St_fss_Maker   *fssMk   = 0;     
class St_tcl_Maker;   St_tcl_Maker   *tclMk   = 0;     
class St_tpt_Maker;   St_tpt_Maker   *tptMk   = 0;
class St_ems_Maker;   St_ems_Maker   *emsMk   = 0;    
class St_l3t_Maker;   St_l3t_Maker   *l3tMk   = 0;    
class StMaker;        StMaker        *glbMk   = 0;     
class StMatchMaker;   StMatchMaker   *matchMk = 0;
class StPrimaryMaker; StPrimaryMaker *primaryMk = 0;
class StV0Maker;      StV0Maker      *v0Mk = 0;
class StKinkMaker;    StKinkMaker    *kinkMk = 0;
class StXiMaker;      StXiMaker      *xiMk = 0;
class St_dst_Maker;   St_dst_Maker   *dstMk   = 0;     
class StEventMaker;   StEventMaker   *evMk    = 0;
class StTreeMaker;    StTreeMaker    *treeMk  = 0;
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
Int_t NoEvents = 0;
//_____________________________________________________________________
enum EChainOptions { 
  kFIRST   ,
  kSD97    ,kSD98    ,kY1a     ,kY1b     ,kY1c     ,kES99     ,kER99    ,kY1d     ,
  kY1e     ,kY2a     ,kEval    ,kOFF     ,
  kXINDF   ,kXOUTDF  ,kGSTAR   ,kMINIDAQ ,kTDAQ    ,kFZIN     ,kGEANT   ,kCTEST   ,
  kField_On,kNo_Field,kHalfField,kTPC     ,kTSS     ,kTRS     ,kTFS      ,kFPC     ,
  kFSS     ,kEMC     ,kCTF     ,kL3      ,kRICH    ,kSVT      ,kGLOBAL  ,
  kDST     ,kSQA     ,kEVENT   ,kANALYS  ,kTREE    ,kAllEvent ,kLAST    ,kDefault
};
Char_t  *ChainOptions[] = {
 "FIRST"   ,
 "sd97"    ,"sd98"   ,"Y1a"    ,"Y1b"    ,"Y1c"    ,"es99"    ,"er99"   ,"Y1d"    ,
 "Y1e"     ,"Y2a"    ,"Eval"   ,"OFF"    ,
 "XIN"     ,"XOUT"   ,"GSTAR"  ,"MINIDAQ","TDAQ"   ,"FZIN"    ,"GEANT"  ,"CTEST"  ,
 "FieldOn" ,"NoField","HalfField","TPC"    ,"TSS"    ,"TRS"    ,"TFS"     ,"FPC"    ,
 "FSS"     ,"EMC"    ,"CTF"    ,"L3"     ,"RICH"   ,"SVT"     ,"GLOBAL" ,
 "DST"     ,"SQA"    ,"EVENT"  ,"ANALYS" ,"TREE"   ,"AllEvent","LAST"   ,"Default"
};
UChar_t  ChainFlags[] = {
  kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE    ,kFALSE   ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE    ,kFALSE   ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE    ,kFALSE   ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE    ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE    ,kFALSE   ,kFALSE  
};
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
  "Run minidaq chain",
  "TPC DAQ chain",
  "read GSTAR fz-file",
  "initailize GEANT",
  "test DB with MINIDAQ constans",
  "Use nominal STAR field",
  "No Field option",
  "Half Field option",
  "TPC in chain",
  "TPC with TSS",
  "TPC with TRS",
  "TPC with TFS",
  "FTPC in chain",
  "FTPC with FSS",
  "EMC in chain",
  "CTF,MWC & TRG in chain",
  "L3 in chain",
  "RICH in chain",
  "SVT in chain",
  "GLOBAL in chain",
  "DST in chain",
  "STAR QA in chain",
  "StEvent in chain",
  "Analysis with StEvent in chain",
  "write event to StTree",
  "Write whole event to StTree",
  "Nothing to comment",
  "Default Set has been set"
};
//_____________________________________________________________________
void SetOptionOff(EChainOptions k){// set all OFF
  ChainFlags[k] = kFALSE;
  printf (" Switch Off %s\n", ChainOptions[k]);
}
//_____________________________________________________________________
void SetOption(EChainOptions k){// set all OFF
  ChainFlags[k] = kTRUE;
  printf (" Switch On  %s\n", ChainOptions[k]);
}
//_____________________________________________________________________
void SetChainOff(){// set all OFF
  for (EChainOptions k = kFIRST;k<=kLAST;k++) SetOptionOff(k);
}
//_____________________________________________________________________
void SetDefaultChain(){// default for standard chain
  if (! ChainFlags[kDefault]) {
    printf ("Set default options\n");
    for (EChainOptions k = kTPC; k<kAllEvent; k++) if (k != kTSS && k != kTRS) SetOption(k);
    SetOption(kDefault);
  } 
}
//_____________________________________________________________________
void SetFlags(const Char_t *Chain="gstar tfs"){// parse Chain request
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  if (!strcmp("SL99c",STAR_VERSION.Data())) SetOption(kEVENT); 
  printf ("==============================================\n");
  printf ("============= You are in %s ===============\n",STAR_VERSION.Data());
  Int_t k, kgo;
  if (!Chain || !strlen(Chain)) {
    printf ("============= \t U S A G E =============\n");
    printf ("
\tbfc(Int_t First, Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)
\tbfc(Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)
\tbfc(Char_t *Chain, Char_t *infile, Char_t *outfile)
where
\t First   \t- First event to process \t(Default = 1)
\t Nevents \t- Total No. of events    \t(Default = 1)
\t Chain   \t- Chain specification    \t(without First &  Nevents: Default is \"\" which gives this message)
\t         \t                         \t with    First || Nevents: Default is \"gstar tfs\")
\t infile  \t- Name of Input file     \t(Default = 0, i.e. use preset file names depending on Chain)
\t outfile \t- Name of Output file    \t(Default = 0, i.e. define Output file name from Input one)
\n");
    printf ("
Examples:
\t root4star  bfc.C                   \t// Create this message
\t root4star 'bfc.C(1)'               \t// Run one event with default Chain=\"gstar tfs\"
\t root4star 'bfc.C(1,1)'             \t// the same
\t root4star 'bfc.C(2,40,\"y1a fzin\")'\t// run for configuration year_1a, 
\t                                    \t// reading /disk1/star/test/psc0049_08_40evts.fzd
\t                                    \t// skipping the 1-st event for the rest 39 events
\t root4star 'bfc.C(2,40,\"y1a fzin -l3\")'\t// the as above but remove L3 from chain
\t root4star 'bfc.C(1,\"off xin tpc No_Field sd96 eval\",\"Mini_Daq.xdf\")'\t// the same as Chain=\"minidaq\"
\t root4star 'bfc.C(2,40,\"y1a fzin\",\"/disk1/star/test/psc0049_08_40evts.fzd\")'\t// the same
\t root4star 'bfc.C(5,10,\"y1a xin xout\",\"/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf\")'
\t                                    \t// skipping the 4 events for the rest 6 events
\n");
    printf ("
\t root4star 'bfc.C(1,\"off tdaq tpc FieldOn\",\"/disk1/star/daq/990624.306.daq\")' 
\t \t//Cosmics (56) events with full magnetic field 
\t root4star 'bfc.C(1,\"off tdaq tpc HalfField\",\"/disk1/star/daq/990630.602.daq\")' 
\t \t//Laser (10) events with half magnetic field 
\t root4star 'bfc.C(1,\"off tdaq tpc NoField\",\"/disk1/star/daq/990701.614.daq\")' 
\t \t//Laser (12) events with no magnetic field 
\n");
                        
    printf ("============= \tPossible Chain Options are: \n"); 
    for (k=kFIRST;k<kLAST;k++) printf ("============ %2d \t[-]%s   \t:%s \n",
				       k,ChainOptions[k],ChainComments[k]);
    printf ("============= \tImportant two changes:
                           \tIt is required exact matching in Chain definition
                           \tAll Chain options set in supplyed order\n");
    gSystem->Exit(1);
  }
  TString tChain(Chain);
  tChain.ToLower(); //printf ("Chain %s\n",tChain.Data());
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  const Ssiz_t kNPOS        = ~(Ssiz_t)0;
  TString Tag, opt, nopt;
  while ( (begin < tChain.Length()) && (index != kNPOS) ) { // loop over given Chain options 
    index = tChain.Index(separator,&end,begin);
    if (index >= 0) {
      Tag = tChain(index,end);
    }
    begin += end+1;
    for (k = kFIRST+1; k<kLAST; k++) {
      opt = TString(ChainOptions[k]);
      opt.ToLower();
      if (!strstr(Tag.Data(),opt.Data())) continue;
      kgo = k;
      nopt = TString("-");
      nopt += opt;
      if (strstr(Tag.Data(),nopt.Data())) kgo = -k;
      
      switch (kgo) {
      case kOFF:
	SetOption(kDefault);
	break;
      case kMINIDAQ:
	SetOption(kDefault);
	SetOption(kXINDF);
	SetOption(kMINIDAQ);
	SetOption(kTPC);
	SetOption(kNo_Field);
	SetOption(kSD97);
	SetOption(kEval);
	break;
      case kTDAQ:
	SetOption(kDefault);
	SetOption(kY1a);
	SetOption(kTDAQ);
	SetOption(kTPC);
	SetOption(kER99);
	break;
      case  kY1b:
	SetOption(kEMC);
	SetOption(kRICH);
      case kSD97:
      case kSD98:
      case  kY1a:
      case kES99:
      case kER99:
      case  kY1c:
	SetOption(kDefault);
	SetOption(k);
	SetOption(kTPC);
	SetOption(kFPC);
	SetOption(kCTF);
	SetOption(kGLOBAL);
	SetOption(kDST);
	SetOption(kSQA);
	SetOption(kEVENT);
	SetOption(kANALYS);
	break;
      case  kY2a:
	SetOption(kDefault);
	SetOption(kY2a);
	SetDefaultChain();
	break;
      case kGSTAR:
	if (!ChainFlags[kDefault]) {
	  printf ("no setup defined ==> use Y2a\n");
	  SetOption(kY2a);
	  SetDefaultChain(); 
	}
	SetOption(kGEANT);
	SetOption(kGSTAR);
	break;
      case kTSS:
	if (!ChainFlags[kDefault]) {
	  SetDefaultChain();
	}
	SetOption(k);
	SetOptionOff(kTRS);
	break;
      case -kTSS:
	if (!ChainFlags[kDefault]) {
	  SetDefaultChain();
	}
	SetOptionOff(kTSS);
	SetOption(kTRS);
	break;
      case kTRS:
	if (!ChainFlags[kDefault]) {
	  SetDefaultChain();
	}
	SetOptionOff(kTSS);
	SetOption(kTRS);
	break;
      case -kTRS:
	if (!ChainFlags[kDefault]) {
	  SetDefaultChain();
	}
	SetOption(kTSS);
	SetOptionOff(kTRS);
	break;
      case  kTFS:
	if (!ChainFlags[kDefault]) {
	  SetDefaultChain();
	}
	SetOption(kTFS);
	SetOptionOff(kTSS);
	SetOptionOff(kTRS);
	SetOptionOff(kFSS);
	break;
      default:
	if (k <= 0 || k > kLAST ) {printf ("Option %s unrecognized\n",ChainOptions[k]);}
	if (kgo<0) SetOptionOff(k);
	else       SetOption(k);
      }
    }
  }
  // Check flags consistency   
  if (!ChainFlags[kXINDF] && !ChainFlags[kGSTAR] &&!ChainFlags[kTDAQ]) {
    SetOption(kFZIN);
    SetOption(kGEANT);
  }
  if (!ChainFlags[kGEANT] && !ChainFlags[kNo_Field] && 
      !ChainFlags[kHalfField] && !ChainFlags[kField_On]) { 
    SetOption(kField_On); 
  }
  if (ChainFlags[kAllEvent]) {
    SetOption(kEval); 
  }
  SetOptionOff(kL3);
  // Print set values
  for (k = kFIRST; k<kLAST;k++) {
    if (ChainFlags[k]) {
      printf ("================== %2d \t%s      \tis ON \t:%s \n",k,ChainOptions[k],ChainComments[k]);
    }
  }
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void Load(const Char_t *Chain="gstar tfs"){
  SetFlags(Chain);
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  //  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_db_Maker");
  if (ChainFlags[kXINDF]) gSystem->Load("St_xdfin_Maker");
  if (ChainFlags[kNo_Field] || ChainFlags[kField_On] || ChainFlags[kHalfField] ) gSystem->Load("StMagF");
  else        gSystem->Load("geometry");
  gSystem->Load("StarClassLibrary");
  if (ChainFlags[kTPC]) {
    gSystem->Load("St_tpc");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    if (ChainFlags[kTDAQ]) {
      gSystem->Load("StDaqLib");
      gSystem->Load("St_tpcdaq_Maker");
    }
    else {
      if (ChainFlags[kTRS]) {
	gSystem->Load("StTrsMaker"); 
	gSystem->Load("StDaqLib");
	gSystem->Load("St_tpcdaq_Maker");
      }
      else {if  (ChainFlags[kTSS])gSystem->Load("St_tss_Maker");}
    }
  }
  if (ChainFlags[kMINIDAQ]) gSystem->Load("StMinidaqMaker");
  if (ChainFlags[kGEANT])  {
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
  }
  if (ChainFlags[kFPC]) {
    gSystem->Load("St_ftpc");
    if (ChainFlags[kFSS]) gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
  }
  if (ChainFlags[kEMC]) {
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
#if 0
    gSystem->Load("St_emc_Maker");
#endif
  }
  if (ChainFlags[kCTF]) {
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
  }
  if (ChainFlags[kL3]){
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
  }
  if (ChainFlags[kRICH]) {
    gSystem->Load("StRchMaker");
  }
  if (ChainFlags[kSVT] || ChainFlags[kGLOBAL]) {
    gSystem->Load("St_svt");
  }
  if (ChainFlags[kSVT]) {
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
  }
  if (ChainFlags[kGLOBAL]) {
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    if (ChainFlags[kEVENT]) {
      gSystem->Load("StEvent");
      gSystem->Load("StEventMaker");
      if (ChainFlags[kANALYS]) gSystem->Load("StAnalysisMaker");
    }
    gSystem->Load("St_QA_Maker");
  }
  if (ChainFlags[kTREE]) gSystem->Load("StTreeMaker");
}
//_____________________________________________________________________
void Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0 ){
  // define input file
  if (!infile) {
    if (ChainFlags[kMINIDAQ])
      infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
    //infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
    else {
      if (ChainFlags[kFZIN])  
	infile ="/disk1/star/test/psc0049_08_40evts.fzd";                     // zebra file
      //infile = "/afs/rhic/star/tpc/data/trs_muon_10cmdrift_good.fzd";
      else 
	if (!ChainFlags[kGSTAR]) 
	  infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
    }
  }
  if (infile) {
    InFile = new TString(infile);
    if (gSystem->AccessPathName(InFile->Data())) {// file does not exist
      printf(" *** NO FILE: %s, exit!\n", InFile->Data());
      gSystem->Exit(1); 
    }
  }
  if (ChainFlags[kGSTAR]) {
    if (!outfile) FileOut = new TString("gtrack.root");
    else          FileOut = new TString(outfile);
    printf("Output root file name %s\n", FileOut->Data());
    printf("==============================================\n");
  }
  else {
    if (outfile) FileOut = new TString(outfile);
    else {
      FileOut = new TString(gSystem->BaseName(InFile->Data()));
      FileOut->ReplaceAll(".fzd",".root");
      FileOut->ReplaceAll(".fz",".root");
      FileOut->ReplaceAll(".xdf",".root");
      FileOut->Strip();
    }
    printf("==============================================\n");
    printf("Input file name = %s with No. of Events to process = %i\n"
	   ,InFile->Data(),NoEvents);
    printf("Output root file name %s\n", FileOut->Data());
    printf("==============================================\n");
  }
}
//_____________________________________________________________________
void bfc (const Int_t First,
	  const Int_t Nevents=1,
	  const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Chain = "" || "xdf" run STANDARD chain using xd-files as an input
  // Chain = "minidaq" read miniDAQ xdf file and process 
  NoEvents = Nevents;
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load(Chain);
  Set_IO_Files(infile,outfile);
  // Create the main chain object
  if (!chain) delete chain;
  chain = new StChain("bfc");
  printf ("Run chain version %s on %s in %s\n",
	  chain->GetCVSTag(),
	  gSystem->HostName(),
	  gSystem->WorkingDirectory());
  chain->SetDebug();

//  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb/params";
  dbMk = new St_db_Maker("db",mainDB);
  dbMk->SetDebug();
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
  printf ("db Maker set time = %d %d \n",dbMk->GetDateTime().GetDate(),
	                                 dbMk->GetDateTime().GetTime());
  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  calibMk->SetDebug();  
  if (ChainFlags[kXINDF]) {
    xdfMk->SetDebug();
    xdfMk = new St_xdfin_Maker("xdfin",InFile->Data());
    chain->SetInput("geant",".make/xdfin/.data/event/geant/Event");
  }
  if (ChainFlags[kMINIDAQ]) {// defined for ChainFlags[kMINIDAQ]
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  }
  if (ChainFlags[kGEANT]) {
    geant = new St_geant_Maker("geant");
    geant->SetNwGEANT(10000000);
    chain->SetInput("geant",".make/geant/.data");
    //  geant->SetIwtype(1);
    //  geant->SetDebug();
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
      //  geant->Do("debug on;");
      geant->Do("swit 2 3;");
      // geant->LoadGeometry("detp geometry ChainFlags[kField_On] field_off");
    }
    if (ChainFlags[kFZIN]) {
      geant->SetInputFile(InFile->Data());
      if (First > 0) geant->Skip(First-1);
    }
  }
  if (ChainFlags[kNo_Field]) field   = new StMagFC("field","STAR no field",0.00002);
  if (ChainFlags[kField_On]) field   = new StMagFC("field","STAR Normal field",1.);
  if (ChainFlags[kHalfField])field   = new StMagFC("field","STAR Normal field",0.5);
  //  S I M U L A T I O N  or D A Q
  if (ChainFlags[kMINIDAQ]) {
    StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
    if (dbMktpc) {
      cout<<"initializing input for the tpc DB"<<endl;
      //      tpc_raw->SetInput("params","tpcdb:params"); 
    }
  }
  else {
    if (ChainFlags[kTDAQ])  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker("tpc_raw",InFile->Data());
    else {
      if (ChainFlags[kTRS]) {//		trs
	StTrsMaker   *trs = new StTrsMaker;
	if (dbMktpc) {
	  cout<<"initializing input for the trs DB"<<endl;
	  //	trs->SetInput("params","tpcdb:params");
	}
	St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker("tpc_raw",0);
      }
    }
    else { 
      if (ChainFlags[kTSS]) {//		tss
	St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
	if (dbMktpc) {
	  cout<<"initializing input for the tpc DB"<<endl;
	  //	  tssMk->SetInput("params","tpcdb:params"); 
	}
	tssMk->SetDebug();
      }
    }
  }
  if (ChainFlags[kFPC] && ChainFlags[kFSS]) {//		fss
    St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
    fssMk->SetDebug();
  }
  
  if (ChainFlags[kEMC]) {//		emc
    emsMk = new St_ems_Maker("emc_raw" );
    emsMk->SetDebug();
#if 0
    St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
    emcMk->SetDebug();
#endif 
  }
  // L O C A L    R E C O N S T R U C T I O
  if (ChainFlags[kTPC]) {//		tcl
    tclMk = new St_tcl_Maker("tpc_hits");
    if (dbMktpc){
      cout<<"initializing input for the tpc DB"<<endl;
      //      tclMk->SetInput("params","tpcdb:params");  
    }
    if (ChainFlags[kEval]) {//Additional switches
      tclMk->tclPixTransOn(); //Turn on flat adcxyz table
      tclMk->tclEvalOn(); //Turn on the hit finder evaluation
    }
    tclMk->SetDebug();
    if (ChainFlags[kSVT]) {//		svt
      St_srs_Maker *srsMk 	= new St_srs_Maker("svt_hits");
      srsMk->SetDebug();  
    }
    if(ChainFlags[kFPC]){//		fcl
      St_fcl_Maker *fclMk 	= new St_fcl_Maker("ftpc_hits");  
      fclMk->SetDebug();
    }
  }
  // T R A C K I N G
  if (ChainFlags[kTPC]) {
    tptMk = new St_tpt_Maker("tpc_tracks");
    if (ChainFlags[kMINIDAQ]) {
      tptMk->Set_final(kTRUE);// Turn on the final ntuple.
    }
    if (ChainFlags[kEval]) {//Additional switches
      tptMk->tteEvalOn();   //Turn on the tpc evaluation
      tptMk->tptResOn();    // Turn on the residual table
    }
    tptMk->SetDebug();
  }
  if (ChainFlags[kSVT]) {//		stk
    St_stk_Maker *stkMk 	= new St_stk_Maker("svt_tracks");
    stkMk->SetDebug();
  }
  if (ChainFlags[kFPC]){//		fpt
    St_fpt_Maker *fptMk 	= new St_fpt_Maker("ftpc_tracks");
    fptMk->SetDebug();
  }
  // T R I G G E R
  if (ChainFlags[kCTF]) {
    St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf");
    St_mwc_Maker         *mwc      = new St_mwc_Maker("mwc");
    St_trg_Maker         *trg      = new St_trg_Maker("trg");
  }
  // G L O B A L chain
  if (ChainFlags[kGLOBAL]) {//		global
    glbMk = new StMaker("global");
    //    chain->SetInput("dst",".make/global/.data/dst");
    glbMk->SetDebug();
    StMaker *saveMK = glbMk->cd();
    matchMk   = new StMatchMaker();
    primaryMk = new StPrimaryMaker();
    v0Mk      = new StV0Maker();
    xiMk      = new StXiMaker();
    kinkMk    = new StKinkMaker();
    saveMK->cd();
    if (ChainFlags[kEval]) {//Additional switches 
      v0Mk->ev0EvalOn();   //Turn on the ev0 evaluation  
    }
  }
  if (ChainFlags[kL3]) {//		l3t
    l3tMk  = new St_l3t_Maker("l3Tracks");
    l3tMk->SetDebug();
  }
  if (ChainFlags[kGLOBAL]) {
  //		dst
    if (ChainFlags[kDST]){
      dstMk = new St_dst_Maker("dst");
      chain->SetInput("dst",".make/dst/.data/dst");
      dstMk->SetDebug();
    }
    if (ChainFlags[kEVENT]){
      evMk  = new StEventMaker;
      if (ChainFlags[kANALYS]) {
	StAnalysisMaker *anaMk = new StAnalysisMaker;
	anaMk->SetDebug(0);
      }
    }
    if (ChainFlags[kSQA]) St_QA_Maker *qa = new St_QA_Maker;  
    if (ChainFlags[kSQA]) St_QA_Maker *qa = new St_QA_Maker;  
  }
  if (ChainFlags[kTREE]) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut.Data());
    treeMk->SetIOMode("w");
    treeMk->SetDebug();
    if (dstMk) {
      //  treeMk->SetBranch("dstBranch",FileOut.Data());
      treeMk->IntoBranch("dstBranch","dst");
    }
    else if (glbMk) {
      //  treeMk->SetBranch("globalBranch",FileOut.Data());
      treeMk->IntoBranch("globalBranch","global/.data");
    }
    if (evMk){
      //  treeMk->SetBranch("EventBranch",FileOut.Data());
      treeMk->IntoBranch("eventBranch","StEvent");
    }
    if (ChainFlags[kAllEvent]) {
      if (geant) {
	treeMk->IntoBranch("geantBranch","geant");
	//  treeMk->SetBranch("geantBranch",FileOut.Data());
	//    treeMk->IntoBranch("geantBranch","geant/.data/particle");
	//    treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
      }
      if (fssMk) {
	//  treeMk->SetBranch("ftpc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
      }
      if (emsMk) {
	//  treeMk->SetBranch("emc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
      }
      if (ChainFlags[kTSS] || ChainFlags[kTRS]) { 
	//  treeMk->SetBranch("tpc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
      }
      if (tclMk) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
      if (tptMk) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
      if (ChainFlags[kCTF]) {
	//  treeMk->SetBranch("trgBranch",FileOut.Data());
	treeMk->IntoBranch("trgBranch","ctf mwc trg");
      }
      if (l3tMk) {
	//  treeMk->SetBranch("l3TBranch",FileOut.Data());
	treeMk->IntoBranch("l3tBranch","l3Tracks");
      }
    }      
    treeMk->SetBranch("histBranch");
  }
  chain->PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef ChainFlags[kCTF]
  // chain->MakeDoc();
  
  // Init the chain and all its makers
  //  chain->SetDebug(1);
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  if (ChainFlags[kXOUTDF] && FileOut) {
    XdfFile = new TString(FileOut->Data());
    XdfFile->ReplaceAll(".root","_dst.xdf");
    xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
    printf ("Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
  }
  // skip if any
  if (xdfMk && First > 1) xdfMk->Skip(First-1);
  Int_t iMake = 0;
  for (Int_t i = First; i <= NoEvents; i++){
    chain->Clear();
    iMake = chain->Make(i);
    if (iMake <kStEOF && xdf_out){
      St_DataSet *dstSet = chain->GetInputDS("dst");
      if (dstSet) xdf_out->NextEventPut(dstSet); // xdf output
    }
    //    gSystem->Exec("ps ux");
    printf ("=============== Done with Event no. %d (%d)\n",i,iMake);
    if (iMake>=kStEOF) break;
  }
  
  if (NoEvents > 1) {
    chain->Finish();
    if (xdf_out) delete xdf_out;
    printf ("Run completed ");
    gSystem->Exec("date");
  }
  else {if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");}
  //  else {b = new TBrowser("BFC chain",chain);}
}
//_____________________________________________________________________
void bfc (const Int_t Nevents, 
	  const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{
  bfc(1,Nevents,Chain,infile,outfile);
}
//_____________________________________________________________________
void bfc (const Char_t *Chain="",Char_t *infile=0, Char_t *outfile=0)
{
  bfc(1,1,Chain,infile,outfile);
}
