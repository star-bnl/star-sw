//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: BFC.C,v 1.5 1999/10/14 22:08:09 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include "TBrowser.h"
#include "TString.h"
#include "TRegexp.h"
#include "TSystem.h"
#include "StRoot/St_base/St_DataSet.h"
#include "StRoot/StChain/StChain.h"
#include "StRoot/xdf2root/St_XDFFile.h"
#include "StRoot/StMagF/StMagF.h"
#include "StRoot/StIOMaker/StIOMaker.h"
#include "StRoot/St_db_Maker/St_db_Maker.h"
#include "StRoot/St_geant_Maker/St_geant_Maker.h"
#include "StRoot/StMinidaqMaker/StMinidaqMaker.h"
#include "StRoot/St_tpcdaq_Maker/St_tpcdaq_Maker.h"
#include "StRoot/StTrsMaker/StTrsMaker.h"
#include "StRoot/St_tpcdaq_Maker/St_tpcdaq_Maker.h"
#include "StRoot/St_tss_Maker/St_tss_Maker.h"
#include "StRoot/St_fss_Maker/St_fss_Maker.h"
#include "StRoot/St_ems_Maker/St_ems_Maker.h"
#include "StRoot/St_emc_Maker/St_emc_Maker.h"
#include "StRoot/St_tcl_Maker/St_tcl_Maker.h"
#include "StRoot/St_srs_Maker/St_srs_Maker.h"

#endif
TBrowser *b = 0;
class StChain;        
StChain  *chain=0;
class StEvent;
StEvent *Event;
class StIOMaker; StIOMaker *inpMk=0;     
class St_XDFFile;     St_XDFFile     *xdf_out = 0; 
class St_geant_Maker; St_geant_Maker *geant   = 0;   
class St_db_Maker;    St_db_Maker    *dbMk    = 0; St_db_Maker *calibMk = 0;

class StMagFC;         StMagFC        *field   = 0;           

class St_tss_Maker;   St_tss_Maker   *tssMk   = 0;

class StTrsMaker;     StTrsMaker     *trs     = 0;
class St_tcl_Maker;   St_tcl_Maker   *tclMk   = 0;     
class St_tpt_Maker;   St_tpt_Maker   *tptMk   = 0;

class St_srs_Maker;   St_srs_Maker   *srsMk   = 0;
class St_stk_Maker;   St_stk_Maker   *stkMk   = 0;

class St_fss_Maker;   St_fss_Maker   *fssMk   = 0;     
class St_fcl_Maker;   St_fcl_Maker   *fclMk   = 0;
class St_fpt_Maker;   St_fpt_Maker   *fptMk   = 0;

class St_ems_Maker;   St_ems_Maker   *emsMk   = 0;    
class St_emc_Maker;   St_emc_Maker   *emcMk   = 0;    

class St_ctf_Maker;   St_ctf_Maker   *ctfMk   = 0;    
class St_mwc_Maker;   St_mwc_Maker   *mwcMk   = 0;    
class St_trg_Maker;   St_trg_Maker   *trgMk   = 0;    
class St_l3t_Maker;   St_l3t_Maker   *l3tMk   = 0;    

class StMaker;        StMaker        *glbMk   = 0;     
class StMatchMaker;   StMatchMaker   *matchMk = 0;
class StPrimaryMaker; StPrimaryMaker *primaryMk=0;
class StV0Maker;      StV0Maker      *v0Mk    = 0;
class StKinkMaker;    StKinkMaker    *kinkMk  = 0;
class StXiMaker;      StXiMaker      *xiMk    = 0;

class St_dst_Maker;   St_dst_Maker   *dstMk   = 0;     
class StEventMaker;   StEventMaker   *evMk    = 0;
class StAnalysisMaker;StAnalysisMaker *anaMk  = 0;
class St_QA_Maker;    St_QA_Maker    *qa      = 0;
class StTreeMaker;    StTreeMaker    *treeMk  = 0;
class StEventDisplayMaker; StEventDisplayMaker *disp = 0;
class St_geom_Maker;  St_geom_Maker  *geom    = 0;
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
Int_t NoEvents = 0;
//_____________________________________________________________________
enum EChainOptions { 
  kFIRST   ,
  kSD97    ,kSD98    ,kY1a     ,kY1b     ,kY1c     ,          // time stamps
  kES99    ,kER99    ,kY1d     ,kY1e     ,kY2a     ,
  kEval    ,kOFF     ,kXIN     ,kXOUT    ,kGSTAR   ,          // Chains, options
  kTDAQ    ,kFZIN    ,kGEANT   ,
  kFieldOn ,kFieldOff,kHalfField,                             // Magnetic Field
  kTPC     ,kTSS     ,kTRS     ,kMINIDAQ ,kTFS     ,kTCL     ,kTPT     ,// TPC
  kSVT     ,kSRS     ,kSTK     ,                              // SVT  
  kFTPC    ,kFSS     ,kFCL     ,kFPT     ,                    // FTPC
  kEMS     ,kEMC     ,                                        // EMC
  kTRG     ,kCTF     ,kMWC     ,kL3T     ,
  kRICH    ,                                                  // RICH
  kGLOBAL  ,kMATCH   ,kPRIMARY ,kV0      ,kXI      ,kKINK    ,// Global Chain
  kDST     ,kEVENT   ,kANALYSIS,kQA      ,                    // Dst
  kTREE    ,kAllEvent,kDISPLAY ,kLAST    ,                    // StEvent
  kDEFAULT ,
  kMakeDoc ,kDEBUG   ,kHIGZ
};
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
  "St_l2t_Maker   \tin Chain",
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
//_____________________________________________________________________
void SetOption(int k){// set all OFF

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
    SetOption(kXIN);
    SetOption(kER99);
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
    SetOption(kGEANT);
  case FZIN:
  case kXIN:
    if (!ChainFlags[kDEFAULT]) {
      printf ("QAInfo:no setup defined ==> use Y2a\n");
      SetOption(kY2a);
    }
    break;
  default:
    break;
  }
}
//_____________________________________________________________________
void SetOptionOff(Int_t k){// set all OFF
  SetOption(-k);
}
//_____________________________________________________________________
void SetChainOff(){// set all OFF
  for (Int_t k = kFIRST;k<NoChainOptions;k++)  ChainFlags[k] = kFALSE;
}
//_____________________________________________________________________
void SetDefaultChain(){// default for standard chain
  if (! ChainFlags[kDEFAULT]) {
    printf ("Set default options\n");
    for (Int_t k = kTPC; k<kAllEvent; k++) if (k != kTSS && k != kTRS) SetOption(k);
    SetOption(kDEFAULT);
  } 
}
//_____________________________________________________________________
void SetFlags(const Char_t *Chain="gstar tfs"){// parse Chain request
  SetChainOff();
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  printf ("==============================================\n");
  printf ("QAInfo:============= You are in %s ===============\n",STAR_VERSION.Data());
  Int_t k, kgo;
#ifdef __CINT__
  if (!Chain || !strlen(Chain)) {
    printf ("============= \tPossible Chain Options are: \n"); 
    for (Int_t k=kFIRST;k<NoChainOptions;k++)
      printf ("============ %2d\t:[-]%-8s\t:%s \n",k,ChainOptions[k],ChainComments[k]);
#if 0
    printf ("============= \tImportant two changes:
                           \tIt is required exact matching in Chain definition
                           \tAll Chain options set in supplyed order\n");
#endif
    printf ("============= \t U S A G E =============\n");
    printf ("\n
\tBFC(Int_t First, Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)
\tBFC(Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)
\tBFC(Char_t *Chain, Char_t *infile, Char_t *outfile)
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
\t root4star  BFC.C                   \t// Create this message
\t root4star 'BFC.C(1)'               \t// Run one event with default Chain=\"gstar tfs\"
\t root4star 'BFC.C(1,1)'             \t// the same
\t root4star 'BFC.C(2,40,\"y1b fzin\")'\t// run for configuration year_1b, 
\t                                    \t// reading /disk1/star/test/psc0050_01_40evts.fzd
\t                                    \t// skipping the 1-st event for the rest 39 events
\n");
    printf ("
\t root4star 'BFC.C(40,\"y1b fzin\",\"/disk1/star/test/psc0050_01_40evts.fzd\")'
\t root4star 'BFC.C(40,\"y1b fzin\")'\t// the same as  above
\t root4star 'BFC.C(2,40,\"y1b fzin -l3t\")'//the as above but remove L3T from chain
\t root4star 'BFC.C(40,\"y2a fzin\",\"/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fz\")'
\t root4star 'BFC.C(40,\"y2a fzin\")'\t// the same as  above
\t root4star 'BFC.C(5,10,\"y1b xin xout\",\"/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf\")'
\t                                    \t// skipping the 4 events for the rest 6 events
\t root4star 'BFC.C(1,\"off xin tpc FieldOff sd96 eval\",\"Mini_Daq.xdf\")'\t// the same as Chain=\"minidaq\"
\n");
    printf ("
\t root4star 'BFC.C(1,\"off tdaq tpc FieldOn\",\"/disk1/star/daq/990624.306.daq\")' 
\t \t//Cosmics (56) events with full magnetic field, TPC only 
\t root4star 'BFC.C(1,\"tdaq FieldOn\",\"/disk1/star/daq/990624.306.daq\")' 
\t \t//Cosmics (56) events with full magnetic field 
\t root4star 'BFC.C(1,\"tdaq HalfField\",\"/disk1/star/daq/990630.602.daq\")' 
\t \t//Laser (10) events with half magnetic field 
\t root4star 'BFC.C(1,\"tdaq FieldOff\",\"/disk1/star/daq/990701.614.daq\")' 
\t \t//Laser (12) events with no magnetic field 
\n");
    gSystem->Exit(1);
  }
#endif
  TString tChain(Chain);
  printf ("QAInfo:Requested chain is :\t%s\n",tChain.Data());
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
    for (k = kFIRST+1; k<NoChainOptions; k++) {
      opt = TString(ChainOptions[k]);
      opt.ToLower();
      if (!strstr(Tag.Data(),opt.Data())) continue;
      kgo = k;
      nopt = TString("-");
      nopt += opt;
      if (strstr(Tag.Data(),nopt.Data())) kgo = -k;
      if (k <= 0 || k >  NoChainOptions) 
	{printf ("Option %s unrecognized\n",ChainOptions[k]);}
      if (kgo<0) SetOption(-k);
      else       SetOption(k);
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
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  //  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("StDbBroker");
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
}
//_____________________________________________________________________
void Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0 ){
  // define input file
  if (!infile) {
    if (ChainFlags[kMINIDAQ]) {
      infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
      //infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
      printf ("Use default input file %s for %s \n",infile,ChainOptions[kMINIDAQ]);
    }
    else {
      if (ChainFlags[kFZIN]) {
	if (ChainFlags[kY1b]) infile = "/disk0/star/test/venus412/b0_3/year_1b/psc0050_01_40evts.fzd";
	else {
	  if (ChainFlags[kY2a]) infile = "/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fzd";
	  else                  infile ="/disk1/star/test/psc0049_08_40evts.fzd";
	}
	printf ("Use default input file %s for %s \n",infile,ChainOptions[kFZIN]);
      }
      else { 
	if (!ChainFlags[kGSTAR]) {
	  infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
	  printf ("Use default input file %s for %s \n",infile,ChainOptions[kXIN]);
	}
      }
    }
  }
  if (infile) {
    InFile = new TString(infile);
    if (gSystem->AccessPathName(InFile->Data())) {// file does not exist
      printf (" *** NO FILE: %s, exit!\n", InFile->Data());
      gSystem->Exit(1); 
    }
  }
  if (ChainFlags[kGSTAR]) {
    if (!outfile) FileOut = new TString("gtrack.root");
    else          FileOut = new TString(outfile);
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
  }
  else {
    if (outfile) FileOut = new TString(outfile);
    else {
      FileOut = new TString(gSystem->BaseName(InFile->Data()));
      FileOut->ReplaceAll(".daq",".root");
      FileOut->ReplaceAll(".fzd",".root");
      FileOut->ReplaceAll(".fz",".root");
      FileOut->ReplaceAll(".xdf",".root");
      FileOut->Strip();
    }
    printf ("==============================================\n");
    printf ("QAInfo:Input file name = %s with No. of Events to process = %i\n"
	   ,InFile->Data(),NoEvents);
    printf ("QAInfo:Output root file name %s\n", FileOut->Data());
    printf ("==============================================\n");
  }
  //    gSystem->Exit(1);
  
}
//_____________________________________________________________________
void BFC(const Int_t First,
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
  SetFlags(Chain);
  Set_IO_Files(infile,outfile);
  if (gClassTable->GetID("StChain") < 0) Load();
  // Create the main chain object
  if (!chain) delete chain;
  chain = new StChain("bfc");
  {
    TDatime t;
    printf ("QAInfo:Run is started at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
  }
  printf ("QAInfo:Run on %s in %s\n",
	  gSystem->HostName(),
	  gSystem->WorkingDirectory());
  printf ("QAInfo: with %s\n", chain->GetCVS());
  if (chain && ChainFlags[kDEBUG]) chain->SetDebug();
  //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb/params";
  //DbInit from StDbBroker.so checks that mysql db1 server is accessible
  //  if (StDbBroker::DbInit("params")==0) mainDB = "MySQL:params";
  printf ("QAInfo: Main DataBase == %s\n",mainDB);
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
    inpMk = new StIOMaker("inputStream","r",InFile->Data());
    chain->SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
    if (inpMk && ChainFlags[kDEBUG]) inpMk->SetDebug();
//VP    chain->SetInput("geant",".make/xdfin/.data/event/geant/Event");
  }
  if (ChainFlags[kGEANT])  {
    geant = new St_geant_Maker("geant");
    if (geant) {
      geant->SetNwGEANT(10000000);
      chain->SetInput("geant",".make/geant/.data");
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
//VP    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
//VP    chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
    StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
  }
  else {
    //  S I M U L A T I O N  or D A Q
    if (ChainFlags[kTDAQ])  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker("tpc_raw","daq");
    else {
      if (ChainFlags[kTRS]) {//		trs
	trs = new StTrsMaker;
	tpc_raw = new St_tpcdaq_Maker("tpc_raw","trs");
      }
      else { 
	if (ChainFlags[kTSS]) {//		tss
	  tssMk 	= new St_tss_Maker("tpc_raw");
	  if (tssMk && ChainFlags[kDEBUG]) tssMk->SetDebug();
	}
      }
    }
  }
  if (ChainFlags[kFSS]) {//		fss
    fssMk 	= new St_fss_Maker("ftpc_raw");
    if (fssMk && ChainFlags[kDEBUG]) fssMk->SetDebug();
  }
  
  if (ChainFlags[kEMS]) {//		emc
    emsMk = new St_ems_Maker("emc_raw" );
    if (emsMk && ChainFlags[kDEBUG]) emsMk->SetDebug();
  }
  if (ChainFlags[kEMC]) {
    emcMk = new St_emc_Maker("emc_hits");
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
    srsMk 	= new St_srs_Maker("svt_hits");
    if (srsMk && ChainFlags[kDEBUG]) srsMk->SetDebug();  
  }
  if(ChainFlags[kFCL]){//		fcl
    fclMk 	= new St_fcl_Maker("ftpc_hits");  
    if (fclMk && ChainFlags[kDEBUG]) fclMk->SetDebug();
  }
  // T R A C K I N G
  if (ChainFlags[kTPT]) {
    tptMk = new St_tpt_Maker("tpc_tracks");
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
    stkMk 	= new St_stk_Maker("svt_tracks");
    if (stkMk && ChainFlags[kDEBUG]) stkMk->SetDebug();
  }
  if (ChainFlags[kFPT]){//		fpt
    fptMk 	= new St_fpt_Maker("ftpc_tracks");
    if (fptMk && ChainFlags[kDEBUG]) fptMk->SetDebug();
  }
  // T R I G G E R
  if (ChainFlags[kTRG]) {
    if (ChainFlags[kCTF]) ctfMk      = new St_ctf_Maker("ctf");
    if (ChainFlags[kMWC]) mwcMk      = new St_mwc_Maker("mwc");
    trgMk      = new St_trg_Maker("trg");
  }
  // G L O B A L chain
  if (ChainFlags[kGLOBAL]) {//		global
    glbMk = new StMaker("global");
    //    chain->SetInput("dst",".make/global/.data/dst");
    if (glbMk) {
      if (ChainFlags[kDEBUG]) glbMk->SetDebug();
      StMaker *saveMK = glbMk->cd();
      if (ChainFlags[kMATCH])   matchMk   = new StMatchMaker();
      if (ChainFlags[kPRIMARY]) primaryMk = new StPrimaryMaker();
      if (ChainFlags[kV0])      v0Mk      = new StV0Maker();
      if (ChainFlags[kXI])      xiMk      = new StXiMaker();
      if (ChainFlags[kKINK])    kinkMk    = new StKinkMaker();
      saveMK->cd();
      if (v0Mk && ChainFlags[kEval]) {//Additional switches 
	v0Mk->ev0EvalOn();   //Turn on the ev0 evaluation  
      }
    }
  }
  if (ChainFlags[kL3T]) {//		l3t
    l3tMk  = new St_l3t_Maker("l3Tracks");
    if (l3tMk && ChainFlags[kDEBUG]) l3tMk->SetDebug();
  }
  if (ChainFlags[kGLOBAL]) {
  //		dst
    if (ChainFlags[kDST]){
      dstMk = new St_dst_Maker("dst");
      if (dstMk) {
	chain->SetInput("dst",".make/dst/.data/dst");
	if (ChainFlags[kDEBUG]) dstMk->SetDebug();
      }
    }
    if (ChainFlags[kEVENT]){
      evMk  = new StEventMaker;
      if (ChainFlags[kANALYSIS]) {
	anaMk = new StAnalysisMaker;
	if (anaMk && ChainFlags[kDEBUG]) anaMk->SetDebug(0);
      }
    }
    if (ChainFlags[kQA]) qa = new St_QA_Maker;  
  }
  if (ChainFlags[kDISPLAY]) {
    geom = new St_geom_Maker; // this maker open its own TFile !!
    disp = new StEventDisplayMaker;
  }  
  
  if (ChainFlags[kTREE]) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut->Data());
    if (treeMk) {
      treeMk->SetIOMode("w");
      if (ChainFlags[kDEBUG]) treeMk->SetDebug();
      if (dstMk) {
	//  treeMk->SetBranch("dstBranch",FileOut->Data());
	treeMk->IntoBranch("dstBranch","dst");
	//      TString hist(FileOut);
	//      hist.ReplaceAll(".root",".hist.root");
	//      treeMk->SetBranch("histBranch",hist.Data());
      }
      else if (glbMk) {
	//  treeMk->SetBranch("globalBranch",FileOut->Data());
	treeMk->IntoBranch("globalBranch","global/.data");
      }
      if (evMk){
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
	if (fssMk) {
	  //  treeMk->SetBranch("ftpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
	}
	if (emsMk) {
	  //  treeMk->SetBranch("emc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
	}
	if (ChainFlags[kTSS] || ChainFlags[kTRS]) { 
	  //  treeMk->SetBranch("tpc_rawBranch",FileOut->Data());
	  treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
	}
	if (tclMk) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
	if (tptMk) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
	if (ChainFlags[kTRG]) {
	  //  treeMk->SetBranch("trgBranch",FileOut->Data());
	  treeMk->IntoBranch("trgBranch","ctf mwc trg");
	}
	if (l3tMk) {
	  //  treeMk->SetBranch("l3TBranch",FileOut->Data());
	  treeMk->IntoBranch("l3tBranch","l3Tracks");
	}
      }      
      treeMk->SetBranch("histBranch");
    }
  }
  chain->PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef ChainFlags[kTRG]
  if (ChainFlags[kMakeDoc]) chain->MakeDoc();
  
  // Init the chain and all its makers
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  if (ChainFlags[kXOUT] && FileOut) {
    XdfFile = new TString(FileOut->Data());
    XdfFile->ReplaceAll(".root",".dst.xdf");
    xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
    printf ("Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
  }
  // skip if any
  if (geant && First > 0) geant->Skip(First-1);
  if (inpMk && First > 1) inpMk->Skip(First-1);
  Int_t iMake = 0;
  TBenchmark evnt;
  for (Int_t i = First; i <= NoEvents; i++){
    evnt->Reset();
    evnt->Start("QAInfo:");
    chain->Clear();
    iMake = chain->Make(i);
    if (iMake <kStEOF && xdf_out){
      St_DataSet *dstSet = chain->GetInputDS("dst");
      if (dstSet) xdf_out->NextEventPut(dstSet); // xdf output
    }
    //    gSystem->Exec("ps ux");
    evnt->Stop("QAInfo:");
    evnt->Show("QAInfo:");
    printf ("QAInfo: Done with Event no. %d (%d) Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	    i,iMake,evnt->GetRealTime("QAInfo:"),evnt->GetCpuTime("QAInfo:"));
    if (iMake>=kStEOF) break;
  }
  if (NoEvents > 1) {
    chain->Finish();
    if (xdf_out) delete xdf_out;
    fflush(stdout);
    printf ("QAInfo:Run completed ");
    gSystem->Exec("date");
  }
  else {
    if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");
  }

  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
  }

}
//_____________________________________________________________________
void BFC (const Int_t Nevents, 
	  const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{
  BFC(1,Nevents,Chain,infile,outfile);
}
//_____________________________________________________________________
void BFC (const Char_t *Chain="",Char_t *infile=0, Char_t *outfile=0)
{
  BFC(1,1,Chain,infile,outfile);
}
