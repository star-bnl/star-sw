//_____________________________________________________________________
#include "TROOT.h"
#include "TString.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "TSystem.h"
#include "StBFChain.h"
#include "StEvtHddr.h"
#include "StChain.h"
#include "St_XDFFile.h"
#include "StMagF/StMagF.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "StDbBroker/StDbBroker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StMessMgr.h"

//_____________________________________________________________________
BfcItem BFC[] = {
  {"Key"         ,"Name","Chain","Opts"                                   ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"SD97"        ,""  ,"","db"                                ,"","","Turn on 1997 test parameters",kFALSE},
  {"SD98"        ,""  ,"","db"                                ,"","","Turn on 1998 test parameters",kFALSE},
  {"Y1a"         ,""  ,"","db,calib"                            ,"","","Turn on Year 1a parameters",kFALSE},
  {"Y1b"         ,""  ,"","db,calib"                                    ,"","","Year 1b parameters",kFALSE},
  {"Y1c"         ,""  ,"","db,calib"                            ,"","","Turn on Year 1c parameters",kFALSE},
  {"ES99"        ,""  ,"","db"          ,"","","Turn on 1999 engineering run simulation parameters",kFALSE},
  {"ER99"        ,""  ,"","db"           ,"","","Turn on 1999 engineering run real data parameters",kFALSE},
  {"DC99"        ,""  ,"","db"       ,"","","Turn on December 1999 engineering run real parameters",kFALSE},
  {"Y1d"         ,""  ,"","db,calib"                            ,"","","Turn on Year 1d parameters",kFALSE},
  {"Y1e"         ,""  ,"","db,calib"                            ,"","","Turn on Year 1e parameters",kFALSE},
  {"Y1h"         ,""  ,"","db,calib"                            ,"","","Turn on Year 1h parameters",kFALSE},
  {"RY1h"        ,""  ,"","db,calib"                        ,"","","Real data with Year1h geometry",kFALSE},
  {"Y2a"         ,""  ,"","db,calib"                            ,"","","Turn on Year 2a parameters",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"mdc3"        ,""  ,"","cy1h,GeantOut"                               ,"","","MDC3 default chain",kFALSE},
  {"doEvents"    ,""  ,"","xin,event,analysis,FieldOn"                                    ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","global,dst,qa,event,analysis,EventQA"                          ,"","","",kFALSE},
  {"Cdefault"    ,""  ,"","tpc,ftpc,rrs,rich,l0,l3,Cdst,Kalman,tags,Tree"    ,"","","Default chain",kFALSE}, 
  {"Cy1a"        ,""  ,"","y1a,Cdefault"                                 ,"","","Turn on chain y1a",kFALSE},
  {"Cy1b"        ,""  ,"","y1b,Cdefault"                                 ,"","","Turn on chain y1b",kFALSE},
  {"Cy1c"        ,""  ,"","y1c,Cdefault"                                 ,"","","Turn on chain y1c",kFALSE},
  {"Cy1d"        ,""  ,"","y1d,Cdefault"                                 ,"","","Turn on chain y1d",kFALSE},
  {"cy1e"        ,""  ,"","y1e,Cdefault"                                 ,"","","Turn on chain y1h",kFALSE},
  {"cy1h"        ,""  ,"","y1h,Cdefault"                                 ,"","","Turn on chain y1e",kFALSE},
  {"Cy2a"        ,""  ,"","y2a,tpc,ftpc,emc,l0,l3,Cdst,tags,Tree,svt"    ,"","","Turn on chain y2a",kFALSE},
  {"P00h"        ,""  ,"","ry1h,in,tpc_daq,tpc,rich,trg,Cdst,Kalman,tags,Tree,evout","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"OPTIONS     ","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"Kalman"      ,""  ,"","geant"                                                         ,"","","",kFALSE},
  {"Eval"        ,""  ,"","","",""                ,"Turn on evaluation switch for different makers",kFALSE},
  {"Ev03"        ,""  ,"","","",""                                 ,"Turn on alternative V0 method",kFALSE},
  {"off"         ,""  ,"","","",""                                        ,"Turn off default chain",kFALSE},
  {"gstar"       ,""  ,"","geant,Simu","","" ,"gstar for 10 muon tracks with pT = 10GeV in |eta|<1",kFALSE}, 
  {"tdaq"        ,""  ,"","xin,tpc_daq"                                                   ,"","","",kFALSE},  
  {"miniDAQ"     ,"tpc_raw","tpc","xin,FieldOff,SD97,Eval"    ,"StMinidaqMaker","StMinidaqMaker","",kFALSE}, 
  {"fzin"        ,""  ,"","geant,Simu","" ,""                                 ,"read gstar fz-file",kFALSE},
  {"NoInput"     ,""  ,"","","" ,""                                                ,"No input file",kFALSE},
  {"util"        ,""  ,"","","","StAnalysisUtilities",                   "Load StAnalysisUtilities",kFALSE},
  {"NoFieldSet"  ,""  ,"","-FieldOn,-FieldOff,-HalfField,-ReverseField,-magF" ,"","","No Field Set",kFALSE},
  {"FieldOn"     ,""  ,"","NoFieldSet"                ,"StMagFC","StMagF" ,"Constant nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","NoFieldSet"                       ,"StMagFC","StMagF" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","NoFieldSet"                      ,"StMagFC","StMagF","Half Field option",kFALSE},
  {"ReverseField",""  ,"","NoFieldSet"                   ,"StMagFC","StMagF","Reverse Field option",kFALSE},
  {"NoCintDb"    ,""  ,"",""                                            ,"","","Switch off Cint Db",kFALSE},
  {"NoCintCalDb" ,""  ,"",""                                      ,"","","Switch off Cint Calib Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                                ,"","","Set debug flag",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},  
  {"big"         ,""  ,"",""                                         ,"","","Set NwGEANT =20Mwords",kFALSE},
  {"bigbig"      ,""  ,"",""                                         ,"","","Set NwGEANT =40Mwords",kFALSE},
  {"EvOut"       ,""  ,"","Tree"                                   ,"","","Write StEvent to StTree",kFALSE},
  {"GeantOut"    ,""  ,"","Tree"                                ,"","","Write g2t tables to StTree",kFALSE},
  {"TrsOut"      ,""  ,"","Tree"                                ,"","","Write Trs output to StTree",kFALSE},
  {"Simu"        ,""  ,"",""                                                ,"","","Simulated Data",kFALSE},
  {"paw"         ,""  ,"",""                                      ,"","","Allocate memory for pawc",kFALSE},
  {"TrsOut"      ,""  ,"","Tree"                                ,"","","Write Trs output to StTree",kFALSE},
  {"AllEvent"    ,""  ,"","Tree"                               ,"","","Write whole event to StTree",kFALSE},
  {"AllTables"   ,""  ,"","",""                                     ,"St_Tables","Load Star Tables",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"Tables      ","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"tables"      ,""  ,"",
"StDbT,ctf_T,ebyeT,emc_T,ftpcT,gen_T,geomT,globT,l3_T,mwc_T,sim_T,svt_T,tpc_T,trg_T,vpd_T","","","",kFALSE},
  {"StDbT"       ,""  ,"","",""                                ,"libStDb_Tables","Load StDb_Tables",kFALSE},
  {"ctf_T"       ,""  ,"","",""                                  ,"libctf_Tables","Load ctf_Tables",kFALSE},
  {"ebyeT"       ,""  ,"","",""                                ,"libebye_Tables","Load ebye_Tables",kFALSE},
  {"emc_T"       ,""  ,"","",""                                  ,"libemc_Tables","Load emc_Tables",kFALSE},
  {"ftpcT"       ,""  ,"","",""                                ,"libftpc_Tables","Load ftpc_Tables",kFALSE},
  {"gen_T"       ,""  ,"","",""                                  ,"libgen_Tables","Load gen_Tables",kFALSE},
  {"geomT"       ,""  ,"","",""                        ,"libgeometry_Tables","Load geometry_Tables",kFALSE},
  {"globT"       ,""  ,"","",""                            ,"libglobal_Tables","Load global_Tables",kFALSE},
  {"l3_T"        ,"",  "","",""                                    ,"libl3_Tables","Load l3_Tables",kFALSE},
  {"mwc_T"       ,""  ,"","",""                                  ,"libmwc_Tables","Load mwc_Tables",kFALSE},
  {"sim_T"       ,""  ,"","",""                                  ,"libsim_Tables","Load sim_Tables",kFALSE},
  {"svt_T"       ,""  ,"","",""                                  ,"libsvt_Tables","Load svt_Tables",kFALSE},
  {"tpc_T"       ,""  ,"","",""                                  ,"libtpc_Tables","Load tpc_Tables",kFALSE},
  {"trg_T"       ,""  ,"","",""                                  ,"libtrg_Tables","Load trg_Tables",kFALSE},
  {"vpd_T"       ,""  ,"","",""                                  ,"libvpd_Tables","Load vpd_Tables",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"vpd"         ,""  ,"","vpd_T",""                                                   ,"St_vpd","",kFALSE},
  {"tls"         ,""  ,"","",""                                                           ,"tls","",kFALSE},
  {"daq"         ,""  ,"","",""                         ,"StDaqLib,StDAQMakerLib","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","","","StarClassLibrary",                         "Load StarClassLibrary",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----","------------------------------------------------","","","",kFALSE},
  {"in"          ,""  ,"","xin"                                               ,"","","Alias to xin",kFALSE},
  {"xin"         ,""  ,"",""              ,"StIOMaker","StIOMaker","Read [XDF|DAQ|ROOT] input file",kFALSE},
  {"xdf2root"    ,""  ,"",""                                   ,"","xdf2root","Read XDF input file",kFALSE},
  {"geant","geant","","NoFieldSet,geomT,gen_T,sim_T"
                                         ,"St_geant_Maker","geometry,St_g2t,St_geant_Maker","GEANT",kFALSE}, 
  {"db"          ,"db","","StDbT,xdf2root"       ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"dbutil"      ,""  ,"","SCL"                            ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"calib"       ,"calib","","xdf2root"          ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"magF"        ,"","","NoFieldSet,StDbT,db","StMagFMaker","StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"l0"          ,"l0Chain","","trg_T,globT,ctf,mwc,trg"               ,"StChainMaker","StChain","",kFALSE}, 
  {"ctf"         ,"ctf","l0Chain","ctf_T,db"               ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE}, 
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE}, 
  {"trg"         ,"trg","l0Chain","trg_T,db"               ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"tpc"       ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx","StChainMaker","StChain","",kFALSE},
  {"Trs"         ,"","tpcChain","scl,tpcDB,tpc_daq,Simu"              ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"Mixer"         ,"tpc_raw","","","StMixerMaker","StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpcChain","tpc_T"              ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,""  ,"tpcChain","Simu"                           ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tpc_T,tls"        ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpcChain","tpc_T,tls,"     ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"laser"       ,"tpc_tracks","tpcChain","tdaq,tpc,-tpt,-PreVtx"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},  
  {"PreVtx"     ,"","tpcChain","tpt,SCL,sim_T,tpc_T,svt_T,ftpcT,globT,ctf_T",
                                       "StPreVertexMaker","St_tpc,St_svt,St_global,St_dst_Maker","",kFALSE},
  {"emc"    ,"emcChain","","geant,emc_T,tpc_T,db,calib,ems,emh,PreEcl" ,"StChainMaker","StChain","",kFALSE},
  {"ems"    ,"emc_raw","emcChain","geant,emc_T"    ,"St_ems_Maker","StEvent,St_emc,St_ems_Maker","",kFALSE},
  {"emh"    ,"emc_hits","emcChain","geant,emc_T,tpc_T"     ,"St_emc_Maker","St_emc,St_emc_Maker","",kFALSE},
  {"svt"         ,"svtChain","","svt_T,srs,stk"                        ,"StChainMaker","StChain","",kFALSE},
  {"srs"         ,"svt_hits","svtChain","tls,Simu"  ,"St_srs_Maker","St_tpc,St_svt,St_srs_Maker","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain",""                       ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"stk"         ,"svt_tracks","svtChain","tls"     ,"St_stk_Maker","St_tpc,St_svt,St_stk_Maker","",kFALSE},
  {"Ftpc"        ,"ftpcChain"  ,"","ftpcT,fcl,fpt"                     ,"StChainMaker","StChain","",kFALSE},
  {"fss"         ,"ftpc_raw","ftpcChain","SCL,Simu"       ,"St_fss_Maker","St_ftpc,St_fss_Maker","",kFALSE},
  {"Fcl"         ,"ftpc_hits","ftpcChain","SCL"
                          ,"StFtpcClusterMaker","StDaqLib,StDAQMaker,St_ftpc,StFtpcClusterMaker","",kFALSE},
  {"fpt"         ,"ftpc_tracks","ftpcChain","SCL" ,"StFtpcTrackMaker","St_ftpc,StFtpcTrackMaker","",kFALSE},
  {"global"      ,"globalChain","","globT,Match,primary,v0,xi,kink,dst,SCL"
                                                         ,"StChainMaker","St_tpc,St_svt,StChain","",kFALSE},
  {"Match"       ,"match","globalChain","SCL,tpc_T,svt_T,globT,tls"
                                                 ,"StMatchMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Epc"         ,"epc","globalChain","PreEcl,Match"                  ,"StEpcMaker","StEpcMaker","",kFALSE},
  {"Primary"     ,"primary","globalChain","SCL,globT,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"V0"          ,"v0","globalChain","SCL,globT,tls","StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"          ,"xi","globalChain","SCL,globT,tls","StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"   ,"kink","globalChain","SCL,globT,tls","StKinkMaker" ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"dst"         ,"dst","globalChain","SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT","St_dst_Maker" 
                                                                ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Event"       ,"","","globT,SCL"                       ,"StEventMaker","StEvent,StEventMaker","",kFALSE},
  {"PreEcl"      ,"preecl","emcChain","emh"                     ,"StPreEclMaker","StPreEclMaker","",kFALSE},
  {"Rrs"         ,"","","sim_T,Simu"                                  ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rich"        ,"","","sim_T,globT"                      ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"l3"          ,"l3Chain","","l3cl,l3t"                            ,"StChainMaker","StBFChain","",kFALSE},
  {"l3cl"        ,"","l3Chain","l3_T"               ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3Chain","l3_T"                       ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"analysis"    ,"","","Event"           ,"StAnalysisMaker","StAnalysisMaker","Exampe of Analysis",kFALSE},
  {"TagsChain"   ,"TagsChain","",""                                    ,"StChainMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
  {"Flow"        ,"","TagsChain","Event"                            ,"StFlowMaker","StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","TagsChain","Event,Flow"                 ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
  {"FlowAnalysis","","TagsChain","Event,Flow"       ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","Event"              ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"EbyeScaTags" ,"","TagsChain","Event"              ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
  {"PCollTag"    ,"","TagsChain","Event"                    ,"StPCollTagMaker","StPCollTagMaker","",kFALSE},
  {"tags"        ,"","TagsChain","TagsChain,globT,Event,FlowTag,StrangeTags,EbyeScaTags,TpcTag,PCollTag"    
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"QA"          ,"QA","","globT,SCL,global"                        ,"St_QA_Maker","St_QA_Maker","",kFALSE},
  {"EventQA"     ,"EventQA","","Event"                           ,"StEventQAMaker","St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"EventDisplay","","SCL,St_geom"   ,"StEventDisplayMaker","StEventDisplayMaker","",kFALSE},
  {"Mc"          ,"McChain","","sim_T,globT,McAss,McAna"               ,"StChainMaker","StChain","",kFALSE},
  {"McEvent"     ,"","McChain","Event",              "StMcEventMaker","StMcEvent,StMcEventMaker","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAna"       ,"","McChain","McEvent",                "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"LAna"        ,"","","in,RY1h,geant,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE}, 
  {"Tree"        ,""  ,"",""    ,"StTreeMaker","StTreeMaker","Write requested branches into filles",kFALSE}
};
Int_t NoChainOptions = sizeof (BFC)/sizeof (BfcItem);
class StEvent;
StEvent *Event;
class StIOMaker; StIOMaker *inpMk=0;     
class St_geant_Maker; St_geant_Maker *geantMk = 0;   
class St_db_Maker;    
St_db_Maker *dbMk    = 0; 
St_db_Maker *calibMk = 0; 
//St_db_Maker *RunLogMk = 0;
StMaker *tpcDBMk = 0;
class StTreeMaker;    
StTreeMaker    *treeMk  = 0;
Bool_t kMagF = kFALSE; 
ClassImp(StBFChain)

//_____________________________________________________________________________
StBFChain::StBFChain(const char *name):StChain(name),fXdfOut(0),fSetFiles(0),fInFile(0),fFileOut(0),fXdfFile(0) {
   fBFC = new BfcItem[NoChainOptions];
   memcpy (fBFC, &BFC, sizeof (BFC));
}
//_____________________________________________________________________________
StBFChain::~StBFChain(){
  if (fBFC) delete [] fBFC;
  Finish();
}
//_____________________________________________________________________________
Int_t StBFChain::Load() 
{
  Int_t status = kStOk;
  Int_t i, iok;
  for (i = 1; i< NoChainOptions; i++) {// Load Libraries if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Libs) > 0) { 
	TObjArray Libs;
	ParseString(fBFC[i].Libs,Libs);
	TIter nextL(&Libs);
	TObjString *libe = 0;
	while ((libe = (TObjString *) nextL())) {
	  TObjArray LoadedLibs;
	  TString lib(gSystem->GetLibraries(0,"D")); 
	  ParseString(lib,LoadedLibs);
	  TIter next(&LoadedLibs);
	  TObjString *LoadedLib;
	  while ((LoadedLib = (TObjString *) next())){
	    TString Base(gSystem->BaseName(LoadedLib->GetString().Data()));
	    Base.ReplaceAll(".so","");
	    Base.ReplaceAll(".sl","");
	    if (Base == libe->GetString()) goto ENDL;
	  }
	  //	  if (!strstr(lib,libe.Data())) {
	  iok = gSystem->Load(libe->GetString().Data());
	  
	  if (iok < 0)  {
	    printf("QAInfo: problem with loading\t%s\nQAInfo: %s is switched off \t!!!!\n"
		   ,libe->GetString().Data(),fBFC[i].Key);
	    fBFC[i].Flag = kFALSE;
	    status = kStErr;
	    break;
	  }
	  else printf("QAInfo: Library %-20s\t(%s)\tis loaded\n",libe->GetString().Data(),
		      gSystem->DynamicPathName(libe->GetString().Data()));
	  //	  }
	ENDL: continue;
	  LoadedLibs.Delete();
	}
	Libs.Delete();
      }
    }
  }
  return status;
}
//_____________________________________________________________________________
Int_t StBFChain::Instantiate() 
{
  Int_t status = kStOk;
  Int_t i;
  for (i = 1; i< NoChainOptions; i++) {// Instantiate Makers if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Maker) > 0){// && strlen(fBFC[i].Name) > 0){// && GetMaker(fBFC[i].Name))) { 
	TString maker(fBFC[i].Maker);
	TString Key(fBFC[i].Key);
	Key.ToLower();
	StMaker *saveMk = 0;
	if (strlen(fBFC[i].Chain) > 0) {
	  StMaker *chain = GetMaker(fBFC[i].Chain);
	  if (chain) saveMk = chain->cd();
	}
	if (maker == "StMagFC" && !kMagF) {
	  Float_t Scale = 1.0;
	  TString FieldName("STAR Normal field");
	  if (GetOption("FieldOff"))     {Scale = 0.00002; FieldName = "STAR no field";}
	  if (GetOption("HalfField"))    {Scale = 0.5;     FieldName = "STAR Normal field";}
	  if (GetOption("ReverseField")) {Scale = - Scale; FieldName += " Reverse";}
	  new StMagFC("field",FieldName.Data(),Scale);
	  kMagF = kTRUE;
	  continue;
	}
	if (maker == "St_db_Maker"){
	  St_db_Maker *mk = 0;
	  if (Key.CompareTo("calib",TString::kIgnoreCase) == 0) {
	    if (!calibMk) calibMk = new St_db_Maker(fBFC[i].Name,"$STAR_ROOT/calib","$PWD/calib");
	    mk = calibMk;
	  }
	  if (Key.CompareTo("db",TString::kIgnoreCase) == 0) {
	    dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
	    mk = dbMk;
	  }
	  if (!mk) status = kStErr;
	  else {
	    fBFC[i].Name = (Char_t *) mk->GetName();
	    if (GetOption("Simu"))    mk->SetFlavor("sim+ofl");
	    else                      mk->SetFlavor("ofl");
	  }
	  continue;
	}
	if (maker == "StIOMaker" && fSetFiles) {
	  inpMk = new StIOMaker("inputStream","r",fSetFiles);
	  if (inpMk) {
	    fBFC[i].Name = (Char_t *) inpMk->GetName();
	    SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
	    SetInput("geant",".make/inputStream/.make/inputStream_XDF/.data/event/geant/Event");
	  }
	  else status = kStErr;
	  continue;
	}
	if (maker == "StTreeMaker" && fFileOut) {
	  treeMk = new StTreeMaker("tree",fFileOut->Data());
	  if (treeMk) {
	    fBFC[i].Name = (Char_t *) treeMk->GetName();
	    treeMk->SetIOMode("w");
	    SetTreeOptions();
	    continue;
	  }
	}
	if (Key == "geant") {
	  if (!geantMk) {
	    Int_t                    NwGeant = 10000000;
	    if (!GetOption("fzin") && !GetOption("gstar"))
	                             NwGeant =  5000000;
	    if (GetOption("big"))    NwGeant = 20000000;
	    if (GetOption("bigbig")) NwGeant = 40000000;
	    Int_t IwType = 0;
	    if (GetOption("Higz"))  IwType = 1;
	    Int_t NwPaw = 0;
	    if (GetOption("paw")) NwPaw  = 200000;
	    geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
	    if (geantMk) {
	      fBFC[i].Name = (Char_t *) geantMk->GetName();
	      geantMk->SetActive(kFALSE);
	      if (GetOption("fzin") || GetOption("gstar")) geantMk->SetActive(kTRUE);
	      SetGeantOptions();
	    }
	  }
	  if (!geantMk) status = kStErr;
	  continue;
	}
	StMaker *mk = 0;
	if (maker == "StTpcDbMaker") mk = tpcDBMk;
	if (!mk) {
	  if (strlen(fBFC[i].Name) > 0) mk = New(fBFC[i].Maker,fBFC[i].Name);
	  else  {
	    mk = New(fBFC[i].Maker);
	    if (mk) fBFC[i].Name = (Char_t *) mk->GetName();
	  }
	}
	if (mk) {
	  if (maker == "StTpcDbMaker") tpcDBMk = mk;
	  if (maker == "St_dst_Maker") SetInput("dst",".make/dst/.data/dst");
	  if (maker == "StMatchMaker" && !GetOption("Kalman")) mk->SetMode(-1);
	  if (maker == "St_tpcdaq_Maker") {
	    if (GetOption("Trs")) mk->SetMode(1); // trs
	    else                  mk->SetMode(0); // daq
	  }
	  if (maker == "StRchMaker") {
	    if (GetOption("Rrs")) mk->SetMode(1); // rrs
	    else                  mk->SetMode(0); // daq
	  }
	  if (maker == "StV0Maker" && GetOption("Ev03")) mk->SetMode(1); // Turn on alternative V0 method
	}
        else status = kStErr;
	if (saveMk) saveMk->cd();
      }
    }
  }
  if (fXdfFile) {
    fXdfOut = new St_XDFFile(fXdfFile->Data(),"wb"); 
    if (!fXdfOut) status = kStErr;
  }
  //  PrintQAInfo();
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef GetOption("trg")
  if (GetOption("MakeDoc")) MakeDoc();
  if (GetOption("Debug")) SetDEBUG();
  return status;
}
Int_t StBFChain::Finish()
{
  SafeDelete (fXdfOut);
  if (fTFile) {fTFile->Write(); fTFile->Flush(); fTFile->Close(); SafeDelete (fTFile);}
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
Int_t StBFChain::ParseString (const TString &tChain, TObjArray &Opt) {
  Int_t nParsed = -1;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  TString Tag, opt, nopt;
  while ( (begin < tChain.Length()) && (index != kNPOS) ) { 
    // loop over given Chain options 
    index = tChain.Index(separator,&end,begin);
    if (index >= 0 && end > 1) {
      TString substring(tChain(index,end));
      Opt.Add(new TObjString(substring.Data()));
    }
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
  TString opt, nopt;
  for (Int_t i = 1; i< NoChainOptions; i++) {
    opt = TString(fBFC[i].Key); //check nick name
    opt.ToLower();
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;}
    else {if (Tag == nopt) {return -i;}}
    opt = TString(fBFC[i].Maker); //check full maker name2
    nopt = TString("-");
    nopt += opt;
    if       (Tag ==  opt) {return  i;} 
    else {if (Tag == nopt) {return -i;}}
  }
  printf ("Option %s has not been recognized\n", Tag.Data());
  return 0;
}//_____________________________________________________________________
void StBFChain::SetOption(const Int_t k) {// set all off
  if (k > 0 && !fBFC[k].Flag) {
    //    printf ("SetOption: %s %i",fBFC[k].Key,k);
    if (strlen(fBFC[k].Opts) > 0) {
      TObjArray Opts;
      ParseString(fBFC[k].Opts,Opts);
      TIter next(&Opts);
      TObjString *Opt;
      while ((Opt = (TObjString *) next())) SetOption(Opt->GetString().Data());
      Opts.Delete();
    }
    fBFC[k].Flag = kTRUE;
    printf (" Switch On  %s\n", fBFC[k].Key);
  }
  else {
    if (k < 0 && fBFC[-k].Flag) {
      //      printf ("SetOption: %s %i",fBFC[-k].Key,k);
      fBFC[-k].Flag = kFALSE;
      printf (" Switch Off %s\n", fBFC[-k].Key);
    }
    else return;
  }
}
//_____________________________________________________________________
Bool_t StBFChain::GetOption(const Int_t k) {
  return (k>0 && k <NoChainOptions) ? fBFC[k].Flag : kFALSE;
}
//_____________________________________________________________________________
void StBFChain::SetFlags(const Char_t *Chain)
{
  Int_t k;
  if (!Chain || !Chain[0] || !strlen(Chain)) {
    printf ("\tPossible Chain Options are: \n"); 
    for (k=0;k<NoChainOptions;k++)
      printf (" %2d:[-]%-12s:%-12s:%-6s:%-12s :%s :%s :%s\n"
	      ,k,fBFC[k].Key,fBFC[k].Name,fBFC[k].Chain,fBFC[k].Opts,fBFC[k].Maker,fBFC[k].Libs,fBFC[k].Comment);
    return; 
  }         
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  printf ("==============================================\n");
  gMessMgr->QAInfo() << "============= You are in " << STAR_VERSION.Data() << " ===============" << endm;
  TString tChain(Chain);
  gMessMgr->QAInfo() << "Requested chain " << GetName() << " is :\t" << tChain.Data() << endm;
  tChain.ToLower(); //printf ("Chain %s\n",tChain.Data());
  TObjArray Opts;
  ParseString(tChain,Opts);
  TIter next(&Opts);
  TObjString *Opt;
  while ((Opt = (TObjString *) next())) {
    TString string = Opt->GetString();
    Int_t kgo = kOpt(string.Data());
    if (kgo != 0) SetOption(kgo);
  }
  Opts.Delete();
  // Check flags consistency   
  if (!GetOption("NoInput")) {
    if (!GetOption("fzin") && !GetOption("gstar") && 
	!GetOption("xin")  && !GetOption("tdaq")) {
      SetOption("fzin");
      SetOption("geant");
    }
  }
  if (!GetOption("FieldOn") && !GetOption("FieldOff") && 
      !GetOption("HalfField") && !GetOption("ReverseField") &&    
      !GetOption("fzin") &&  !GetOption("gstar")) SetOption("magF"); 
  if (!GetOption("global") && 
      (GetOption("Match") || GetOption("Primary") || GetOption("V0") ||
       GetOption("Xi")    || GetOption("Kink"))) SetOption("global");
  if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval"); 
  if (!GetOption("event")) SetOption("-analysis");
  // Print set values
  for (k = 1; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      gMessMgr->QAInfo() << "================== " << k << "\t" 
			 << fBFC[k].Key << "\tis ON \t:" << fBFC[k].Comment << endm;
    }
  }
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  // define input file
  if (infile) fInFile = new TString(infile);
  if (!GetOption("NoInput")) {
    if (!fInFile && GetOption("Lana")) {
      if (GetOption("FieldOff")) {// Laser data
	fInFile = new TString("/star/rcf/daq/2000/03/st_physics_1062035_raw_000*.daq");
	fInFile->Append(",/star/rcf/scratch/love/st_physics_1062036_raw_0002.daq");
	printf ("Use default input file %s for Laser, FieldOff \n",fInFile->Data());
      }
      else {
	if (GetOption("HalfField")) {// Laser data
	  fInFile = new TString("/star/rcf/daq/2000/03/st_physics_1062006_raw_0002.daq");
	  fInFile->Append(",/star/rcf/daq/2000/03/st_physics_1062004_raw_0002.daq");
	  printf ("Use default input file %s for Laser, HalfField \n",fInFile->Data());
	}
	else {
	  fInFile = new TString("/star/rcf/daq/2000/03/st_physics_1063012_raw_0002.daq");
	  fInFile->Append(",/star/rcf/daq/2000/03/st_physics_1063013_raw_0002.daq");
	  printf ("Use default input file %s for Laser, Full Field \n",fInFile->Data());
	}
      }
    }
    if (!fInFile && GetOption("Laser")) {
	  fInFile = new TString("/star/data08/daq/2000/06/st_physics_1169006_raw_0001.daq");
	  printf ("Use default input file %s for Laser\n",fInFile->Data());
    }    
    if (!fInFile && GetOption("p00h")) {
	  fInFile = new TString("/star/data08/daq/2000/06/st_physics_1164056_raw_0001.daq");
	  printf ("Use default input file %s forfirst real data No Field \n",fInFile->Data());
    }    
    if (!fInFile && GetOption("miniDAQ")) {
      fInFile = new TString("/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"); // laser data
      printf ("Use default input file %s for %s \n",fInFile->Data(),"miniDAQ");
    }
    if (!fInFile && GetOption("fzin")) {
      fInFile = new TString("/star/rcf/simu/cocktail/hadronic/default/lowdensity/");
      if (GetOption("y1h")) fInFile->Append("year_1h/hadronic_on/Gstardata/rcf0078/hc_lowdensity.400_evts.fz");
      else 
	if (GetOption("y2a")) fInFile->Append("year_2a/hadronic_on/Gstardata/rcf0079/hc_lowdensity.400_evts.fz");
	else {printf ("for fzin Option In file has not been defined. Exit!\n"); gSystem->Exit(1);}
      printf ("Use default input file %s for %s \n",fInFile->Data(),"fzin");
    }
    if (!fInFile && GetOption("doEvents")){
      fInFile = new TString("/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf");
      printf ("Use default input file %s for %s \n",fInFile->Data(),"doEvent");
    }
    if (!fInFile && GetOption("in")) {
      fInFile = new TString("/star/rcf/daq/2000/02/st_physics_1054031_raw_000*.daq"); 
      printf ("Use default DAQ input file %s\n",fInFile->Data());
    }
    if (!fInFile && !GetOption("gstar")) {
      fInFile = new TString("/afs/rhic/star/data/samples/hijet-g2t.xdf");	       // g2t xdf file
      printf ("Use default input file %s for %s \n",fInFile->Data(),"xin");
    }
    if (fInFile) {
      if (!GetOption("fzin")) {
	fSetFiles= new StFile();
	TObjArray Files;
	ParseString((const TString )*fInFile,Files);
	TIter next(&Files);
	TObjString *File;
	while ((File = (TObjString *) next())) {
	  TString string = File->GetString();
	  if (!strstr(string.Data(),"*") &&
	      gSystem->AccessPathName(string.Data())) {// file does not exist
	    printf (" *** NO FILE: %s, exit!\n", string.Data());
	    gSystem->Exit(1); 
	  }
	  else fSetFiles->AddFile(File->String().Data());
	}
	Files.Delete();
      }
    }
  }
  if (fInFile) gMessMgr->QAInfo() << "Input file name = " << fInFile->Data() << endm;
  if (outfile)               fFileOut = new TString(outfile);
  else {
    if (GetOption("gstar"))  fFileOut = new TString("gtrack.root");
    else {
      if (fInFile) {
	fFileOut = new TString(gSystem->BaseName(fInFile->Data()));
	fFileOut->ReplaceAll("*","");
	fFileOut->ReplaceAll("..",".");
	fFileOut->ReplaceAll(".daq","");
	fFileOut->ReplaceAll(".fzd","");
	fFileOut->ReplaceAll(".fz","");
	fFileOut->ReplaceAll(".xdf","");
	fFileOut->Strip();
	fFileOut->Append(".root");
      }
    }
  }
  if (fFileOut)  gMessMgr->QAInfo() << "Output root file name " <<  fFileOut->Data() << endm;
  if (!fTFile) {
    if (GetOption("tags")  && fFileOut ||
	GetOption("lana") ||  GetOption("Laser")) {
      TString TagsName = TString(fFileOut->Data());
      TagsName.ReplaceAll(".root",".tags.root");
      fTFile = new TFile(TagsName.Data(),"RECREATE");
    }
  }
  if (GetOption("xout") && fFileOut) {
    fXdfFile = new TString(fFileOut->Data());
    fXdfFile->Append(".dst.xdf");
    gMessMgr->QAInfo() << "Open output xdf file  = " << fXdfFile->Data() <<  "++++++++++++++++++++++" << endm;
  }
  //    gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::SetGeantOptions(){
  if (geantMk) {
    SetInput("geant",".make/geant/.data");
    if (!GetOption("fzin")) {
            if (GetOption("SD97") || 
		GetOption("SD98") || 
		GetOption("Y1a")  || 
		GetOption("ES99") || 
		GetOption("ER99") || 
		GetOption("DC99"))   geantMk->LoadGeometry("detp geometry YEAR_1A"); 
      else {if (GetOption("Y1b"))    geantMk->LoadGeometry("detp geometry YEAR_1B");
      else {if (GetOption("Y1c"))    geantMk->LoadGeometry("detp geometry YEAR_1C");
      else {if (GetOption("Y1h") || 
		GetOption("RY1h"))   geantMk->LoadGeometry("detp geometry YEAR_1H");
      else {if (GetOption("Y2a"))    geantMk->LoadGeometry("detp geometry YEAR_2A");
      else                           geantMk->LoadGeometry("detp geometry YEAR_2A");}}}}
      if (GetOption("gstar")) {
	geantMk->Do("subevent 0;");
	// gkine #particles partid ptrange yrange phirange vertexrange 
	geantMk->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  0. 0.;");
	geantMk->Do("mode g2tm prin 1;");
	//  geantMk->Do("next;");
	//  geantMk->Do("dcut cave z 1 10 10 0.03 0.03;");
	if (GetOption("Debug")) geantMk->Do("debug on;");
	geantMk->Do("swit 2 3;");
      }
    }
    else {
      if (fInFile && geantMk->SetInputFile(fInFile->Data()) > kStOK) {
	printf ("File %s cannot be opened. Exit! \n",fInFile->Data());
	gSystem->Exit(1);
      }
    }
  }
}
//_____________________________________________________________________
void StBFChain::SetDbOptions(){
        if (GetOption("SD97")) SetDataBases("sd97");
  else {if (GetOption("SD98")) SetDataBases("sd98");
  else {if (GetOption("Y1a"))  SetDataBases("year_1a");
  else {if (GetOption("Y1b"))  SetDataBases("year_1b");
  else {if (GetOption("Y1c"))  SetDataBases("year_1c");
  else {if (GetOption("ES99")) SetDataBases("es99");
  else {if (GetOption("ER99")) SetDataBases("er99");
  else {if (GetOption("DC99")) SetDataBases("dc99");
  else {if (GetOption("Y1d"))  SetDataBases("year_1d");
  else {if (GetOption("Y1e"))  SetDataBases("year_1e");
  else {if (GetOption("Y1h"))  SetDataBases("year_1h");
  else {if (GetOption("Y2a"))  SetDataBases("year_2a");
  else {if (GetOption("gstar"))SetDataBases("year_2a");
  }}}}}}}}}}}}
  gMessMgr->QAInfo() << "db Maker set time = " << dbMk->GetDateTime().GetDate() 
		   << dbMk->GetDateTime().GetTime() << endm;
  gMessMgr->QAInfo()  << endm;
}
//_____________________________________________________________________
void StBFChain::SetDataBases(const Char_t* TimeStamp){
  if (dbMk)           dbMk->SetDateTime(TimeStamp);
  if (calibMk)     calibMk->SetDateTime(TimeStamp);
}
//_____________________________________________________________________
void StBFChain::SetTreeOptions()
{
  treeMk->SetBranch("histBranch");
  
  if (GetOption("dst"))      {
    treeMk->IntoBranch("dstBranch","dst");
    treeMk->SetBranch("runcoBranch");
  }
  if (GetOption("Event") && GetOption("EvOut"))  
                             treeMk->IntoBranch("eventBranch","StEvent");
  if (GetOption("AllEvent")) {
    if (geantMk) {
      treeMk->IntoBranch("geantBranch","geant");
      treeMk->IntoBranch("geantBranch","geant/.data/particle");
      treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
    }
    if (GetOption("fss"))    treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
    if (GetOption("ems"))    treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
    if (GetOption("Trs"))    treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
    if (GetOption("tcl"))    treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
    if (GetOption("tpt"))    treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
    if (GetOption("srs"))    treeMk->IntoBranch("svt_hitsBranch","svt_hits/.data");
    if (GetOption("stk"))    treeMk->IntoBranch("svt_tracksBranch","svt_tracks/.data");
    if (GetOption("trg"))    treeMk->IntoBranch("trgBranch","ctf mwc trg");
    if (GetOption("l3t"))    treeMk->IntoBranch("l3tBranch","l3Tracks");
    if (GetOption("global")) treeMk->IntoBranch("globalBranch","global/.data");
  }
  else if (GetOption("GeantOut") && geantMk) treeMk->IntoBranch("geantBranch","geant");
  else if (GetOption("TrsOut") && GetOption("Trs")) treeMk->IntoBranch("TrsBranch","Trs");
}
//_____________________________________________________________________
// @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.cxx,v 1.112 2000/07/03 02:07:43 perev Exp $
//_____________________________________________________________________
