//_____________________________________________________________________
#include "TROOT.h"
#include "TString.h"
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
  {"P00h"        ,""  ,"","ry1h,in,tpc_daq,tpc,Cdst,tags,Tree,evout","",""
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
  {"db"          ,""  ,"","StDbT,xdf2root"       ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"dbutil"      ,""  ,"","SCL"                            ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"calib"       ,""  ,"","xdf2root"             ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"magF"        ,"","","NoFieldSet,StDbT,db","StMagFMaker","StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"tpc"         ,"tpc","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StChainMaker","StChain","",kFALSE},
  {"tpcDB"       ,"tpcDB","tpc","tpc_T,dbutil,db"                      ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"Trs"         ,"","tpc","scl,tpcDB,tpc_daq,Simu"                   ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"Mixer"      ,"tpc_raw","tpc","","StMixerMaker","StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpc","tpc_T"                   ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,""  ,"tpc","Simu"                                ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"tcl"         ,"tpc_hits","tpc","tpc_T,tls"             ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpc","tpc_T,tls,"          ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"laser"       ,"tpc_tracks","tpc","tdaq,tpc,-tpt"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},  
  {"PreVtx"     ,"","","tpt,SCL,sim_T,tpc_T,svt_T,ftpcT,globT,ctf_T",
                                       "StPreVertexMaker","St_tpc,St_svt,St_global,St_dst_Maker","",kFALSE},
  {"emc"         ,"emc","","geant,emc_T,tpc_T,db,calib,ems,emh,PreEcl" ,"StChainMaker","StChain","",kFALSE},
  {"ems"         ,"emc_raw","emc","geant,emc_T"    ,"St_ems_Maker","StEvent,St_emc,St_ems_Maker","",kFALSE},
  {"emh"         ,"emc_hits","emc","geant,emc_T,tpc_T"     ,"St_emc_Maker","St_emc,St_emc_Maker","",kFALSE},
  {"svt"         ,"svt","","svt_T,srs,stk"                             ,"StChainMaker","StChain","",kFALSE},
  {"srs"         ,"svt_hits","svt","tls,Simu"       ,"St_srs_Maker","St_tpc,St_svt,St_srs_Maker","",kFALSE},
  {"stk"         ,"svt_tracks","svt","tls"          ,"St_stk_Maker","St_tpc,St_svt,St_stk_Maker","",kFALSE},
  {"Ftpc"        ,"ftpc"  ,"","ftpcT,fcl,fpt"                          ,"StChainMaker","StChain","",kFALSE},
  {"fss"         ,"ftpc_raw","ftpc","SCL,Simu"            ,"St_fss_Maker","St_ftpc,St_fss_Maker","",kFALSE},
  {"Fcl"         ,"ftpc_hits","ftpc","SCL"
                          ,"StFtpcClusterMaker","StDaqLib,StDAQMaker,St_ftpc,StFtpcClusterMaker","",kFALSE},
  {"fpt"         ,"ftpc_tracks","ftpc","SCL"              ,"StFtpcTrackMaker","St_ftpc,StFtpcTrackMaker","",kFALSE},
  {"l0"          ,"l0","","trg_T,globT,ctf,mwc,trg"                    ,"StChainMaker","StChain","",kFALSE}, 
  {"ctf"         ,"ctf","l0","ctf_T,db"                    ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE}, 
  {"mwc"         ,"mwc","l0","mwc_T,db"                    ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE}, 
  {"trg"         ,"trg","l0","trg_T,db"                    ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"global"      ,"global","","globT,Match,primary,v0,xi,kink,dst,SCL"
                                                         ,"StChainMaker","St_tpc,St_svt,StChain","",kFALSE},
  {"Match"       ,"match","global","SCL,tpc_T,svt_T,globT,tls"
                                                 ,"StMatchMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Epc"         ,"epc","global","PreEcl,Match"                       ,"StEpcMaker","StEpcMaker","",kFALSE},
  {"Primary"     ,"primary","global","SCL,globT,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"V0"          ,"v0","global","SCL,globT,tls"     ,"StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"          ,"xi","global","SCL,globT,tls"     ,"StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"        ,"kink","global","SCL,globT,tls","StKinkMaker" ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"dst"         ,"dst","global","SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT","St_dst_Maker" 
                                                                ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Event"       ,"","","globT,SCL"                       ,"StEventMaker","StEvent,StEventMaker","",kFALSE},
  {"PreEcl"      ,"preecl","emc","emh"                          ,"StPreEclMaker","StPreEclMaker","",kFALSE},
  {"Rrs"         ,"","","sim_T,Simu"                                  ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rich"        ,"","","sim_T,globT"                      ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"l3"          ,"l3","","l3cl,l3t"                                 ,"StChainMaker","StBFChain","",kFALSE},
  {"l3cl"        ,"","l3","l3_T"                    ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3","l3_T"                            ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"analysis"    ,"","","Event"           ,"StAnalysisMaker","StAnalysisMaker","Exampe of Analysis",kFALSE},
  {"TagsChain"   ,"","TagsChain",""                                    ,"StChainMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
  {"Flow"        ,"","TagsChain","Event"                            ,"StFlowMaker","StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","TagsChain","Event,Flow"                 ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
  {"FlowAnalysis","","TagsChain","Event,Flow"       ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","Event"              ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"EbyeScaTags" ,"","TagsChain","Event"              ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
  {"tags"        ,"","TagsChain","TagsChain,globT,Event,FlowTag,StrangeTags,EbyeScaTags,TpcTag"    
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"QA"          ,"QA","","globT,SCL,global"                        ,"St_QA_Maker","St_QA_Maker","",kFALSE},
  {"EventQA"     ,"EventQA","","Event"                           ,"StEventQAMaker","St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"EventDisplay","","SCL,St_geom"   ,"StEventDisplayMaker","StEventDisplayMaker","",kFALSE},
  {"Mc"          ,"Mc","","sim_T,globT,McAss,McAna"                    ,"StChainMaker","StChain","",kFALSE},
  {"McEvent"     ,"","Mc","Event",                   "StMcEventMaker","StMcEvent,StMcEventMaker","",kFALSE},
  {"McAss"       ,"","Mc","McEvent",                   "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAna"       ,"","Mc","McEvent",                     "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"LAna"        ,"","","in,RY1h,geant,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE}, 
  {"Tree"        ,""  ,"",""                                        ,"StTreeMaker","StTreeMaker","",kFALSE}
};
Int_t NoChainOptions = sizeof (BFC)/sizeof (BfcItem);
class StEvent;
StEvent *Event;
class StIOMaker; StIOMaker *inpMk=0;     
class St_geant_Maker; St_geant_Maker *geantMk = 0;   
class St_db_Maker;    
St_db_Maker *dbMk    = 0; 
St_db_Maker *calibMk = 0; 
St_db_Maker *StarDbMk = 0; 
St_db_Maker *RunLogMk = 0;
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
  Int_t i, j, k, l, iok;
  for (i = 1; i< NoChainOptions; i++) {// Load Libraries if any
    if (fBFC[i].Flag) {
      if (strlen(fBFC[i].Libs) > 0) { 
	TString *Libs[80];
	Int_t NParsed = ParseString(fBFC[i].Libs,Libs);
	TString lib(gSystem->GetLibraries(0,"D")); 
	TString *LoadedLibs[80];
	Int_t NLoaded = ParseString(lib,LoadedLibs);
	for (j=0;j<=NParsed;j++) {
	  TString libe(*Libs[j]);
	  libe.Append(".so");
	  for (l = 0; l < 2; l++) { // so / sl
	    if (l) libe.ReplaceAll(".so",".sl");
	    for (k = 0; k < NLoaded; k++) {
	      const Char_t *Base =  gSystem->BaseName(LoadedLibs[k]->Data());
	      if (!strcmp(Base,libe.Data())) goto ENDL;
	    }
	  }
	  //	  if (!strstr(lib,libe.Data())) {
	  iok = gSystem->Load(Libs[j]->Data());
	  if (iok < 0)  {
	    printf("QAInfo: problem with loading\t%s\nQAInfo: %s is switched off \t!!!!\n"
		   ,Libs[j]->Data(),fBFC[i].Key);
	    fBFC[i].Flag = kFALSE;
	    status = kStErr;
	    break;
	  }
	  else printf("QAInfo: Library %-20s\t(%s)\tis loaded\n",Libs[j]->Data(),
		      gSystem->DynamicPathName(Libs[j]->Data()));
	  //	  }
	ENDL: continue;
	}
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
	  continue;
	}
	if (maker == "St_db_Maker"){
	  if (Key == "calib") {
	    if (!GetOption("NoCintCalDb")) {
	      const char *calibDB = "$STAR_ROOT/calib";
	      if (!calibMk) {
		calibMk = new St_db_Maker("calib",calibDB);
		if (!calibMk) status = kStErr;
		else {
		  fBFC[i].Name = (Char_t *) calibMk->GetName();
		  if (GetOption("Simu")) calibMk->SetFlavor("sim");
		  else                   calibMk->SetFlavor("ofl");
		}
	      }
	    } 
	  }
	  else {
	    if (!GetOption("NoCintDb")) {
	      const char *mainDB = "$STAR/StDb/params";
	      char *userDB = 0;
	      TString STAR("$STAR");
	      gSystem->ExpandPathName(STAR);
	      TString PWD(gSystem->pwd());
	      if (STAR != PWD && gSystem->AccessPathName("./StDb/params") == 0)  userDB = "./StDb/params";
	      gMessMgr->QAInfo() << " Main DataBase == " << mainDB << endm;  
	      if (userDB) gMessMgr->QAInfo() << " User DataBase == " << PWD.Data() << "/" << userDB << endm;  
	      if (!dbMk) {
		dbMk = new St_db_Maker("db",mainDB,userDB);
		if (!dbMk) status = kStErr;
		else {
		  fBFC[i].Name = (Char_t *) dbMk->GetName();
		  if (GetOption("Simu")) dbMk->SetFlavor("sim");
		  else                   dbMk->SetFlavor("ofl");
		  SetDbOptions();
		}
	      }
	    }
	    if (!GetOption("NoMySQLDb")) {
	      if (GetOption("tpcDB")){// 
		if (!StarDbMk) {
		  StarDbMk     = new St_db_Maker("StarDb","MySQL:StarDb");
		  if (!StarDbMk) status = kStErr;
		  else {
		    if (GetOption("Simu")) StarDbMk->SetFlavor("sim");
		    else                   StarDbMk->SetFlavor("ofl");
		  }
		}
		if (!RunLogMk) {
		  RunLogMk = new St_db_Maker("RunConditions","MySQL:RunLog");
		  if (!RunLogMk) status = kStErr;
		  else {
		    if (GetOption("Simu")) RunLogMk->SetFlavor("sim");
		    else                   RunLogMk->SetFlavor("ofl");
		  }
		}
	      }
	    }
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
	      if (!GetOption("fzin") && !GetOption("gstar")) {
		geantMk->SetActive(kFALSE);
	      }
	      else {
		kMagF = kTRUE;
	      }
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
Int_t StBFChain::ParseString (const TString &tChain, TString *Opt[]) {
  Int_t nParsed = -1;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  TRegexp separator("[^ ;,\\t\\s]+");
  TString Tag, opt, nopt;
  while ( (begin < tChain.Length()) && (index != kNPOS) ) { 
    // loop over given Chain options 
    index = tChain.Index(separator,&end,begin);
    if (index >= 0 && end > 1) Opt[++nParsed] = new TString(tChain(index,end));
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
    opt = TString(fBFC[i].Key);
    opt.ToLower();
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
      TString *Opt[80];
      Int_t NParsed = ParseString(fBFC[k].Opts,Opt);
      Int_t i;
      for (i=0;i<=NParsed;i++) SetOption(Opt[i]);
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
  TString *Opt[80];
  Int_t NParsed = ParseString(tChain,Opt);
  Int_t i;
  for (i=0;i<=NParsed;i++){
    Int_t kgo = kOpt(*Opt[i]);
    if (kgo != 0) SetOption(kgo);
  }
  // Check flags consistency   
  if (!GetOption("NoInput")) {
    if (!GetOption("fzin") && !GetOption("gstar") && 
	!GetOption("xin")  && !GetOption("tdaq")) {
      SetOption("fzin");
      SetOption("geant");
    }
  }
  if (!GetOption("geant") && !GetOption("FieldOn") && !GetOption("FieldOff") && 
      !GetOption("HalfField") && !GetOption("ReverseField") && !kMagF)   SetOption("magF"); 
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
	fInFile = new TString("/star/rcf/daq/2000/03/st_physics_1062035_raw_0001.daq");
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
	TString *Files[80];
	Int_t NParsed = ParseString((const TString )*fInFile,Files);
	Int_t i;
	for (i=0;i<=NParsed;i++) {
	  if (!strstr(Files[i]->Data(),"*") &&
	      gSystem->AccessPathName(Files[i]->Data())) {// file does not exist
	    printf (" *** NO FILE: %s, exit!\n", Files[i]->Data());
	    gSystem->Exit(1); 
	  }
	  else fSetFiles->AddFile(Files[i]->Data());
	}
      }
    }
  }
  if (fInFile) gMessMgr->QAInfo() << "Input file name = " << fInFile->Data() << endm;
  if (outfile)               fFileOut = new TString(outfile);
  else {
    if (GetOption("gstar"))  fFileOut = new TString("gtrack");
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
  if (GetOption("tags") && fFileOut && !fTFile) {
    TString *TagsName = new  TString(fFileOut->Data());
    TagsName->ReplaceAll(".root",".tags.root");
    fTFile = new TFile(TagsName->Data(),"RECREATE");
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
  gMessMgr->QAInfo() << "Geometry Maker set time = ";
  if (StarDbMk)  gMessMgr->QAInfo()                
                   << StarDbMk->GetDateTime().GetDate() 
		   << StarDbMk->GetDateTime().GetTime();
  gMessMgr->QAInfo()  << endm;
}
//_____________________________________________________________________
void StBFChain::SetDataBases(const Char_t* TimeStamp){
  if (dbMk)           dbMk->SetDateTime(TimeStamp);
  if (StarDbMk)     StarDbMk->SetDateTime(TimeStamp);
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
// $Id: StBFChain.cxx,v 1.97 2000/06/10 23:13:45 fisyak Exp $
// $Log: StBFChain.cxx,v $
// Revision 1.97  2000/06/10 23:13:45  fisyak
// Add flavour for Db
//
// Revision 1.96  2000/06/09 13:02:53  fisyak
// Move l3 chain after StEventMaker
//
// Revision 1.95  2000/06/05 22:44:40  fisyak
// Add chain P00h for year 1 data production
//
// Revision 1.94  2000/05/31 16:30:23  fisyak
// Put l3 back into default chain
//
// Revision 1.92  2000/05/30 14:20:42  fisyak
// pt TpcTagMaker into default chain
//
// Revision 1.91  2000/05/26 21:44:51  fisyak
// move emc after PreVtx
//
// Revision 1.90  2000/05/25 21:25:47  fisyak
// Put emc and rich after StEvent, add TpcTag
//
// Revision 1.89  2000/05/17 16:22:59  fisyak
// Add Ev03, PreEcl and Epc flags, checks for loading shared libraries and instatiaton of makers
//
// Revision 1.88  2000/05/10 21:12:02  didenko
// change to switch to StFtpcTrackMaker
//
// Revision 1.87  2000/05/01 22:28:23  fisyak
// Update from James C Dunlop for RICH
//
// Revision 1.86  2000/04/27 19:25:07  fisyak
// Add dependence StRchMaker versus StRrsMaker shared library
//
// Revision 1.85  2000/04/15 20:48:11  fisyak
// Add svt for PreVtx
//
// Revision 1.84  2000/04/13 23:07:05  fisyak
// Only one access to Db; add svt maker to output
//
// Revision 1.83  2000/03/23 03:45:51  fine
// Clean up
//
// Revision 1.82  2000/03/16 00:28:26  fisyak
// To be sure that we get magnetic field once
//
// Revision 1.81  2000/03/15 23:20:12  fisyak
// Force only instance of geant Maker
//
// Revision 1.80  2000/03/14 01:12:52  fisyak
// Split chain files
//
// Revision 1.79  2000/03/12 21:09:39  fisyak
// Add NoInput option
//
// Revision 1.78  2000/03/09 20:08:51  fisyak
// ReInstall TagsChain
//
// Revision 1.77  2000/03/09 16:18:34  fisyak
// Make chain table local to the chain
//
// Revision 1.76  2000/02/29 01:58:47  fisyak
// Fix MDC3 chain
//
// Revision 1.74  2000/02/27 22:29:52  fisyak
// Remove clash with I/O Maker in file names
//
// Revision 1.73  2000/02/25 21:53:39  fisyak
// Add rrs to default
//
// Revision 1.72  2000/02/25 21:43:49  fisyak
// Change default
//
// Revision 1.71  2000/02/18 23:02:46  fisyak
// Fix bug for tables, add sim_T to PreVtx
//
// Revision 1.70  2000/02/15 22:29:17  fisyak
// Add rrs into chain
//
// Revision 1.69  2000/02/14 13:58:27  fisyak
// Add dependence of dst Maker on tables
//
// Revision 1.68  2000/02/13 00:05:49  fisyak
// Add dependence of St_dst_Maker versus gen,sim,ctf,trg and l3t tables
//
// Revision 1.67  2000/02/12 17:36:31  fisyak
// Restore depend tpcdaq from Trs
//
// Revision 1.66  2000/02/11 00:10:02  fisyak
// Add dependence of tpc on globT
//
// Revision 1.65  2000/02/10 00:47:08  fisyak
// Add dependence of dst_Maker versus SCL
//
// Revision 1.64  2000/02/09 22:27:06  fisyak
// Add tpcDB in default tpc chain
//
// Revision 1.63  2000/02/09 22:13:00  fisyak
// Dave's correction for tpcDB
//
// Revision 1.62  2000/02/09 20:49:46  fisyak
// Change naming convention for Tables shared libraries
//
// Revision 1.61  2000/02/07 23:42:25  fisyak
// Splitted tables, Don't call St_tpcdaq_Maker for Trs, only load its shared library
//
// Revision 1.60  2000/02/03 19:34:32  fisyak
// Clean up St_geant_Maker::Init, move its parameters to ctor
//
// Revision 1.59  2000/02/02 14:50:26  fisyak
// Put PreVtx into chain if tpc has been selected
//
// Revision 1.58  2000/01/31 15:18:23  fisyak
// Add Laser Analysis
//
// Revision 1.57  2000/01/28 21:06:46  fisyak
// Add Mc Event and Mc Ass to chain
//
// Revision 1.56  2000/01/25 14:29:49  fisyak
// Add StFlowMaker to chain
//
// Revision 1.55  2000/01/22 00:34:57  fisyak
// Add dependence emc on db & calib
//
// Revision 1.54  2000/01/19 23:43:09  didenko
// take out l3 from chains
//
// Revision 1.53  2000/01/19 22:29:07  fisyak
// replace l3t to l3 in chain option
//
// Revision 1.52  2000/01/16 21:39:34  fisyak
// Fix problem with quotes
//
// Revision 1.51  2000/01/14 23:16:21  fisyak
// remove possibility of strlen(0)
//
// Revision 1.50  2000/01/14 01:16:36  fisyak
// Add xdf2root for calib
//
// Revision 1.49  2000/01/14 00:44:18  fisyak
// Add calib to chain
//
// Revision 1.48  2000/01/13 02:12:42  fisyak
// Add geometry year_1h
//
// Revision 1.47  2000/01/11 01:27:03  fisyak
// add tables for tdaq
//
// Revision 1.46  2000/01/10 18:03:14  fisyak
// Add bigbig option for 40 Mw and big for 20 Mw geant common block
//
// Revision 1.45  2000/01/07 18:30:25  fisyak
// Add option big to increase NwGEANT to 40Mwords for venus events
//
// Revision 1.44  2000/01/07 01:04:57  fisyak
// Add magF option to work with field map (WL) + take scale factor for the field from Db
//
// Revision 1.43  2000/01/04 20:45:04  fisyak
// Add StMagFMaker
//
// Revision 1.42  1999/12/28 22:07:00  fisyak
// Fix typo
//
// Revision 1.41  1999/12/28 21:07:35  didenko
// Add dc99 tag for December 99 test run, remove StEvent from default output
//
// Revision 1.40  1999/12/20 22:53:16  didenko
// libtls => tls
//
// Revision 1.39  1999/12/18 00:23:46  fisyak
// Add runco branch
//
// Revision 1.38  1999/12/01 02:06:19  didenko
// Add tpc to y1b, dependence of St_global from St_tpc and St_svt
//
// Revision 1.37  1999/11/29 22:32:05  fisyak
// Add St_l3 for St_l3Clufi_Maker
//
// Revision 1.36  1999/11/29 21:38:26  fisyak
// Add Dave Hardtke corrections, clean up print outs
//
// Revision 1.35  1999/11/20 20:50:52  fisyak
// Rewmove consistency check from BFC table
//
// Revision 1.34  1999/11/18 23:34:17  fisyak
// Add l3 chain with new clustering, add ChainMaker to remove ugly print out
//
// Revision 1.33  1999/11/16 16:10:01  fisyak
// Remove ambiguity between dst maker and dst chain (Cdst)
//
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
// Clean up fFileOut treatment
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
