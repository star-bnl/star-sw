//_____________________________________________________________________
// @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.cxx,v 1.187 2001/04/03 18:17:33 didenko Exp $
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
#include "StEventMaker/StEventMaker.h"
#include "StDbBroker/StDbBroker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StChallenger/StChallenger.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "StMessMgr.h"
//_____________________________________________________________________
Bfc_st BFC[] = {
  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"SD97"        ,""  ,"","db"                                ,"","","Turn on 1997 test parameters",kFALSE},
  {"SD98"        ,""  ,"","db"                                ,"","","Turn on 1998 test parameters",kFALSE},
  {"Y1a"         ,""  ,"","db,calib"         ,"","","YEAR_1A  approximation to year1: TPC+CTB+FTPC",kFALSE},
  {"Y1b"         ,""  ,"","db,calib"         ,"","","YEAR_1B: TPC+CTB+FTPC+calo patch+RICH, no svt",kFALSE},
  {"Y1s"         ,""  ,"","db,calib"        ,"","","YEAR_1S  started in summer: TPC, CONE, AL pipe",kFALSE},
  {"Y1d"         ,"","","db,calib","","","YEAR_1D  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"         ,"","","db,calib","","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"         ,"","","db,calib","","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"ES99"        ,""  ,"","db"          ,"","","Turn on 1999 engineering run simulation parameters",kFALSE},
  {"ER99"        ,""  ,"","db"           ,"","","Turn on 1999 engineering run real data parameters",kFALSE},
  {"DC99"        ,""  ,"","db"       ,"","","Turn on December 1999 engineering run real parameters",kFALSE},
  {"Y1h"      ,"","","db,calib","","","YEAR_1H  fantastic y1:TPC+CTB+FTPC+RICH+caloPatch+svtLadder",kFALSE},
  {"Y2000"    ,"","","db,calib"             ,"","","actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"RY1h"        ,""  ,"","db,calib"                        ,"","","Real data with Year1h geometry",kFALSE},
  {"Y2a"         ,""  ,"","db,calib"                          ,"","","Old (CDR time) complete STAR",kFALSE},
  {"Y2b"         ,"" ,"","db,calib","","","2001 geometry 1st guess:TPC+CTB+FTPC+RICH+CaloPatch+SVT",kFALSE},
  {"Y2001"       ,"","","db,calib","","","year2001: geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Complete"   ,"","","db,calib"         ,"","","complete: new (currently foreseen) complete STAR",kFALSE},
  {"NoDb"        ,""  ,"","HalfField"                               ,"","","Take out Db from Chain",kFALSE},
  {"NoHits"      ,""  ,"",""                            ,"","","Don't write hits into Event.Branch",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Physics"     ,"","","trg"                                        ,"","","Select Physics events",kFALSE},
  {"LaserTest"   ,"","","trg"                                          ,"","","Select Laser events",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"mdc3"        ,""  ,"","cy1h,GeantOut"                               ,"","","MDC3 default chain",kFALSE},
  {"doEvents"    ,""  ,"","xin,event,analysis,NoDb"                                       ,"","","",kFALSE},
  {"drawDst"     ,""  ,"","xin,ry1h,globT,SCL,geant,display,NoDb,TbUtil"                  ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","global,dst,qa,event,analysis,EventQA"                          ,"","","",kFALSE},
  {"C1default"   ,""  ,"","tpc,rrs,rich,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits"
                                                                              ,"","","Year 1 chain",kFALSE}, 
  {"C2default"   ,""  ,"","C1default,ftpc,svt,emcY2"                          ,"","","Year 2 chain",kFALSE}, 
  {"CAdefault"   ,""  ,"","tpc,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits,ftpc,svt,emcY2"
                                                                         ,"","","Assymptotic chain",kFALSE}, 
  {"Cy1a"        ,""  ,"","y1a,C1default"                                ,"","","Turn on chain y1a",kFALSE},
  {"Cy1b"        ,""  ,"","y1b,C1default"                                ,"","","Turn on chain y1b",kFALSE},
  {"Cy1s"        ,""  ,"","y1s,C1default"                                ,"","","Turn on chain y1s",kFALSE},
  {"Cy1d"        ,""  ,"","y1d,C1default"                                ,"","","Turn on chain y1d",kFALSE},
  {"cy1e"        ,""  ,"","y1e,C1default"                                ,"","","Turn on chain y1h",kFALSE},
  {"cy1h"        ,""  ,"","y1h,C1default"                                ,"","","Turn on chain y1e",kFALSE},
  {"Cy2a"        ,""  ,"","y2a,CAdefault"                                ,"","","Turn on chain y2a",kFALSE},
  {"Cy2b"        ,""  ,"","y2b,C2default"                                ,"","","Turn on chain y2b",kFALSE},
  {"C2000"       ,""  ,"","y2000,C1default"                            ,"","","Turn on chain Y2001",kFALSE},
  {"C2001"       ,""  ,"","y2001,C2default"                            ,"","","Turn on chain Y2001",kFALSE},
  {"mdc4"        ,""  ,"","C2001,trs,sss,fss,rrs,GeantOut"          ,"","","Turn on chain for mdc4",kFALSE},
  {"CComplete"   ,""  ,"","Complete,C2default"             ,"","","Turn on chain for Complete STAR",kFALSE},
  {"P00h"        ,""  ,"","ry1h,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},
  {"P2000"       ,""  ,"","y2000,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"OPTIONS     ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
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
  {"NoCintDb"    ,""  ,"",""                                   ,"","","Switch off standard Cint Db",kFALSE},
  {"NoCintCalDb" ,""  ,"",""                                      ,"","","Switch off Cint Calib Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug2"      ,""  ,"",""                                            ,"","","Set debug flag = 2",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},  
  {"big"         ,""  ,"",""                                         ,"","","Set NwGEANT =20Mwords",kFALSE},
  {"bigbig"      ,""  ,"",""                                         ,"","","Set NwGEANT =40Mwords",kFALSE},
  {"InTree"      ,""  ,"","in",""                                     ,"","bfcTree Input Tree name",kFALSE},
  {"OutTree"     ,""  ,"","Tree",""                                  ,"","bfcTree Output Tree name",kFALSE},
  {"DstOut"      ,""  ,"","Tree"                                       ,"","","Write dst to StTree",kFALSE},
  {"EvOut"       ,""  ,"","Tree"                                   ,"","","Write StEvent to StTree",kFALSE},
  {"GeantOut"    ,""  ,"","Tree"                                ,"","","Write g2t tables to StTree",kFALSE},
  {"TrsOut"      ,""  ,"","Tree"                                ,"","","Write Trs output to StTree",kFALSE},
  {"Simu"        ,""  ,"",""                                                ,"","","Simulated Data",kFALSE},
  {"HitsBranch"  ,""  ,"",""  ,"","","take out points from dst branch and put them into HitsBranch",kFALSE},
  {"paw"         ,""  ,"",""                                      ,"","","Allocate memory for pawc",kFALSE},
  {"TrsOut"      ,""  ,"","Tree"                                ,"","","Write Trs output to StTree",kFALSE},
  {"AllEvent"    ,""  ,"","Tree"                               ,"","","Write whole event to StTree",kFALSE},
  {"AllTables"   ,""  ,"","",""                                     ,"St_Tables","Load Star Tables",kFALSE},
  {"ExB"         ,""  ,"","",""                       ,"","Activate ExB correction in St_tpt_Maker",kFALSE},
  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Tables      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
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
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"vpd"         ,""  ,"","vpd_T",""                                                   ,"St_vpd","",kFALSE},
  {"tls"         ,""  ,"","",""                                                           ,"tls","",kFALSE},
  {"daq"         ,""  ,"","",""                         ,"StDaqLib,StDAQMakerLib","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                             ,"StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T",""                                  ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"in"          ,""  ,"","xin"                                               ,"","","Alias to xin",kFALSE},
  {"xin"         ,""  ,"",""              ,"StIOMaker","StIOMaker","Read [XDF|DAQ|ROOT] input file",kFALSE},
  {"xdf2root"    ,""  ,"",""                                   ,"","xdf2root","Read XDF input file",kFALSE},
  {"geant"       ,"geant","","NoFieldSet,geomT,gen_T,sim_T"
                                         ,"St_geant_Maker","geometry,St_g2t,St_geant_Maker","GEANT",kFALSE}, 
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"db"          ,"db","","StDbT,xdf2root"       ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"dbutil"      ,""  ,"","SCL"                            ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"calib"       ,"calib","","xdf2root"          ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Valid Db    ","Versions   ","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"DbV"         ,""  ,"","db,calib,ry1h"                   ,"","","19940614/0 Db Version for none",kFALSE},
  {"DbV0614"     ,""  ,"","db,calib,ry1h"                  ,"","","20000614/0 Db Version for p00hd",kFALSE},
  {"DbV0624"     ,""  ,"","db,calib,ry1h"                ,"","","20000624/0 Db Version for p00hd_1",kFALSE},
  {"DbV0713"     ,""  ,"","db,calib,ry1h"                  ,"","","20000713/0 Db Version for p00he",kFALSE},
  {"DbV0727"     ,""  ,"","db,calib,ry1h"                  ,"","","20000727/0 Db Version for p00he",kFALSE},
  {"DbV0819"     ,""  ,"","db,calib,ry1h"                  ,"","","20000819/0 Db Version for p00hg",kFALSE}, 
  {"DbV1123"     ,""  ,"","db,calib,ry1h" ,"","","20001123/0 Db w/o TpcDriftVel. from StTpcT0Maker",kFALSE}, 
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"magF"        ,"","","NoFieldSet,StDbT,db","StMagFMaker","StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"l0"          ,"l0Chain","","trg_T,globT,ctf,trg"                        ,"StMaker","StChain","",kFALSE}, 
  {"ctf"         ,"ctf","l0Chain","ctf_T,db"               ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE}, 
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE}, 
  {"trg"         ,"trg","l0Chain","trg_T,globT,db"         ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"tpc"         ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StMaker","StChain","",kFALSE},
  {"Trs"         ,"","tpcChain","scl,tpcDB,tpc_daq,Simu"              ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"Mixer"       ,"tpc_raw","","","StMixerMaker"  ,"StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpcChain","tpc_T"              ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,"","tpcChain","Simu"                             ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tpc_T,tls"        ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"Velo"        ,"","tpcChain","tpc_T,tls"                         ,"StVeloMaker","StVeloMaker","",kFALSE},
  {"TpcHitFilter","tpc_hit_filter","tpcChain",""    ,"StTpcHitFilterMaker","StTpcHitFilterMaker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpcChain","tpc_T,tls,"     ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"TpcT0"      ,"TpcT0","","tpc_T,svt_T,ctf_T,ftpcT,globT,tls,db,tpcDB,tpc_daq,kalman,ry1h","StTpcT0Maker",
                   "St_tpc,St_tcl_Maker,St_tpt_Maker,St_svt,St_global,St_dst_Maker,StTpcT0Maker","",kFALSE},
  {"ChargeStep","","","tpc_T,globT,tls,db,tpcDB,tpc_daq","StChargeStepMaker","StChargeStepMaker","",kFALSE},
  {"laser"       ,"tpc_tracks","LaserTest,tpcChain","tdaq,tpc,-tpt,-PreVtx"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},  
  {"PreVtx"     ,"","tpcChain","tpt,SCL,sim_T,tpc_T,svt_T,ftpcT,globT,ctf_T",
                                       "StPreVertexMaker","St_tpc,St_svt,St_global,St_dst_Maker","",kFALSE},
  {"svt"         ,"svtChain","","svt_T,SvtCL,Est"                           ,"StMaker","StChain","",kFALSE},
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"SvtSlowSim","svtChain","SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                                  ,"StSvtSimulationMaker","StSvtSimulationMaker","",kFALSE},
  {"ssd"         ,"","","sls,spa,scf,scm,sce"                                             ,"","","",kFALSE},
  {"srs"         ,"svt_hits","svtChain","tls,Simu,SvtCL,-SvtCL,-sss,-SvtSlowSim,Simu"
                                                    ,"St_srs_Maker","St_tpc,St_svt,St_srs_Maker","",kFALSE},
  {"sls"     ,"","svtChain","tls,Simu,SvtCL","St_sls_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"spa"     ,"","svtChain","tls,Simu,SvtCL","St_spa_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain","SvtCL"                  ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"      ,"SvtClu","svtChain","SvtCL"   ,"StSvtClusterMaker","StEvent,StSvtClusterMaker","",kFALSE},  
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},  
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},  
  {"stk"        ,"svt_tracks","svtChain","tls,SvtCL","St_stk_Maker","St_tpc,St_svt,St_stk_Maker","",kFALSE},  
  {"scf"      ,"","svtChain",""                ,"St_scf_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"scm"      ,"","svtChain",""                ,"St_scm_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"sce"      ,"","svtChain",""                   ,"St_sce_Maker","St_tpc,St_svt,StSsdEvalMaker","",kFALSE},
  {"Est"      ,"","svtChain","globT"                 ,"StEstMaker","St_global,St_svt,StEstMaker","",kFALSE},
  {"Ftpc"        ,"ftpcChain"  ,"","ftpcT,fcl,fpt,Fglobal,Fprimary"         ,"StMaker","StChain","",kFALSE},
  {"fss"    ,"ftpc_raw","ftpcChain","SCL,Simu",        "StFtpcSlowSimMaker","StFtpcSlowSimMaker","",kFALSE},
  {"Fcl"    ,"ftpc_hits","ftpcChain","SCL"
                                  ,"StFtpcClusterMaker","StDaqLib,StDAQMaker,StFtpcClusterMaker","",kFALSE},
  {"fpt"         ,"ftpc_tracks","ftpcChain","SCL"         ,"StFtpcTrackMaker","StFtpcTrackMaker","",kFALSE},
  {"emc"    ,"emcChain","","geant,emc_T,tpc_T,db,calib,ems,emh,PreEcl"      ,"StMaker","StChain","",kFALSE},
  {"ems","emc_raw","emcChain","geant,emc_T,EmcUtil","St_ems_Maker","StEvent,St_emc,St_ems_Maker","",kFALSE},
  {"emh" ,"emc_hits","emcChain","geant,emc_T,tpc_T,EmcUtil","St_emc_Maker","St_emc,St_emc_Maker","",kFALSE},
  {"emcY2"    ,"emcY2","","geant,emc_T,tpc_T,db,calib,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim" ,"emcRaw","emcY2","geant,emc_T,EmcUtil","StEmcSimulatorMaker","StMcEvent,StEmcSimulatorMaker", 
                                                                           "New simulator for BEMC",kFALSE},
  {"global"      ,"globalChain","","globT,Match,primary,v0,xi,kink,dst,SCL,dEdx"
                                                              ,"StMaker","St_tpc,St_svt,StChain","",kFALSE},
  {"Match"       ,"match","globalChain","SCL,tpc_T,svt_T,globT,tls"
                                                 ,"StMatchMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Fglobal"    ,"fglobal","globalChain","SCL,tables,tls"
                                                   ,"StFtpcGlobalMaker","St_global,St_dst_Maker","",kFALSE},
  {"point"      ,"point","globalChain","SCL,tables,tls","StPointlMaker","St_global,St_dst_Maker","",kFALSE},
  {"Primary"     ,"primary","globalChain","SCL,globT,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Fprimary"    ,"fprimary","globalChain","SCL,tables,tls"
                                                  ,"StFtpcPrimaryMaker","St_global,St_dst_Maker","",kFALSE},
  {"V0"          ,"v0","globalChain","SCL,globT,tls","StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"          ,"xi","globalChain","SCL,globT,tls","StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"   ,"kink","globalChain","SCL,globT,tls","StKinkMaker" ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"dst"         ,"dst","globalChain","dstOut,SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT","St_dst_Maker" 
                                                                ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"dEdx"       ,"dEdx","globalChain","globT,tpcDb,TbUtil"          ,"StdEdxMaker","StdEdxMaker","",kFALSE},
  {"Event"       ,"","","globT,SCL"                       ,"StEventMaker","StEvent,StEventMaker","",kFALSE},
  {"PostEmc"     ,"PostChain","","geant,emc_T,tpc_T,db,calib,PreEcl,EmcUtil","StMaker","StChain","",kFALSE},
  {"PreEcl"      ,"preecl","PostChain",""                 ,"StPreEclMaker",
                                         "StPreEclMaker,StEmcSimulatorMaker,St_emc,St_ems_Maker","",kFALSE},
  {"Epc"         ,"epc","PostChain","PreEcl,Match,EmcUtil"            ,"StEpcMaker","StEpcMaker","",kFALSE},
  {"rich"        ,"RichChain","","rch,RichPiD",                    "StMaker","StChain","RICH chain",kFALSE},
  {"Rrs"         ,"","RichChain","sim_T,Simu"                         ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rch"         ,"","RichChain","sim_T,globT"             ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"RichPiD"     ,"","RichChain","Event"                      ,"StRichPIDMaker","StRichPIDMaker","",kFALSE},
  {"l3"          ,"l3Chain","","l3cl,l3t"                                   ,"StMaker","StChain","",kFALSE},
  {"l3cl"        ,"","l3Chain","l3_T"               ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3Chain","l3_T"                       ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"l3onl"       ,"","",""                      ,"Stl3RawReaderMaker","St_l3,Stl3RawReaderMaker","",kFALSE},
  {"analysis"    ,"","","Event"           ,"StAnalysisMaker","StAnalysisMaker","Exampe of Analysis",kFALSE},
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra" ,"","",""                            ,"StRichSpectraMaker","StRichSpectraMaker","",kFALSE},
  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
  {"Flow"        ,"","TagsChain","Event"   ,"StFlowMaker","StEvent,StEventUtilities,StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","TagsChain","Event,Flow"                 ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
  {"FlowAnalysis","","TagsChain","Event,Flow"       ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","Event"              ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"SpectraTag"  ,"","TagsChain","Event"                ,"StSpectraTagMaker","StSpectraTagMaker","",kFALSE},
  {"EbyeScaTags" ,"","TagsChain","Event"              ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
  {"PCollTag"    ,"","TagsChain","Event"                    ,"StPCollTagMaker","StPCollTagMaker","",kFALSE},
  {"tags"        ,"","TagsChain",
                 "TagsChain,globT,Event,FlowTag,StrangeTags,SpectraTag,EbyeScaTags,TpcTag,PCollTag"    
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"QA"          ,"QA","","globT,SCL,global"                        ,"St_QA_Maker","St_QA_Maker","",kFALSE},
  {"EventQA"     ,"EventQA","","Event"                           ,"StEventQAMaker","St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"","","SCL,St_geom"               ,"StEventDisplayMaker","StEventDisplayMaker","",kFALSE},
  {"Mc"          ,"McChain","","sim_T,globT,McAss,McAna"                    ,"StMaker","StChain","",kFALSE},
  {"McEvent"     ,"","McChain","Event,EmcUtil",      "StMcEventMaker","StMcEvent,StMcEventMaker","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAna"       ,"","McChain","McEvent",                "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"LAna"        ,"","","in,RY1h,geant,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE}, 
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE}
};
Int_t NoChainOptions = sizeof (BFC)/sizeof (Bfc_st);
class StEvent;
StEvent *Event;
class StIOMaker; 
class St_geant_Maker; St_geant_Maker *geantMk = 0;   
class St_db_Maker;    
static St_db_Maker *dbMk    = 0; 
static St_db_Maker *calibMk = 0; 
class StTreeMaker;    
static Bool_t kMagF = kFALSE; 
ClassImp(StBFChain)

//_____________________________________________________________________________
StBFChain::StBFChain(const char *name):StChain(name),fXdfOut(0),fSetFiles(0),fInFile(0),fFileOut(0),fXdfFile(0) {
   fBFC = new Bfc_st[NoChainOptions];
   memcpy (fBFC, &BFC, sizeof (BFC));
}
//_____________________________________________________________________________
StBFChain::~StBFChain(){
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
  Int_t i, iFail=0;
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
	    if (!calibMk && ! GetChain()->GetMaker(fBFC[i].Name)) {
	      if (!GetOption("NoCintCalDb"))
		calibMk = new St_db_Maker(fBFC[i].Name,"$STAR_ROOT/calib","$PWD/calib");
	      else
		calibMk = new St_db_Maker(fBFC[i].Name,"$PWD/calib");
	    }
	    mk = calibMk;
	  }
	  if (Key.CompareTo("db",TString::kIgnoreCase) == 0) {
            if (!dbMk && ! GetChain()->GetMaker(fBFC[i].Name)) {
	      if (!GetOption("NoMySQLDb") && !GetOption("NoCintDb"))
		dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
	      else {
		if (GetOption("NoMySQLDb") && GetOption("NoCintDb"))
		  dbMk = new St_db_Maker(fBFC[i].Name,"$PWD/StarDb");
		else {
		  if (GetOption("NoMySQLDb")) 
		    dbMk = new St_db_Maker(fBFC[i].Name,"$STAR/StarDb","$PWD/StarDb");
		  if (GetOption("NoCintDb"))
		    dbMk = new St_db_Maker(fBFC[i].Name,"MySQL:StarDb","$PWD/StarDb");
		}
	      }
	    }
	    mk = dbMk;
	  }
	  if (!mk) status = kStErr;
	  else {
	    strcpy (fBFC[i].Name, (Char_t *) mk->GetName());
	    if (GetOption("Simu"))    mk->SetFlavor("sim+ofl");
	    else                      mk->SetFlavor("ofl");
	  }
	  continue;
	}
	if (maker == "StIOMaker" && fSetFiles) {
	  StIOMaker *inpMk=0;     
	  if (GetOption("InTree")) {
	    Char_t line[80] = "bfcTree";
	    Int_t k = kOpt("InTree");
	    sscanf(fBFC[k].Comment,"%s",line);
	    inpMk = new StIOMaker("inputStream","r",fSetFiles,line);
	  }
	  else inpMk = new StIOMaker("inputStream","r",fSetFiles);
	  if (inpMk) {
	    strcpy (fBFC[i].Name,(Char_t *) inpMk->GetName());
	    SetInput("StDAQReader",".make/inputStream/.make/inputStream_DAQ/.const/StDAQReader");
	    SetInput("geant",".make/inputStream/.make/inputStream_XDF/.data/event/geant/Event");
	    if (GetOption("ReadAll")) inpMk->SetBranch("*",0,"r");	//activate all branches
	  }
	  else status = kStErr;
	  continue;
	}
	if (maker == "StTreeMaker" && fFileOut) {
	  StTreeMaker    *treeMk  = 0;
	  if (GetOption("OutTree")) {
	    Char_t line[80] = "bfcTree";
	    Int_t k = kOpt("OutTree");
	    sscanf(fBFC[k].Comment,"%s",line);
	    treeMk = new StTreeMaker("outputStream",fFileOut->Data(),line);
	  }
	  else treeMk = new StTreeMaker("outputStream",fFileOut->Data());
	  if (treeMk) {
	    strcpy (fBFC[i].Name,(Char_t *) treeMk->GetName());
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
	      strcpy (fBFC[i].Name,(Char_t *) geantMk->GetName());
	      geantMk->SetActive(kFALSE);
	      if (GetOption("fzin") || GetOption("gstar")) geantMk->SetActive(kTRUE);
	      SetGeantOptions();
	    }
	  }
	  if (!geantMk) status = kStErr;
	  continue;
	}
	StMaker *mk = 0;
	if (maker == "StTpcDbMaker") mk = GetChain()->GetMaker(fBFC[i].Name);
	if (!mk) {
	  if (strlen(fBFC[i].Name) > 0) mk = New(fBFC[i].Maker,fBFC[i].Name);
	  else  {
	    mk = New(fBFC[i].Maker);
	    if (mk) strcpy (fBFC[i].Name,(Char_t *) mk->GetName());
	  }
	}
	// special maker options 
	if (mk) {
	  if (maker == "St_dst_Maker") SetInput("dst",".make/dst/.data/dst");
	  if (maker == "St_dst_Maker" && GetOption("HitsBranch")) mk->SetMode(2); 
	  if (maker == "StMatchMaker" && !GetOption("Kalman")) mk->SetMode(-1);
	  if (maker == "St_tpt_Maker" && GetOption("ExB")) mk->SetMode(1); // Al Saulys request
	  if (maker == "St_tcl_Maker") {
	    St_tcl_Maker *tclMk = (St_tcl_Maker *) mk;
	    if (GetOption("EastOff")) tclMk->EastOff(); 
	    if (GetOption("WestOff")) tclMk->WestOff(); 
	    if (GetOption("AllOn"))   tclMk->AllOn(); 
	  }
	  if (maker == "St_tpcdaq_Maker") {
	    if (GetOption("Trs")) mk->SetMode(1); // trs
	    else                  mk->SetMode(0); // daq
	  }
	  if (maker == "StRchMaker") {
	    if (GetOption("Rrs")) mk->SetMode(1); // rrs
	    else                  mk->SetMode(0); // daq
	  }
	  if (maker == "StV0Maker" && GetOption("Ev03")) mk->SetMode(1); // Turn on alternative V0 method
	  if (maker == "St_trg_Maker") {
	    Int_t mode = 0;
	    if (GetOption("Physics"))   mode += 1;
	    if (GetOption("LaserTest")) mode += 2;
	    if (mode) mk->SetMode(mode);
	  }
	  if (GetOption("dst") && GetOption("NoHits") && maker == "StEventMaker") {
	    StEventMaker *EvMk = (StEventMaker *) mk;
	    EvMk->doLoadTpcHits  = kFALSE;
	    EvMk->doLoadFtpcHits = kFALSE;
	    EvMk->doLoadSvtHits  = kFALSE;
	    EvMk->doLoadSsdHits  = kFALSE;
	  }
	}
        else status = kStErr;
	if (saveMk) saveMk->cd();
	if (status != kStOk && i != iFail) {
	  printf("QAInfo: ======================================\n");
	  printf("QAInfo: problem with Instantiation of %s\n",fBFC[i].Maker);
	  printf("QAInfo: ======================================\n");
	  iFail = i;
	}
      }
    }
  }
  if (fXdfFile) {
    fXdfOut = new St_XDFFile(fXdfFile->Data(),"wb"); 
    if (!fXdfOut) status = kStErr;
  }
  SetDbOptions();
  //  PrintQAInfo();
  PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv
  if (GetOption("MakeDoc")) MakeDoc();
  if (GetOption("Debug")) SetDEBUG(1);
  if (GetOption("Debug2")) SetDEBUG(2);
  return status;
}
Int_t StBFChain::Finish()
{
  if (fBFC) {
    delete [] fBFC; fBFC = 0;
    SafeDelete (fXdfOut);
    if (fTFile) {fTFile->Write(); fTFile->Flush(); fTFile->Close(); SafeDelete (fTFile);}
    return StMaker::Finish();
  }
  else return kStOK;
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
      printf (" %3d:[-]%-13s:%-12s:%-12s:%-12s :%s :%s :%s\n"
	      ,k,fBFC[k].Key,fBFC[k].Name,fBFC[k].Chain,fBFC[k].Opts,fBFC[k].Maker,fBFC[k].Libs,fBFC[k].Comment);
    return; 
  }         
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  printf ("==============================================\n");
  gMessMgr->QAInfo() << "============= You are in " << STAR_VERSION.Data() << " ===============" << endm;
  TString tChain(Chain);
  gMessMgr->QAInfo() << "Requested chain " << GetName() << " is :\t" << tChain.Data() << endm;
  TObjArray Opts;
  ParseString(tChain,Opts);
  TIter next(&Opts);
  TObjString *Opt;
  while ((Opt = (TObjString *) next())) {
    TString string = Opt->GetString();
    Int_t in = string.Index("=");
    Int_t kgo;
    if (in <= 0) {
      string.ToLower(); //printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(string.Data());
      if (kgo != 0) SetOption(kgo);
    }
    else {// string with  "="
      TString substring(string.Data(),in);
      substring.ToLower(); //printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(substring.Data());
      if (kgo > 0) {
	memset(fBFC[kgo].Comment,0,200); // be careful size of Comment
	SetOption(kgo);
	TString Comment(string.Data()+in+1,string.Capacity()-in-1);
	strcpy (fBFC[kgo].Comment, Comment.Data());
	printf (" Set        %s = %s\n", fBFC[kgo].Key,fBFC[kgo].Comment);
      }
    }
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
  else {
    if (GetOption("fzin")) SetOption("-fzin");
    if (GetOption("in") || GetOption("xin")) {
      SetOption("-xin");
      SetOption("-in");
    }
  }
  if (!GetOption("FieldOn") && !GetOption("FieldOff") && 
      !GetOption("HalfField") && !GetOption("ReverseField") &&    
      !GetOption("fzin") &&  !GetOption("gstar")) SetOption("magF"); 
  if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval"); 
  if (!GetOption("event")) SetOption("-analysis");
  if (GetOption("NoDb")) {SetOption("-db"); SetOption("-calib");}
  // Print set values
  St_Bfc *Bfc = new St_Bfc("BFChain",NoChainOptions);
  AddRunco(Bfc);
  for (k = 1; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      gMessMgr->QAInfo() << "================== " << k << "\t" 
			 << fBFC[k].Key << "\tis ON \t:" << fBFC[k].Comment << endm;
      Bfc->AddAt(&fBFC[k]);
    }
  }
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void StBFChain::Set_IO_Files (const Char_t *infile, const Char_t *outfile){
  TString gc("");
  if (infile) {
    if (strlen(infile) > 2) {
      gc = TString(infile,3);
      gc.ToLower();
    }
    if (gc == "gc:") SetGC(infile+3);
    else             SetInputFile(infile);
  }
  SetOutputFile(outfile);
}
//_____________________________________________________________________
void StBFChain::SetGC (const Char_t *queue){
  TString Queue(queue);
  gMessMgr->QAInfo() << "Requested GC queue is :\t" << Queue.Data() << endm;
  TObjArray Opts;
  ParseString(Queue,Opts);
  TIter next(&Opts);
  TObjString *Opt;
  static TString ARGV[40];
  Int_t Argc = -1;
  while ((Opt = (TObjString *) next())) {
    TString string = Opt->GetString();
    const Char_t *argv = string.Data();
    if (argv[0] == '-') {
      switch (argv[1]) {
      case 'o':
      case 'i':
      case 'c':
      case 'q':
      case 's':
      case 'n':
      case 'm':
      case 't':
      case '-': // now do --options, they get added to Config
        ARGV[++Argc] = string.Data();
        Argc++;        
	break;
      default :
	gMessMgr->QAInfo() << "Unrecognized option :\t" << string << endm;
	break;
      }
    }
    else if (Argc > 0) {ARGV[Argc] += " "; ARGV[Argc] += string;}
  }
  Opts.Delete();
  fSetFiles = (StFileI *)StChallenger::Challenge();
  fSetFiles->SetDebug();
  Argc++;
  Char_t **Argv = new Char_t* [Argc];
  for (int i=0;i<Argc;i++)  {Argv[i] = (Char_t *) ARGV[i].Data();}
  fSetFiles->Init(Argc,(const Char_t **) Argv);
}
//_____________________________________________________________________
void StBFChain::SetInputFile (const Char_t *infile){
  // define input file
  if (infile) fInFile = new TString(infile);
  if (!GetOption("NoInput")) {
    if (!fInFile && GetOption("fzin")) {
      fInFile = new TString("/star/rcf/simu/cocktail/hadronic/default/lowdensity/");
      if (GetOption("y1h")) fInFile->Append("year_1h/hadronic_on/Gstardata/rcf0078/hc_lowdensity.400_evts.fz");
      else 
	if (GetOption("y2a")) fInFile->Append("year_2a/hadronic_on/Gstardata/rcf0079/hc_lowdensity.400_evts.fz");
	else {printf ("for fzin Option In file has not been defined. Exit!\n"); gSystem->Exit(1);}
      printf ("Use default input file %s for %s \n",fInFile->Data(),"fzin");
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
}
//_____________________________________________________________________
void StBFChain::SetOutputFile (const Char_t *outfile){
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
		GetOption("DC99"))    geantMk->LoadGeometry("detp geometry YEAR_1A"); 
      else {if (GetOption("Y1b"))     geantMk->LoadGeometry("detp geometry YEAR_1B");
      else {if (GetOption("Y1E"))     geantMk->LoadGeometry("detp geometry YEAR_1E");
      else {if (GetOption("Y1h") || 
		GetOption("RY1h"))    geantMk->LoadGeometry("detp geometry YEAR_1H");
      else {if (GetOption("Y1s"))     geantMk->LoadGeometry("detp geometry YEAR_1S");
      else {if (GetOption("Y2000"))   geantMk->LoadGeometry("detp geometry year2000");
      else {if (GetOption("Y2a"))     geantMk->LoadGeometry("detp geometry YEAR_2A");
      else {if (GetOption("Y2001"))   geantMk->LoadGeometry("detp geometry year2001");
      else {if (GetOption("Y2b"))     geantMk->LoadGeometry("detp geometry YEAR_2b");
      else {if (GetOption("Y2001"))   geantMk->LoadGeometry("detp geometry year2001");
      else {if (GetOption("Complete"))geantMk->LoadGeometry("detp geometry complete");
      else                            geantMk->LoadGeometry("detp geometry year2001");}}}}}}}}}}
	    if (GetOption("gstar")) {
	      geantMk->Do("subevent 0;");
	      // gkine #particles partid ptrange yrange phirange vertexrange 
	      geantMk->Do("gkine 80 6 1. 1. -4. 4. 0 6.28  0. 0.;");
	      geantMk->Do("mode g2tm prin 1;");
	      //  geantMk->Do("next;");
	      //  geantMk->Do("dcut cave z 1 10 10 0.03 0.03;");
	      if (GetOption("Debug") ||GetOption("Debug2")) geantMk->Do("debug on;");
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
  Int_t i;
  Int_t Idate=0, Itime=0;
  for (i = 1; i< NoChainOptions; i++) {// Load Libraries if any
    if (fBFC[i].Flag && !strncmp(fBFC[i].Key ,"DbV",3)) 
      sscanf(fBFC[i].Comment,"%d/%d",&Idate,&Itime);
  }
  StMakerIter nextMaker(this);
  StMaker *maker;
  while ((maker = nextMaker.NextMaker())) { 
    if (!strcmp(maker->ClassName(),"St_db_Maker")) {
      St_db_Maker *db = (St_db_Maker *) maker;
        if (GetOption("SD97")) db->SetDateTime("sd97");
  else {if (GetOption("SD98")) db->SetDateTime("sd98");
  else {if (GetOption("Y1a"))  db->SetDateTime("year_1a");
  else {if (GetOption("Y1b"))  db->SetDateTime("year_1b");
  else {if (GetOption("Y1s"))  db->SetDateTime("year_1s");
  else {if (GetOption("ES99")) db->SetDateTime("es99");
  else {if (GetOption("ER99")) db->SetDateTime("er99");
  else {if (GetOption("DC99")) db->SetDateTime("dc99");
  else {if (GetOption("Y1d"))  db->SetDateTime("year_1d");
  else {if (GetOption("Y1e"))  db->SetDateTime("year_1e");
  else {if (GetOption("Y1h"))  db->SetDateTime("year_1h");
  else {if (GetOption("Y2000"))  db->SetDateTime("year_1h");
  else {if (GetOption("Y2a"))  db->SetDateTime("year_2a");
  else {if (GetOption("Y2b"))  db->SetDateTime("year_2b"); 
  else {if (GetOption("Simu")) db->SetDateTime("year_2b");
  else {if (GetOption("Y2001"))  db->SetDateTime("year_2b");
  }}}}}}}}}}}}}}}
	gMessMgr->QAInfo() << db->GetName() 
			   << " Maker set time = " 
			   << db->GetDateTime().GetDate() << "." 
			   << db->GetDateTime().GetTime() << endm;
      if (Idate) {
	db->SetMaxEntryTime(Idate,Itime);
	cout << "\tSet DataBase max entry time " << Idate << "/" << Itime 
	     << " for St_db_Maker(\"" << db->GetName() <<"\")" << endl;
      }
    }
  }
}
//_____________________________________________________________________
void StBFChain::SetTreeOptions()
{
  StTreeMaker *treeMk = (StTreeMaker *) GetMaker("outputStream");
  if (!treeMk) return;
  treeMk->SetBranch("histBranch");
  if (GetOption("dstOut"))      {
    treeMk->IntoBranch("dstBranch","dst");
    if (GetOption("HitsBranch")) {
      treeMk->SetBranch("dstHitsBranch");
      treeMk->IntoBranch("dstHitsBranch","dst/.data/Hits");
    }
    else treeMk->IntoBranch("dstBranch","dst/.data/Hits");
    treeMk->IntoBranch("dstBranch","dst/.data/dst");
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
    if (GetOption("global")) treeMk->IntoBranch("globalBranch","global/.data");
  }
  else if (GetOption("GeantOut") && geantMk) treeMk->IntoBranch("geantBranch","geant");
  else if (GetOption("TrsOut") && GetOption("Trs")) treeMk->IntoBranch("TrsBranch","Trs");
}
