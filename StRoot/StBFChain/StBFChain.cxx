//_____________________________________________________________________
// @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.cxx,v 1.295 2002/05/18 01:01:26 jeromel Exp $
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
#include "St_geant_Maker/St_geant_Maker.h"
#include "StEventMaker/StEventMaker.h"
#include "StDbBroker/StDbBroker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTreeMaker/StTreeMaker.h"
#include "StIOMaker/StIOMaker.h"
#include "StChallenger/StChallenger.h"
#include "St_tcl_Maker/St_tcl_Maker.h"
#include "St_tpt_Maker/St_tpt_Maker.h"
#include "St_dst_Maker/StPrimaryMaker.h"
#include "St_dst_Maker/StVertexMaker.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StMessMgr.h"
//_____________________________________________________________________
Bfc_st BFC1[] = {
  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"SD97"  ,"","","db,detDb"                                  ,"","","Turn on 1997 test parameters",kFALSE},
  {"SD98"  ,"","","db,detDb"                                  ,"","","Turn on 1998 test parameters",kFALSE},
  {"Y1a"   ,"","","db,calib,detDb"           ,"","","YEAR_1A  approximation to year1: TPC+CTB+FTPC",kFALSE},
  {"Y1b"   ,"","","db,calib,detDb"           ,"","","YEAR_1B: TPC+CTB+FTPC+calo patch+RICH, no svt",kFALSE},
  {"Y1s"   ,"","","db,calib,detDb"          ,"","","YEAR_1S  started in summer: TPC, CONE, AL pipe",kFALSE},
  {"Y1d"   ,"","","db,calib,detDb","","","YEAR_1D  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"   ,"","","db,calib,detDb","","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"   ,"","","db,calib,detDb","","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"ES99"  ,"","","db,detDb"            ,"","","Turn on 1999 engineering run simulation parameters",kFALSE},
  {"ER99"  ,"","","db,detDb"             ,"","","Turn on 1999 engineering run real data parameters",kFALSE},
  {"DC99"  ,"","","db,detDb"         ,"","","Turn on December 1999 engineering run real parameters",kFALSE},
  {"Y1h"   ,"","","db,calib,detDb",
                                 "","","YEAR_1H fantastic y1:TPC+CTB+FTPC+RICH+caloPatch+svtLadder",kFALSE},
  {"Y2000" ,"","","db,calib,detDb"          ,"","","actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"RY1h"  ,"","","db,calib,detDb,VtxOffSet"                ,"","","Real data with Year1h geometry",kFALSE},
  {"RY2000","","","db,calib,detDb,VtxOffSet","","","actual 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2000a","","","db,calib,detDb"      ,"","","alternate 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2001","","","db,calib,detDb"          ,"","","actual 2001: Real data with Year2001 geometry ",kFALSE},
  {"Y2a"   ,"","","db,calib,detDb"                            ,"","","Old (CDR time) complete STAR",kFALSE},
  {"Y2b"   ,"","","db,calib,detDb" ,"","","2001 geometry 1st guess:TPC+CTB+FTPC+RICH+CaloPatch+SVT",kFALSE},
  {"Y2001" ,"","","db,calib,detDb","","","year2001: geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Y2001n","","","db,calib,detDb","","",
                                     "year2001: new geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Complete","","","db,calib,detDb"      ,"","","complete: new (currently foreseen) complete STAR",kFALSE},
  {"NoDb"  ,""  ,"","HalfField"                                     ,"","","Take out Db from Chain",kFALSE},
  {"NoHits",""  ,"",""                                  ,"","","Don't write hits into Event.Branch",kFALSE},

  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Physics"     ,"","","trg"                                        ,"","","Select Physics events",kFALSE},
  {"LaserTest"   ,"","","trg"                                          ,"","","Select Laser events",kFALSE},
  {"PulserSvt"   ,"","","trg"                                     ,"","","Select SVT Pulser events",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"mdc3"        ,""  ,"","cy1h,GeantOut"                               ,"","","MDC3 default chain",kFALSE},
  {"doEvents"    ,""  ,"","xin,event,analysis,NoDb"                                       ,"","","",kFALSE},
  {"drawDst"     ,""  ,"","xin,ry1h,globT,SCL,geant,display,NoDb,TbUtil"                  ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","global,dst,qa,event,analysis,EventQA"                          ,"","","",kFALSE},
  {"C1default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits"    ,"","","Year 1 chain",kFALSE},
  {"C2default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,ftpc,svt,emcY2"
                                                                              ,"","","Year 2 chain",kFALSE},
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
  {"C2000"       ,""  ,"","y2000,C1default"                            ,"","","Turn on chain Y2000",kFALSE},
  {"C2001"       ,""  ,"","y2001,C2default"                            ,"","","Turn on chain Y2001",kFALSE},
  {"MDC4"        ,""  ,"","C2001,trs,srs,fss,rrs,big,GeantOut"      ,"","","Turn on chain for MDC4",kFALSE},
  {"MDC4New"     ,""  ,"","y2001n,C2default,trs,srs,fss,rrs,big,GeantOut","","",
                                                     "Turn on chain for MDC4 (for after September)",kFALSE},
  {"PostMDC4"    ,""  ,"","C2001,trs,sss,fss,rrs,big,GeantOut"     ,"","","Turn on Post MDC4 chain",kFALSE},
  {"ppMDC4"      ,""  ,"","pp,C2001,-PreVtx,ppMCTrig,mwc,ppLPeval1,trs,srs,rrs,big,GeantOut",
                                                                    "","","Turn on chain for ppMDC",kFALSE},
  {"CComplete"   ,""  ,"","Complete,C2default"             ,"","","Turn on chain for Complete STAR",kFALSE},

  // Detector combined-chains
  {"SvtD"        ,""  ,"","SvtDb,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit,SvtVtx", "", "",
                                                                               "SVT chain for Data",kFALSE},

  // Year 1 chains
  {"P00h"        ,""  ,"","ry1h,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},
  {"P2000"       ,""  ,"","ry2000,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},

  {"B2000"       ,""  ,"","ry2000a,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2000a"      ,""  ,"","B2000,Corr1","",""              ,"Production chain for summer 2000 data",kFALSE},



  // Year 2 chains. 
  // B2001 is a base-chain for 2001 (with tpc+rhic). 
  {"B2001"       ,""  ,"","ry2001,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2001"       ,""  ,"","B2001,l3onl,tofDat,Corr2,OSpaceZ","",""
                                               ,"Production chain for summer 2001 data (+ l3, tof)",kFALSE},
  {"P2001a"      ,""  ,"","B2001,svt_daq,SvtD,ftpc,l3onl,tofDat,emcDY2,Corr2,OSpaceZ","","" 
                               ,"Production chain for summer 2001 data (+ ftpc, svt, l3, tof, emc)",kFALSE},

  // pp Chains 
  {"pp2001"      ,""  ,"","pp,B2001,-PreVtx,-SpinTag,l3onl,tofDat,emcDY2,Corr2","",""          
                                                                        ,"pp 2001 (+ l3, tof, emc)",kFALSE},
  {"pp2001a"      ,""  ,"","pp2001,svt_daq,SvtD,ftpc","",""                          
                                                             ,"pp 2001 (+ ftpc, svt, l3, tof, emc)",kFALSE},


  // Other chains/Calibration
  {"LaserCal",""  ,"","db,detDb,tpc_daq,tpcDb,tcl,globT,laser,LaserTest","","", 
                                                                          "Laser Calibration Chain",kFALSE},
  {"L3Counter","" ,"","db,detDb,xin,l3count","","",                    "L3 Counter extraction pass",kFALSE},
  {"VtxSeedCal","","",
   "ppOpt,ry2001,in,tpc_daq,tpc,global,-v0,-xi,-kink,-Tree,Physics,-PreVtx,FindVtxSeed,NoEvent,Corr2",
                                                                     "","","Pass0 Vertex evaluator",kFALSE},


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
  {"FieldOn"     ,""  ,"","MagF"                                   ,"","" ,"Constant nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","MagF"                                          ,"","" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","MagF"                                         ,"","","Half Field option",kFALSE},
  {"ReverseField",""  ,"","MagF"                                      ,"","","Reverse Field option",kFALSE},
  {"NoCintDb"    ,""  ,"",""                                   ,"","","Switch off standard Cint Db",kFALSE},
  {"NoCintCalDb" ,""  ,"",""                                      ,"","","Switch off Cint Calib Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug1"      ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
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

  {"Corr1"       ,""  ,"","AlignSectors,ExB,OBmap,OClock,OPr13","","",
                                                      "... AlignSectors,ExB,OBmap,OClock,OPr13 ...",kFALSE},
  {"Corr2"       ,""  ,"","AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC","","",
                                          "... AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC ...",kFALSE},
  {"ExB"         ,""  ,"","",""                       ,"","Activate ExB correction in St_tpt_Maker",kFALSE},
  {"EB1"         ,""  ,"","",""                                     ,"","Force ExB configuration 1",kFALSE},
  {"EB2"         ,""  ,"","",""                                     ,"","Force ExB configuration 2",kFALSE},
  {"OBmap"       ,""  ,"","",""                                          ,"","ExB shape correction",kFALSE},
  {"OTwist"      ,""  ,"","",""                                          ,"","ExB twist correction",kFALSE},
  {"OClock"      ,""  ,"","",""                                     ,"","Clock/tpc rot. correction",kFALSE},
  {"OPr13"       ,""  ,"","",""                                          ,"","PadRow 13 distortion",kFALSE},
  {"OCentm"      ,""  ,"","",""                                   ,"","Central membrane correction",kFALSE},
  {"OECap"       ,""  ,"","",""                                    ,"","EndCap (curved) correction",kFALSE},
  {"OIFC"        ,""  ,"","",""                                         ,"","Field Cage correction",kFALSE},
  {"OSpaceZ"     ,""  ,"","",""                                      ,"","Space Charge corrections",kFALSE},
  {"AlignSectors",""  ,"","",""          ,"","Activate Sector Alignment correction in St_tpt_Maker",kFALSE},

  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},
  {"pp"      ,""  ,"","SpinTag,ppLPfind1,SpinSortA,ppLPprojectA","","","Use pp specific parameters",kFALSE},
  {"ppOpt"       ,""  ,"","pp,-SpinTag,-ppLPfind1,-SpinSortA,-ppLPprojectA",   "","","pp no makers",kFALSE},
  {"VtxOffSet"   ,""  ,"","",""                 ,"","Account Primary Vertex offset from y2000 data",kFALSE},
  {"Calibration" ,""  ,"","",""                                              ,"","Calibration mode",kFALSE},
  {"beamLine"    ,""  ,"","",""                                       ,"","LMV Beam line constrain",kFALSE},
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
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"vpd"         ,""  ,"","vpd_T",""                                                   ,"St_vpd","",kFALSE},
  {"tls"         ,""  ,"","",""                                                           ,"tls","",kFALSE},
  {"daq"         ,""  ,"","",""                            ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                             ,"StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"StEvent"     ,""  ,"","globT,SCL",""                                  ,"StEvent","Load StEvent",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T",""                                  ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"in"          ,""  ,"","xin"                                           ,"","","... Alias to xin",kFALSE},
  {"xin"         ,""  ,"",""              ,"StIOMaker","StIOMaker","Read [XDF|DAQ|ROOT] input file",kFALSE},
  {"xdf2root"    ,""  ,"",""                                   ,"","xdf2root","Read XDF input file",kFALSE},
  {"geant"       ,"geant","","geomT,gen_T,sim_T"
                                  ,"St_geant_Maker","geometry,St_g2t,StMagF,St_geant_Maker","GEANT",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"db"          ,"db"   ,"","StDbT,xdf2root"    ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"svtDb"       ,"svtDb","","SvtCL,db" ,                          "StSvtDbMaker","StSvtDbMaker","",kFALSE},
  {"dbutil"      ,""     ,"","SCL"            ,"","StSvtDbMaker,StDbUtilities","Load StDbUtilities",kFALSE},
  {"calib"       ,"calib","","xdf2root"          ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"detDb"       ,""     ,"",""   ,"StDetectorDbMaker","StDetectorDbMaker","Load StDetectorDbMaker",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Valid Db    ","Versions   ","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"DbV"         ,""  ,"","db,calib,ry1h"                   ,"","","19940614/0 Db Version for none",kFALSE},
  {"DbV0614"     ,""  ,"","db,calib,ry1h"                  ,"","","20000614/0 Db Version for p00hd",kFALSE},
  {"DbV0624"     ,""  ,"","db,calib,ry1h"                ,"","","20000624/0 Db Version for p00hd_1",kFALSE},
  {"DbV0713"     ,""  ,"","db,calib,ry1h"                  ,"","","20000713/0 Db Version for p00he",kFALSE},
  {"DbV0727"     ,""  ,"","db,calib,ry1h"                  ,"","","20000727/0 Db Version for p00he",kFALSE},
  {"DbV0819"     ,""  ,"","db,calib,ry1h"                  ,"","","20000819/0 Db Version for p00hg",kFALSE},
  {"DbV1123"     ,""  ,"","db,calib,ry1h"  ,"","","20001123/0 Db wo TpcDriftVel. from StTpcT0Maker",kFALSE},
  {"DbV0523"     ,""  ,"","db,calib,ry1h"  ,"","",                "20010523/0 Db Version for p01he",kFALSE},
  {"DbV1007"     ,""  ,"","db,calib,ry1h"  ,"","",                "20011007/0 Db Version for p01hi",kFALSE},
  {"DbV1107"     ,""  ,"","db,calib,ry1h"  ,"","",          "20011107/0 Db Version for pass1 p01gk",kFALSE},
  {"DbV1211"     ,""  ,"","db,calib,ry1h"  ,"","",           "20011211/0 Db Version for prod p01gl",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"magF"        ,"","","geant,StDbT,db","StMagFMaker","geometry,StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"l0"          ,"l0Chain","","trg_T,globT,ctf,trg"                        ,"StMaker","StChain","",kFALSE},
  {"ctf"         ,"ctf","l0Chain","ctf_T,db"               ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE},
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE},
  {"trg"         ,"trg","l0Chain","trg_T,globT,db"         ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"ppMCTrig"    ,"ppMC_trig1","l0Chain",""
                         ,"StppTrigMaker","StppSpin","Add emulation of pp Trigger based on CTB+MWC",kFALSE},

  {"tpc"         ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StMaker","StChain","",kFALSE},
  {"Trs"         ,"","tpcChain","scl,tpcDB,tpc_daq,Simu"              ,"StTrsMaker","StTrsMaker","",kFALSE},

  {"Mixer"       ,"tpc_raw","","","StMixerMaker"  ,"StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpcChain","detDb,tpc_T"        ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,"","tpcChain","Simu"                             ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"ZDCVtx"      ,"","tpcChain","db"                      ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tpc_T,tls"        ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"daqclf"      ,"","tpcChain","","StDaqClfMaker","StDaqClfMaker",    "Offline DAQ Cluster finder",kFALSE},


  {"Velo"        ,"","tpcChain","tpc_T,tls"                         ,"StVeloMaker","StVeloMaker","",kFALSE},
  {"TpcHitFilter","tpc_hit_filter","tpcChain",""    ,"StTpcHitFilterMaker","StTpcHitFilterMaker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpcChain","tpc_T,tls,"     ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"TpcT0"       ,"TpcT0","","tpc_T,svt_T,ctf_T,ftpcT,globT,tls,db,tpcDB,tpc_daq,kalman","StTpcT0Maker",
              "St_tpc,St_tcl_Maker,St_tpt_Maker,St_svt,St_global,St_dst_Maker,StPass0CalibMaker","",kFALSE},
  {"ChargeStep","","","tpc_T,globT,tls,db,tpcDB,tpc_daq","StChargeStepMaker","StChargeStepMaker","",kFALSE},
  {"laser"       ,"tpc_tracks","LaserTest,tpcChain","tdaq,tpc,-tpt,-PreVtx"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},
  {"PreVtx"      ,"","tpcChain","tpt,SCL,sim_T,tpc_T,svt_T,ftpcT,globT,ctf_T",
                                       "StPreVertexMaker","St_tpc,St_svt,St_global,St_dst_Maker","",kFALSE},


  {"svt"         ,"svtChain","","svt_T,SvtCL,Est,SvtVtx"                    ,"StMaker","StChain","",kFALSE},
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"SvtSlowSim","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                                  ,"StSvtSimulationMaker","StSvtSimulationMaker","",kFALSE},
  {"ssd"         ,"","","sls,spa,scf,scm,sce"                                             ,"","","",kFALSE},
  {"srs"         ,"svt_hits","svtChain","svtDb,tls,Simu,SvtCL,-sss,-SvtSlowSim,StEvent"
                                  ,"St_srs_Maker","St_tpc,St_svt,StSvtClusterMaker,St_srs_Maker","",kFALSE},
  {"sls"     ,"","svtChain","tls,Simu,SvtCL","St_sls_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"spa"     ,"","svtChain","tls,Simu,SvtCL","St_spa_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain","SvtCL"                  ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"      ,"SvtClu","svtChain","StEvent,SvtCL"   ,"StSvtClusterMaker","StSvtClusterMaker","",kFALSE},
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},
  {"SvtVtx"      ,"SvtVtx","SvtChain",""           ,"StSvtVertexFinderMaker","StSvtClusterMaker","",kFALSE},
  {"stk"        ,"svt_tracks","svtChain","tls,SvtCL","St_stk_Maker","St_tpc,St_svt,St_stk_Maker","",kFALSE},
  {"scf"      ,"","svtChain",""                ,"St_scf_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"scm"      ,"","svtChain",""                ,"St_scm_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"sce"      ,"","svtChain",""                   ,"St_sce_Maker","St_tpc,St_svt,StSsdEvalMaker","",kFALSE},
  {"Est"      ,"","svtChain","globT"                 ,"StEstMaker","St_global,St_svt,StEstMaker","",kFALSE},


  {"Ftpc"        ,"ftpcChain"  ,"","ftpcT,fcl,fpt,Fglobal,Fprimary"         ,"StMaker","StChain","",kFALSE},
  {"fss"    ,"ftpc_raw","ftpcChain","SCL,Simu", 
                                    "StFtpcSlowSimMaker","StFtpcSlowSimMaker","FTPC Slow simulator",kFALSE},
  {"Fcl"    ,"ftpc_hits","ftpcChain","SCL"
               ,"StFtpcClusterMaker","StDaqLib,StDAQMaker,StFtpcClusterMaker","FTPC cluster finder",kFALSE},


  {"emcY2"    ,"emcY2","","geant,emc_T,tpc_T,db,calib,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim" ,"emcRaw","emcY2","geant,emc_T,EmcUtil","StEmcSimulatorMaker","StMcEvent,StEmcSimulatorMaker",
                                                                           "New simulator for BEMC",kFALSE},
  {"emcDY2","",""       ,"db,StEvent,EmcUtil,PreEcl,Epc","StEmcADCtoEMaker","StEmcADCtoEMaker",
                                                                                    "EMC raw chain",kFALSE},


  {"global"      ,"globalChain","","globT,Match,vertex,primary,v0,xi,kink,dst,SCL,dEdx"
                                                              ,"StMaker","St_tpc,St_svt,StChain","",kFALSE},
  {"Match"       ,"match","globalChain","SCL,tpc_T,svt_T,globT,tls"
                                                 ,"StMatchMaker","St_svt,St_global,St_dst_Maker","",kFALSE},

  {"point"      ,"point","globalChain","SCL,tables,tls","StPointlMaker","St_global,St_dst_Maker","",kFALSE},
  {"Vertex"     ,"Vertex","globalChain","SCL,tls"
                           ,"StVertexMaker","St_svt,St_global,St_dst_Maker","Primary Vertex finder",kFALSE},
  {"Primary"    ,"primary","globalChain","SCL,globT,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"V0"         ,"v0","globalChain","SCL,globT,tls","StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"         ,"xi","globalChain","SCL,globT,tls","StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"   ,"kink","globalChain","SCL,globT,tls","StKinkMaker" ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"fpt"      ,"ftpc_tracks","globalChain","SCL"          
                                          ,"StFtpcTrackMaker","StFtpcTrackMaker","FTPC Track Maker",kFALSE},
  {"Fglobal"    ,"fglobal","globalChain","SCL,tables,tls"
                 ,"StFtpcGlobalMaker","St_global,St_dst_Maker","FTPC track refit, fills dst_tracks",kFALSE},
  {"Fprimary"    ,"fprimary","globalChain","SCL,tables,tls"
                  ,"StFtpcPrimaryMaker","St_global,St_dst_Maker","FTPC track refit + dca selection",kFALSE},

  {"dst"         ,"dst","globalChain","dstOut,SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT,svt_T"
                                                 ,"St_dst_Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"FindVtxSeed" ,"FindVtxSeed","","","StVertexSeedMaker","St_global,St_dst_Maker,StPass0CalibMaker",
                                                                     "Performs vertex seed finding",kFALSE},
  {"dEdx"        ,"dEdx","globalChain","globT,tpcDb,TbUtil",         "StdEdxMaker","StdEdxMaker","",kFALSE},
  {"svtdEdx"     ,"svtdEdx","globalChain","globT",                "StSvtdEdxMaker","StdEdxMaker","",kFALSE},
  {"Event"       ,"","","StEvent,tpcDB"         ,"StEventMaker","StDetectorDbMaker,StEventMaker","",kFALSE},
  {"PostEmc"     ,"PostChain","","geant,emc_T,tpc_T,db,calib,PreEcl,EmcUtil","StMaker","StChain","",kFALSE},
  {"PreEcl"      ,"preecl","PostChain",""                 ,"StPreEclMaker",      "StPreEclMaker","",kFALSE},
                          
  {"Epc"         ,"epc","PostChain","PreEcl,Match,EmcUtil"            ,"StEpcMaker","StEpcMaker","",kFALSE},

  {"fpd"         ,"fpd","","",                  "StFpdMaker","StFpdMaker","FPD/BBC Data base chain",kFALSE},

  {"rich"        ,"RichChain","","rch,RichPiD,RichSpectra",        "StMaker","StChain","RICH chain",kFALSE},
  {"Rrs"         ,"","RichChain","sim_T,Simu"                         ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rch"         ,"","RichChain","sim_T,globT"             ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"RichPiD"     ,"","RichChain","Event"                      ,"StRichPIDMaker","StRichPIDMaker","",kFALSE},

  {"tofDat"      ,"tof_raw","","db,Tofutil","StTofMaker","StEvent,StTofMaker","TOF Data base chain",kFALSE},

  {"l3"          ,"l3Chain","","l3cl,l3t"                                   ,"StMaker","StChain","",kFALSE},
  {"l3cl"        ,"","l3Chain","l3_T"               ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3Chain","l3_T"                       ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"l3onl"       ,"","",""                            ,"Stl3RawReaderMaker","Stl3RawReaderMaker","",kFALSE},
  {"l3count"     ,"","",""                              ,"Stl3CounterMaker","Stl3RawReaderMaker","",kFALSE},

  {"analysis"    ,"","","Event"          ,"StAnalysisMaker","StAnalysisMaker","Example of Analysis",kFALSE},
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra" ,"","",""                            ,"StRichSpectraMaker","StRichSpectraMaker","",kFALSE},

  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
  {"Flow"        ,"","TagsChain","StEvent"         ,"StFlowMaker","StEventUtilities,StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","TagsChain","StEvent,Flow"               ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
  {"FlowAnalysis","","TagsChain","StEvent,Flow"     ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","StEvent"            ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"SpectraTag"  ,"","TagsChain","StEvent"              ,"StSpectraTagMaker","StSpectraTagMaker","",kFALSE},
  {"EbyeScaTags" ,"","TagsChain","StEvent"            ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
  {"PCollTag"    ,"","TagsChain","StEvent"                  ,"StPCollTagMaker","StPCollTagMaker","",kFALSE},
  {"tags"        ,"","TagsChain",
                 "TagsChain,globT,Event,FlowTag,StrangeTags,SpectraTag,EbyeScaTags,TpcTag,PCollTag"
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},


  {"MuDSTChain","MuDSTChain","EMCmDST,CMuDST",""                            ,"StMaker","StChain","",kFALSE},
  {"MuDSTDeps","","","","",          "StEvent,StStrangeMuDstMaker","MuDST misc. dependencies (all)",kFALSE},
  {"StrngMuDST","","","",                                       "StStrangeMuDstMaker","StStrangeMuDstMaker",
                                                                            "Load Stangeness MuDST",kFALSE},

  {"EMCmDST"  ,"","MuDSTChain","MuDSTDeps,EmcUtil",                     "StEmcMicroDstMaker","StMuDSTMaker",
                                                                                 "Writes EMC MuDST",kFALSE},
  {"CMuDST"   ,"","MuDSTChain","MuDSTDeps,StrngMuDST",                        "StMuDstMaker","StMuDSTMaker",
                                                                              "Writes Common MuDST",kFALSE},



  {"QA"          ,"QA","","globT,SCL,global"                        ,"St_QA_Maker","St_QA_Maker","",kFALSE},
  {"EventQA"     ,"EventQA","","Event"                           ,"StEventQAMaker","St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"","","SCL,St_geom",
                                    "StEventDisplayMaker","StEventDisplayMaker,StTableUtilities","",kFALSE},
  {"Mc"          ,"McChain","","sim_T,globT,McAss,McAna"                    ,"StMaker","StChain","",kFALSE},
  {"McEvent"     ,"","McChain","Event,EmcUtil",      "StMcEventMaker","StMcEvent,StMcEventMaker","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAna"       ,"","McChain","McEvent",                "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"LAna"        ,"","","in,RY1h,geant,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  {"SpinTag" ,"SpinTag","","","StSpinTagMaker","StppSpin","tag for analysis of polarized pp events",kFALSE},
  {"ppLPfind1"   ,"ppLPfind1"  ,"",""  ,"StppLPfindMaker","StppSpin","Find leading particle for pp",kFALSE},
  {"SpinSortA"   ,"SpinSortA"  ,"",""               ,"StSpinSortMaker","StppSpin","Spin sort event",kFALSE},
  {"ppLPprojectA","ppLPprojectA","",""
                      ,"StppLPprojectMaker","StppSpin","project LP to the spin dependent phi-histo",kFALSE},
  {"ppLPeval1"   ,"ppLPeval1"  ,"",""  ,"StppLPevalMaker","StppSpin","Evaluation of LP algo for pp",kFALSE},
  {"ppDAQfilter1","ppDAQfilter1"  ,"",""  ,"StDAQfilterMaker","StppSpin","DAQ filter (used for pp)",kFALSE},
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE}
};




// ITTF Chain will be put here. Option list starting from minimalistic requirements
// and may not initially work.
Bfc_st BFC2[] = {
  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Y1h"   ,"","","db,calib,detDb",
                                 "","","YEAR_1H fantastic y1:TPC+CTB+FTPC+RICH+caloPatch+svtLadder",kFALSE},
  {"Y2000" ,"","","db,calib,detDb"          ,"","","actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"RY1h"  ,"","","db,calib,detDb,VtxOffSet"                ,"","","Real data with Year1h geometry",kFALSE},
  {"RY2000","","","db,calib,detDb,VtxOffSet","","","actual 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2000a","","","db,calib,detDb"      ,"","","alternate 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2001","","","db,calib,detDb"          ,"","","actual 2001: Real data with Year2001 geometry ",kFALSE},
  {"Y2b"   ,"","","db,calib,detDb" ,"","","2001 geometry 1st guess:TPC+CTB+FTPC+RICH+CaloPatch+SVT",kFALSE},
  {"Y2001" ,"","","db,calib,detDb","","","year2001: geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Y2001n","","","db,calib,detDb","","",
                                     "year2001: new geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Complete","","","db,calib,detDb"      ,"","","complete: new (currently foreseen) complete STAR",kFALSE},
  {"NoDb"  ,""  ,"","HalfField"                                     ,"","","Take out Db from Chain",kFALSE},
  {"NoHits",""  ,"",""                                  ,"","","Don't write hits into Event.Branch",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Physics"     ,"","","trg"                                        ,"","","Select Physics events",kFALSE},
  {"LaserTest"   ,"","","trg"                                          ,"","","Select Laser events",kFALSE},
  {"PulserSvt"   ,"","","trg"                                     ,"","","Select SVT Pulser events",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"doEvents"    ,""  ,"","xin,event,analysis,NoDb"                                       ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","global,dst,qa,event,analysis,EventQA"                          ,"","","",kFALSE},
  {"C1default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits"    ,"","","Year 1 chain",kFALSE},
  {"C2default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,ftpc,svt,emcY2"
                                                                              ,"","","Year 2 chain",kFALSE},
  {"CAdefault"   ,""  ,"","tpc,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits,ftpc,svt,emcY2"
                                                                         ,"","","Assymptotic chain",kFALSE},
  {"C2000"       ,""  ,"","y2000,C1default"                            ,"","","Turn on chain Y2000",kFALSE},
  {"C2001"       ,""  ,"","y2001,C2default"                            ,"","","Turn on chain Y2001",kFALSE},
  {"MDC4"        ,""  ,"","C2001,trs,srs,fss,rrs,big,GeantOut"      ,"","","Turn on chain for MDC4",kFALSE},
  {"MDC4New"     ,""  ,"","y2001n,C2default,trs,srs,fss,rrs,big,GeantOut","","",
                                                     "Turn on chain for MDC4 (for after September)",kFALSE},
  {"PostMDC4"    ,""  ,"","C2001,trs,sss,fss,rrs,big,GeantOut"     ,"","","Turn on Post MDC4 chain",kFALSE},
  {"ppMDC4"      ,""  ,"","pp,C2001,-PreVtx,ppMCTrig,mwc,ppLPeval1,trs,srs,rrs,big,GeantOut",
                                                                    "","","Turn on chain for ppMDC",kFALSE},
  {"CComplete"   ,""  ,"","Complete,C2default"             ,"","","Turn on chain for Complete STAR",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  // Detector combined-chains
  {"SvtD"        ,""  ,"","SvtDb,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit,SvtVtx", "", "",
                                                                               "SVT chain for Data",kFALSE},

  // Year 1 chains
  {"P2000"       ,""  ,"","ry2000,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
                                                           ,"Production chain for summer 2000 data",kFALSE},

  {"B2000"       ,""  ,"","ry2000a,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2000a"      ,""  ,"",
   "B2000,AlignSectors,ExB,OBmap,OClock,OPr13","",""       ,"Production chain for summer 2000 data",kFALSE},


  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  // Year 2 chains. 
  // B2001 is a base-chain for 2001 (with tpc+rhic). 
  {"B2001"       ,""  ,"","ry2001,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2001"       ,""  ,"",
   "B2001,l3onl,tofDat,AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC,OSpaceZ",
   "",""                                       ,"Production chain for summer 2001 data (+ l3, tof)",kFALSE},

  {"P2001a"      ,""  ,"",
   "B2001,svt_daq,SvtD,ftpc,l3onl,tofDat,emcDY2,AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC,OSpaceZ",
   "",""                       ,"Production chain for summer 2001 data (+ ftpc, svt, l3, tof, emc)",kFALSE},

  // pp Chains 
  {"pp2001"      ,""  ,"",
   "pp,B2001,-PreVtx,-SpinTag,l3onl,tofDat,emcDY2,AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC",
   "",""                                                                ,"pp 2001 (+ l3, tof, emc)",kFALSE},
  {"pp2001a"      ,""  ,"",
   "pp,B2001,-PreVtx,-SpinTag,SvtD,ftpc,l3onl,tofDat,emcDY2,AlignSectors,ExB,OBmap,OClock,OPr13,OTwist,OIFC",
   "",""                                                     ,"pp 2001 (+ ftpc, svt, l3, tof, emc)",kFALSE},


  // Other chains/Calibration
  {"LaserCal",""  ,"","db,detDb,tpc_daq,tpcDb,tcl,globT,laser,LaserTest","","", 
                                                                          "Laser Calibration Chain",kFALSE},
  {"L3Counter","" ,"","db,detDb,xin,l3count","","",                    "L3 Counter extraction pass",kFALSE},
  {"VtxSeedCal","","",
   "pp2001 -l3onl -rich -rch -RichPiD -tofDat -v0 -xi -kink FindVtxSeed NoEvent -tree -tags -QA -EventQA",
                                                                     "","","Pass0 Vertex evaluator",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"OPTIONS     ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"ITTF"        ,""  ,"","","",""                         ,"Turn on ITTF chain options evaluation",kFALSE},
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
  {"FieldOn"     ,""  ,"","MagF"                                   ,"","" ,"Constant nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","MagF"                                          ,"","" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","MagF"                                         ,"","","Half Field option",kFALSE},
  {"ReverseField",""  ,"","MagF"                                      ,"","","Reverse Field option",kFALSE},
  {"NoCintDb"    ,""  ,"",""                                   ,"","","Switch off standard Cint Db",kFALSE},
  {"NoCintCalDb" ,""  ,"",""                                      ,"","","Switch off Cint Calib Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug1"      ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
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
  {"EB1"         ,""  ,"","",""                                     ,"","Force ExB configuration 1",kFALSE},
  {"EB2"         ,""  ,"","",""                                     ,"","Force ExB configuration 2",kFALSE},
  {"OBmap"       ,""  ,"","",""                                          ,"","ExB shape correction",kFALSE},
  {"OTwist"      ,""  ,"","",""                                          ,"","ExB twist correction",kFALSE},
  {"OClock"      ,""  ,"","",""                                     ,"","Clock/tpc rot. correction",kFALSE},
  {"OPr13"       ,""  ,"","",""                                          ,"","PadRow 13 distortion",kFALSE},
  {"OCentm"      ,""  ,"","",""                                   ,"","Central membrane correction",kFALSE},
  {"OECap"       ,""  ,"","",""                                    ,"","EndCap (curved) correction",kFALSE},
  {"OIFC"        ,""  ,"","",""                                         ,"","Field Cage correction",kFALSE},
  {"OSpaceZ"     ,""  ,"","",""                                      ,"","Space Charge corrections",kFALSE},
  {"AlignSectors",""  ,"","",""          ,"","Activate Sector Alignment correction in St_tpt_Maker",kFALSE},
  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},
  {"pp"      ,""  ,"","SpinTag,ppLPfind1,SpinSortA,ppLPprojectA","","","Use pp specific parameters",kFALSE},
  {"VtxOffSet"   ,""  ,"","",""                 ,"","Account Primary Vertex offset from y2000 data",kFALSE},
  {"Calibration" ,""  ,"","",""                                              ,"","Calibration mode",kFALSE},
  {"beamLine"    ,""  ,"","",""                                       ,"","LMV Beam line constrain",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
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

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"vpd"         ,""  ,"","vpd_T",""                                                   ,"St_vpd","",kFALSE},
  {"tls"         ,""  ,"","",""                                                           ,"tls","",kFALSE},
  {"daq"         ,""  ,"","",""                            ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                             ,"StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"StEvent"     ,""  ,"","globT,SCL",""                                  ,"StEvent","Load StEvent",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T",""                                  ,"StEmcUtil","Load StEmcUtil",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"in"          ,""  ,"","xin"                                               ,"","","Alias to xin",kFALSE},
  {"xin"         ,""  ,"",""              ,"StIOMaker","StIOMaker","Read [XDF|DAQ|ROOT] input file",kFALSE},
  {"xdf2root"    ,""  ,"",""                                   ,"","xdf2root","Read XDF input file",kFALSE},
  {"geant"       ,"geant","","geomT,gen_T,sim_T"
                                  ,"St_geant_Maker","geometry,St_g2t,StMagF,St_geant_Maker","GEANT",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"db"          ,"db"   ,"","StDbT,xdf2root"    ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"svtDb"       ,"svtDb","","SvtCL,db" ,                          "StSvtDbMaker","StSvtDbMaker","",kFALSE},
  {"dbutil"      ,""     ,"","SCL"            ,"","StSvtDbMaker,StDbUtilities","Load StDbUtilities",kFALSE},
  {"calib"       ,"calib","","xdf2root"          ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"detDb"       ,""     ,"",""   ,"StDetectorDbMaker","StDetectorDbMaker","Load StDetectorDbMaker",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Valid Db    ","Versions   ","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"DbV"         ,""  ,"","db,calib,ry1h"                   ,"","","19940614/0 Db Version for none",kFALSE},

  // REMINDER :: THIS IS THE ITTF CHAIN OPTIONS
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"magF"        ,"","","geant,StDbT,db","StMagFMaker","geometry,StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"l0"          ,"l0Chain","","trg_T,globT,ctf,trg"                        ,"StMaker","StChain","",kFALSE},
  {"ctf"         ,"ctf","l0Chain","ctf_T,db"               ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE},
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE},
  {"trg"         ,"trg","l0Chain","trg_T,globT,db"         ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"ppMCTrig"    ,"ppMC_trig1","l0Chain",""
                         ,"StppTrigMaker","StppSpin","Add emulation of pp Trigger based on CTB+MWC",kFALSE},

  {"tpc"         ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StMaker","StChain","",kFALSE},
  {"Trs"         ,"","tpcChain","scl,tpcDB,tpc_daq,Simu"              ,"StTrsMaker","StTrsMaker","",kFALSE},

  {"Mixer"       ,"tpc_raw","","","StMixerMaker"  ,"StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpcChain","detDb,tpc_T"        ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,"","tpcChain","Simu"                             ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"ZDCVtx"      ,"","tpcChain","db"                      ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tpc_T,tls"        ,"St_tcl_Maker","St_tpc,St_tcl_Maker","",kFALSE},
  {"daqclf"      ,"","tpcChain","","StDaqClfMaker","StDaqClfMaker",    "Offline DAQ Cluster finder",kFALSE},


  {"Velo"        ,"","tpcChain","tpc_T,tls"                         ,"StVeloMaker","StVeloMaker","",kFALSE},
  {"TpcHitFilter","tpc_hit_filter","tpcChain",""    ,"StTpcHitFilterMaker","StTpcHitFilterMaker","",kFALSE},
  {"tpt"         ,"tpc_tracks","tpcChain","tpc_T,tls,"     ,"St_tpt_Maker","St_tpc,St_tpt_Maker","",kFALSE},
  {"TpcT0"       ,"TpcT0","","tpc_T,svt_T,ctf_T,ftpcT,globT,tls,db,tpcDB,tpc_daq,kalman","StTpcT0Maker",
              "St_tpc,St_tcl_Maker,St_tpt_Maker,St_svt,St_global,St_dst_Maker,StPass0CalibMaker","",kFALSE},
  {"ChargeStep","","","tpc_T,globT,tls,db,tpcDB,tpc_daq","StChargeStepMaker","StChargeStepMaker","",kFALSE},
  {"laser"       ,"tpc_tracks","LaserTest,tpcChain","tdaq,tpc,-tpt,-PreVtx"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},
  {"PreVtx"      ,"","tpcChain","tpt,SCL,sim_T,tpc_T,svt_T,ftpcT,globT,ctf_T",
                                       "StPreVertexMaker","St_tpc,St_svt,St_global,St_dst_Maker","",kFALSE},


  {"svt"         ,"svtChain","","svt_T,SvtCL,Est,SvtVtx"                    ,"StMaker","StChain","",kFALSE},
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"SvtSlowSim","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                                  ,"StSvtSimulationMaker","StSvtSimulationMaker","",kFALSE},
  {"ssd"         ,"","","sls,spa,scf,scm,sce"                                             ,"","","",kFALSE},
  {"srs"         ,"svt_hits","svtChain","svtDb,tls,Simu,SvtCL,-sss,-SvtSlowSim,StEvent"
                                  ,"St_srs_Maker","St_tpc,St_svt,StSvtClusterMaker,St_srs_Maker","",kFALSE},
  {"sls"     ,"","svtChain","tls,Simu,SvtCL","St_sls_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"spa"     ,"","svtChain","tls,Simu,SvtCL","St_spa_Maker","St_tpc,St_svt,StSsdSimulationMaker","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain","SvtCL"                  ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"      ,"SvtClu","svtChain","StEvent,SvtCL"   ,"StSvtClusterMaker","StSvtClusterMaker","",kFALSE},
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},
  {"SvtVtx"      ,"SvtVtx","SvtChain",""           ,"StSvtVertexFinderMaker","StSvtClusterMaker","",kFALSE},
  {"stk"        ,"svt_tracks","svtChain","tls,SvtCL","St_stk_Maker","St_tpc,St_svt,St_stk_Maker","",kFALSE},
  {"scf"      ,"","svtChain",""                ,"St_scf_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"scm"      ,"","svtChain",""                ,"St_scm_Maker","St_tpc,St_svt,StSsdClusterMaker","",kFALSE},
  {"sce"      ,"","svtChain",""                   ,"St_sce_Maker","St_tpc,St_svt,StSsdEvalMaker","",kFALSE},
  {"Est"      ,"","svtChain","globT"                 ,"StEstMaker","St_global,St_svt,StEstMaker","",kFALSE},


  {"Ftpc"        ,"ftpcChain"  ,"","ftpcT,fcl,fpt,Fglobal,Fprimary"         ,"StMaker","StChain","",kFALSE},
  {"fss"    ,"ftpc_raw","ftpcChain","SCL,Simu", 
                                    "StFtpcSlowSimMaker","StFtpcSlowSimMaker","FTPC Slow simulator",kFALSE},
  {"Fcl"    ,"ftpc_hits","ftpcChain","SCL"
               ,"StFtpcClusterMaker","StDaqLib,StDAQMaker,StFtpcClusterMaker","FTPC cluster finder",kFALSE},


  {"emcY2"    ,"emcY2","","geant,emc_T,tpc_T,db,calib,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim" ,"emcRaw","emcY2","geant,emc_T,EmcUtil","StEmcSimulatorMaker","StMcEvent,StEmcSimulatorMaker",
                                                                           "New simulator for BEMC",kFALSE},
  {"emcDY2","",""       ,"db,StEvent,EmcUtil,PreEcl,Epc","StEmcADCtoEMaker","StEmcADCtoEMaker",
                                                                                    "EMC raw chain",kFALSE},


  {"global"      ,"globalChain","","globT,Match,vertex,primary,v0,xi,kink,dst,SCL,dEdx"
                                                              ,"StMaker","St_tpc,St_svt,StChain","",kFALSE},
  {"Match"       ,"match","globalChain","SCL,tpc_T,svt_T,globT,tls"
                                                 ,"StMatchMaker","St_svt,St_global,St_dst_Maker","",kFALSE},

  {"point"      ,"point","globalChain","SCL,tables,tls","StPointlMaker","St_global,St_dst_Maker","",kFALSE},
  {"Vertex"     ,"Vertex","globalChain","SCL,tls"
                           ,"StVertexMaker","St_svt,St_global,St_dst_Maker","Primary Vertex finder",kFALSE},
  {"Primary"    ,"primary","globalChain","SCL,globT,tls"
                                               ,"StPrimaryMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"V0"         ,"v0","globalChain","SCL,globT,tls","StV0Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Xi"         ,"xi","globalChain","SCL,globT,tls","StXiMaker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"Kink"   ,"kink","globalChain","SCL,globT,tls","StKinkMaker" ,"St_svt,St_global,St_dst_Maker","",kFALSE},
  {"fpt"      ,"ftpc_tracks","globalChain","SCL"          
                                          ,"StFtpcTrackMaker","StFtpcTrackMaker","FTPC Track Maker",kFALSE},
  {"Fglobal"    ,"fglobal","globalChain","SCL,tables,tls"
                 ,"StFtpcGlobalMaker","St_global,St_dst_Maker","FTPC track refit, fills dst_tracks",kFALSE},
  {"Fprimary"    ,"fprimary","globalChain","SCL,tables,tls"
                  ,"StFtpcPrimaryMaker","St_global,St_dst_Maker","FTPC track refit + dca selection",kFALSE},

  {"dst"         ,"dst","globalChain","dstOut,SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT,svt_T"
                                                 ,"St_dst_Maker","St_svt,St_global,St_dst_Maker","",kFALSE},
  {"FindVtxSeed" ,"FindVtxSeed","","","StVertexSeedMaker","St_global,St_dst_Maker,StPass0CalibMaker",
                                                                     "Performs vertex seed finding",kFALSE},
  {"dEdx"       ,"dEdx","globalChain","globT,tpcDb,TbUtil",          "StdEdxMaker","StdEdxMaker","",kFALSE},
  {"Event"       ,"","","StEvent,tpcDB"         ,"StEventMaker","StDetectorDbMaker,StEventMaker","",kFALSE},
  {"PostEmc"     ,"PostChain","","geant,emc_T,tpc_T,db,calib,PreEcl,EmcUtil","StMaker","StChain","",kFALSE},
  {"PreEcl"      ,"preecl","PostChain",""                 ,"StPreEclMaker",      "StPreEclMaker","",kFALSE},
                          
  {"Epc"         ,"epc","PostChain","PreEcl,Match,EmcUtil"            ,"StEpcMaker","StEpcMaker","",kFALSE},

  {"fpd"         ,"fpd","","",                  "StFpdMaker","StFpdMaker","FPD/BBC Data base chain",kFALSE},

  {"rich"        ,"RichChain","","rch,RichPiD,RichSpectra",        "StMaker","StChain","RICH chain",kFALSE},
  {"Rrs"         ,"","RichChain","sim_T,Simu"                         ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rch"         ,"","RichChain","sim_T,globT"             ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"RichPiD"     ,"","RichChain","Event"                      ,"StRichPIDMaker","StRichPIDMaker","",kFALSE},

  {"tofDat"      ,"tof_raw","","db,Tofutil","StTofMaker","StEvent,StTofMaker","TOF Data base chain",kFALSE},

  {"l3"          ,"l3Chain","","l3cl,l3t"                                   ,"StMaker","StChain","",kFALSE},
  {"l3cl"        ,"","l3Chain","l3_T"               ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3Chain","l3_T"                       ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"l3onl"       ,"","",""                            ,"Stl3RawReaderMaker","Stl3RawReaderMaker","",kFALSE},
  {"l3count"     ,"","",""                              ,"Stl3CounterMaker","Stl3RawReaderMaker","",kFALSE},

  {"analysis"    ,"","","Event"          ,"StAnalysisMaker","StAnalysisMaker","Example of Analysis",kFALSE},
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra" ,"","",""                            ,"StRichSpectraMaker","StRichSpectraMaker","",kFALSE},

  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
  {"Flow"        ,"","TagsChain","StEvent"         ,"StFlowMaker","StEventUtilities,StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","TagsChain","StEvent,Flow"               ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
  {"FlowAnalysis","","TagsChain","StEvent,Flow"     ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","StEvent"            ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"SpectraTag"  ,"","TagsChain","StEvent"              ,"StSpectraTagMaker","StSpectraTagMaker","",kFALSE},
  {"EbyeScaTags" ,"","TagsChain","StEvent"            ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
  {"PCollTag"    ,"","TagsChain","StEvent"                  ,"StPCollTagMaker","StPCollTagMaker","",kFALSE},
  {"tags"        ,"","TagsChain",
                 "TagsChain,globT,Event,FlowTag,StrangeTags,SpectraTag,EbyeScaTags,TpcTag,PCollTag"
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},

  // THIS IS THE ITTF PART OF THE OPTION
  // MicroDST options were removed (being shaped)


  {"QA"          ,"QA","","globT,SCL,global"                        ,"St_QA_Maker","St_QA_Maker","",kFALSE},
  {"EventQA"     ,"EventQA","","Event"                           ,"StEventQAMaker","St_QA_Maker","",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"","","SCL,St_geom",
                                    "StEventDisplayMaker","StEventDisplayMaker,StTableUtilities","",kFALSE},
  {"Mc"          ,"McChain","","sim_T,globT,McAss,McAna"                    ,"StMaker","StChain","",kFALSE},
  {"McEvent"     ,"","McChain","Event,EmcUtil",      "StMcEventMaker","StMcEvent,StMcEventMaker","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAna"       ,"","McChain","McEvent",                "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"LAna"        ,"","","in,RY1h,geant,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  {"SpinTag" ,"SpinTag","","","StSpinTagMaker","StppSpin","tag for analysis of polarized pp events",kFALSE},
  {"ppLPfind1"   ,"ppLPfind1"  ,"",""  ,"StppLPfindMaker","StppSpin","Find leading particle for pp",kFALSE},
  {"SpinSortA"   ,"SpinSortA"  ,"",""               ,"StSpinSortMaker","StppSpin","Spin sort event",kFALSE},
  {"ppLPprojectA","ppLPprojectA","",""
                      ,"StppLPprojectMaker","StppSpin","project LP to the spin dependent phi-histo",kFALSE},
  {"ppLPeval1"   ,"ppLPeval1"  ,"",""  ,"StppLPevalMaker","StppSpin","Evaluation of LP algo for pp",kFALSE},
  {"ppDAQfilter1","ppDAQfilter1"  ,"",""  ,"StDAQfilterMaker","StppSpin","DAQ filter (used for pp)",kFALSE},
  {"xout"        ,""  ,"",""                                 ,"","xdf2root","Write dst to XDF file",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE}
};



Int_t NoChainOptions;
Int_t NoChainOptions1 = sizeof (BFC1)/sizeof (Bfc_st);
Int_t NoChainOptions2 = sizeof (BFC2)/sizeof (Bfc_st);

class StEvent;
StEvent *Event;
class StIOMaker;
class St_geant_Maker; St_geant_Maker *geantMk = 0;
class St_db_Maker;
static St_db_Maker *dbMk    = 0;
static St_db_Maker *calibMk = 0;
class StTreeMaker;
ClassImp(StBFChain)

//_____________________________________________________________________________
/// Default Constructor
StBFChain::StBFChain(const char *name, const Bool_t UseOwnHeader):
  StChain(name,UseOwnHeader),fXdfOut(0),fTFile(0),fSetFiles(0),fInFile(0),fFileOut(0),fXdfFile(0) {

  (void) printf("StBFChain :: Default Constructor called.\n");
  fBFC = new Bfc_st[NoChainOptions1];
  memcpy (fBFC, &BFC1, sizeof (BFC1));
  NoChainOptions = NoChainOptions1;
  FDate  = FTime  = 0;
  FDateS = FTimeS = 0;
}

// Hack onstructor. 
/*!
 * This method can be called with mode 1 or 2 to enable chain setup 1 or chain
 * setup 2. Note that this constructor does not allow for changing the chain
 * name (set to be <tt>bfc</tt>) as the regular constructor does. So, embeding
 * is a no-no (until we extend this, actually easy with a 3 arguments default
 * constructor but would require embeding scripts update).
 *
 * This was primarily set to make possible the transition between the regular
 * chain and the ITTF chain options.
 */
StBFChain::StBFChain(Int_t mode):
  StChain("bfc",kFALSE),fXdfOut(0),fTFile(0),fSetFiles(0),fInFile(0),fFileOut(0),fXdfFile(0) {

  if(mode == 2){
    (void) printf("StBFChain :: Special Constructor called using chain-setup 2\n");
    fBFC = new Bfc_st[NoChainOptions2];
    memcpy (fBFC, &BFC2, sizeof (BFC2));
    FDate  = FTime  = 0;  
    FDateS = FTimeS = 0;  
    NoChainOptions= NoChainOptions2;
  } else {
    (void) printf("StBFChain :: Special Constructor called using chain-setup 1\n");
    fBFC = new Bfc_st[NoChainOptions1];
    memcpy (fBFC, &BFC1, sizeof (BFC1));
    FDate  = FTime  = 0;      
    FDateS = FTimeS = 0;      
    NoChainOptions= NoChainOptions1;
  }
}



//_____________________________________________________________________________
/// Destructor. Call Finish() . See this method for detail on what is cleaned.
StBFChain::~StBFChain(){
  Finish();
}
//_____________________________________________________________________________
/// Routine handling library loading depending on chain options
Int_t StBFChain::Load()
{
  Int_t status = kStOk;
  Int_t i, iok;
  for (i = 1; i< NoChainOptions; i++) { // Load Libraries if any
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
/// Maker-instantiation handler.
/*!
  This routine contains it all (make instantiation that is), from calibration 
  precedence to parameter setting depending on option etc ... Other thing done
  here which deserves attention
  - The maker's SetMode() mechanism is treated here.
  - Calibration options like NoMySQLDb or NoCintDb and path are set
  - SetFlavor() sim+ofl or sim is made

  If a maker is added along with some flag options, this is the place to
  implement the switches.
*/
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
	  if (GetOption("pp") ) {                           // pp specific stuff
	    if (maker == "StTrsMaker") mk->SetMode(1);      // Pile-up correction
	    if (maker == "StVertexMaker"){
	      mk->SetMode(1);                               // Switch vertex finder to ppLMV
	      if( GetOption("beamLine")){
		StVertexMaker *pMk = (StVertexMaker*) mk;
		pMk->SetBeam4ppLMV();                       // Add beam-line constrain
	      }
	    }
	  }
	  if (GetOption("CMuDST") && GetOption("StrngMuDST")) {
	    if (maker == "StStrangeMuDstMaker"){
	      StStrangeMuDstMaker *pMk = (StStrangeMuDstMaker*) mk;
	      pMk->DoV0();                                  // Set StrangeMuDstMaker parameters
	      pMk->DoXi();
	      pMk->DoKink();
	      pMk->SetNoKeep();                             // Set flag for output OFF
	    }
	  }
	  if ((maker == "StVertexMaker"  || maker == "StPreVertexMaker") &&
	      GetOption("VtxOffSet")) mk->SetMode(2);  

	  if (maker == "St_dst_Maker") SetInput("dst",".make/dst/.data/dst");
	  if (maker == "St_dst_Maker" && GetOption("HitsBranch")) mk->SetMode(2);
	  if (maker == "StMatchMaker" && !GetOption("Kalman")) mk->SetMode(-1);
	  if (maker == "StLaserEventMaker"){
	    // Bill stuff - Empty place-holder
	  }
	  if (maker == "St_tpt_Maker" && GetOption("ExB")){
	    // bit 0 is ExB ON or OFF
	    // The next 3 bits are reserved for yearly changes.
	    // Backward compatibility preserved.
	    int mask=1;                                    // Al Saulys request
	    if( GetOption("EB1") ){
	      // Do nothing (i.e. bit 1 at 0)
	    } else if ( GetOption("EB2") ){
	      // Force bit 1 at 1 regardless
	      mask = mask | 2;
	    } else {
	      // depend on RY option i.e. take default for that RealYear data
	      // expectations.
	      if( GetOption("RY2001") ) mask = mask | 2 ;  // Jim Thomas request
	    }
	    // Other options introduced in October 2001 for distortion corrections
	    // studies and year1 re-production. Those are OR additive to the mask.
	    if( GetOption("OBmap") ){	      mask |=   16; }
	    if( GetOption("OPr13") ){	      mask |=   32; }
	    if( GetOption("OTwist") ){	      mask |=   64; }
	    if( GetOption("OClock") ){	      mask |=  128; }
	    if( GetOption("OCentm") ){	      mask |=  256; }
	    if( GetOption("OECap") ){	      mask |=  512; }
	    if( GetOption("OIFC") ){	      mask |= 1024; }
	    if( GetOption("OSpaceZ") ){	      mask |= 2048; }

	    (void) printf("StBFChain: ExB The option passed will be %d 0x%X\n",mask,mask);
	    mk->SetMode(mask);
	  }
	  if (maker == "St_tpt_Maker" && GetOption("AlignSectors")){
	    St_tpt_Maker *tptMk = (St_tpt_Maker*)mk;
            tptMk->AlignHits(kTRUE);
	  }
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
	    if (GetOption("PulserSvt")) mode += 4;
	    if (mode) mk->SetMode(mode);
	  }
	  if (maker == "StdEdxMaker" &&GetOption("Simu"))  mk->SetMode(-10);
	  if (maker == "StTpcDbMaker"){  
            mk->SetMode(0);
	    // this change may be temporary i.e. if Simulation includes
	    // rotation/translation, this won't be necessarily true.
	    // Will investigate further.
            if (GetOption("Simu")) mk->SetMode(1);
	    // This is commented for now but may be used. Those extensions
	    // were implemented by David H. on Jan 2 2002. DEfault is ofl+laserDV
	    //mk->UseOnlyLaserDriftVelocity();    // uses laserDV database
	    //mk->UseOnlyCathodeDriftVelocity();  // uses offl database
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
  if (GetOption("Debug"))  SetDEBUG(1);
  if (GetOption("Debug1")) SetDEBUG(1);
  if (GetOption("Debug2")) SetDEBUG(2);
  return status;
}


//_____________________________________________________________________
/// Really the destructor (close files, delete pointers etc ...)
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
/// Check option if defined (Char_t argument interface)
Int_t StBFChain::kOpt (const Char_t *tag) const {
  TString *Tag = new TString(tag);
  Int_t kO = kOpt(Tag);
  delete Tag;
  return kO;
}

/// Check option if defined.
/*!
  This method checks if the options are valid by comparing them
  to the list of declared options. This is called for each option
  passed as argument. The real sorting of all options is done in
  SetFlags().
 */
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
  (void) printf (" Option %s has not been recognized\n", Tag.Data());
  return 0;
}

//_____________________________________________________________________
/// Enable/disable valid command line options
void StBFChain::SetOption(const Int_t k) { 
  // set all off
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
  } else {
    if (k < 0 && fBFC[-k].Flag) {
      //      printf ("SetOption: %s %i",fBFC[-k].Key,k);
      fBFC[-k].Flag = kFALSE;
      printf (" Switch Off %s\n", fBFC[-k].Key);
    } else {
      // 0
      return;
    }
  }
}

//_____________________________________________________________________
/// Returns chain-option state (on/off)
Bool_t StBFChain::GetOption(const Int_t k) const
{
  return (k>0 && k <NoChainOptions) ? fBFC[k].Flag : kFALSE;
}

/// Returns the comment string associated to an option
/*!
 * Any option passed a bla=XX is reshaped as follow ...
 * - The SetFlags() function strip out the =XX part and replaces
 *   the comment by the value XX
 * - This GetOptionString() returns the comment part so makers
 *   can globally access the option string.
 * 
 * <i>Note</i> : If the requested option is not part of the global BFC[]
 * array, the kOpt() method is going to scream at you but it will still
 * work. You can ask for that option to be added to the chain official
 * options later whenever your code debugging is done. In other words,
 * this method allows you to pass ANY options not officially declared
 * and use it as test/work-around to pass any parameters to your maker.
 *
 * However, if the parameters are to be used in production, we DO
 * request/require that they are declared as a valid option.
 *
 *
 */
Char_t *StBFChain::GetOptionString(const Char_t *Opt)
{
  int o = kOpt(Opt);
  if(!o) return NULL;
  else if(!GetOption(o)) return NULL;
  else return(fBFC[o].Comment);  
}


//_____________________________________________________________________________
/// Scan all flags, check if they are correct, manipulate the comment if necessary
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
      string.ToLower(); 
      // printf ("Chain %s\n",tChain.Data());
      kgo = kOpt(string.Data());
      if (kgo != 0){
	SetOption(kgo);
      } else {
	// it is 0 i.e. was not recognized. Check if it is a dbvXXXXXXXX
	// with a 8 digit long time stamp. We can do all of that in the
	// SetDbOptions() only (removing the fBFC[i].Flag check) but the
	// goal here is to avoid user's histeria by displaying extra 
	// messages NOW !!!
	if( ! strncmp( string.Data() ,"dbv",3) && strlen(string.Data()) == 11){
	  (void) sscanf(string.Data(),"dbv%d",&FDate);
	  cout << " ... but still will be considered as a dynamic timestamp (MaxEntryTime)" << FDate << endl;
	}
	if( ! strncmp( string.Data() ,"sdt",3) && strlen(string.Data()) == 11){
	  (void) sscanf(string.Data(),"sdt%d",&FDateS);
	  cout << " ... but still will be considered as a dynamic timestamp (DateTime))" << FDateS << endl;
	}
      }
    } else {
      // string with  "="
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
  if (!GetOption("NoDefault")) {
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
    if (!(GetOption("fzin") || GetOption("gstar"))) SetOption("magF");
    if (!GetOption("Eval") && GetOption("AllEvent"))  SetOption("Eval");
    if (!GetOption("event")) SetOption("-analysis");
    if (GetOption("NoDb")) {SetOption("-db"); SetOption("-calib");}
  }
  // Print set values
  St_Bfc *Bfc = new St_Bfc("BFChain",NoChainOptions);
  AddRunco(Bfc);
  for (k = 1; k<NoChainOptions;k++) {
    if (GetOption(k)) {
      (void) printf("QAInfo: ================== %4d %15s\tis ON \t: %s\n",
		    k, (char *) fBFC[k].Key, (char *) fBFC[k].Comment);
      //      gMessMgr->QAInfo() << "================== " << k << "\t"
      //		 << fBFC[k].Key << "\tis ON \t:" << fBFC[k].Comment << endm;
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
    if (gc == "gc:") {SetGC(infile+3); goto SetOut;}
  }
  SetInputFile(infile);
 SetOut:
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
  if (!GetOption("NoInput") && !GetOption("NoDefault")) {
    if (!fInFile && GetOption("fzin")) {
      fInFile = new TString("/star/rcf/simu/cocktail/hadronic/default/lowdensity/");
      if (GetOption("y1h")) fInFile->Append("year_1h/hadronic_on/Gstardata/rcf0078/hc_lowdensity.400_evts.fz");
      else
	if (GetOption("y2a")) fInFile->Append("year_2a/hadronic_on/Gstardata/rcf0079/hc_lowdensity.400_evts.fz");
	else {printf ("for fzin Option In file has not been defined. Exit!\n"); gSystem->Exit(1);}
      printf ("Use default input file %s for %s \n",fInFile->Data(),"fzin");
    }
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
  if (fInFile) gMessMgr->QAInfo() << "Input file name = " << fInFile->Data() << endm;
}


//_____________________________________________________________________
/// Takes care of output file name (extension)
void StBFChain::SetOutputFile (const Char_t *outfile){
  if (outfile)               fFileOut = new TString(outfile);
  else {
    if (GetOption("gstar"))  fFileOut = new TString("gtrack.root");
    else {
      if (fInFile) {
	if (GetOption("fzin")) {
	  TObjArray words;
	  ParseString(*fInFile,words);
	  TIter nextL(&words);
	  TObjString *word = 0;
	  while ((word = (TObjString *) nextL())) {
	    if (word->GetString().Contains(".fz")) {
	      fFileOut = new TString(gSystem->BaseName(word->GetString().Data()));
	      break;
	    }
	  }
	}
	else fFileOut = new TString(gSystem->BaseName(fInFile->Data()));
	if (  fFileOut) {
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
  }
  if (fFileOut)  gMessMgr->QAInfo() << "Output root file name " <<  fFileOut->Data() << endm;
  if (!fTFile) {
    if (GetOption("tags")  && fFileOut ||
	GetOption("lana") ||  GetOption("Laser")) {
      TString TagsName = TString(fFileOut->Data());
      if(GetOption("LaserCal")){
	TagsName.ReplaceAll(".root",".laser.root");
      } else {
	TagsName.ReplaceAll(".root",".tags.root");
      }
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
/// Handles all geant options
/*!
  This method sets the Geant options that is the Geometry loading
  part. Depends on St_geant_Maker instantiated in the Instantiate()
  method.
 */
void StBFChain::SetGeantOptions(){
  if (geantMk) {
    SetInput("geant",".make/geant/.data");
    if (!GetOption("fzin")) {
      if (GetOption("SD97") ||
	  GetOption("SD98") ||
	  GetOption("Y1a")  ||
	  GetOption("ES99") ||
	  GetOption("ER99") ||
	  GetOption("DC99"))          geantMk->LoadGeometry("detp geometry YEAR_1A");
      else {if (GetOption("Y1b"))     geantMk->LoadGeometry("detp geometry YEAR_1B");
      else {if (GetOption("Y1E"))     geantMk->LoadGeometry("detp geometry YEAR_1E");
      else {if (GetOption("Y1h") ||
		GetOption("RY1h"))    geantMk->LoadGeometry("detp geometry YEAR_1H");
      else {if (GetOption("Y1s"))     geantMk->LoadGeometry("detp geometry YEAR_1S");
      else {if (GetOption("Y2000")  ||
		GetOption("RY2000") ||
		GetOption("RY2000a")) geantMk->LoadGeometry("detp geometry year2000");
      else {if (GetOption("Y2a"))     geantMk->LoadGeometry("detp geometry YEAR_2A");
      else {if (GetOption("Y2001")  ||
		GetOption("Y2001n") ||
		GetOption("RY2001"))  geantMk->LoadGeometry("detp geometry year2001");
      else {if (GetOption("Y2b"))     geantMk->LoadGeometry("detp geometry YEAR_2b");
      //else {if (GetOption("Y2001"))   geantMk->LoadGeometry("detp geometry year2001");
      else {if (GetOption("Complete"))geantMk->LoadGeometry("detp geometry complete");
      else                            geantMk->LoadGeometry("detp geometry year2001");
      }}}}}}}}
    }

    if (GetOption("gstar")) {
	      geantMk->Do("subevent 0;");
	      // gkine #particles partid ptrange yrange phirange vertexrange
	      geantMk->Do("gkine 80 6 1. 1. -4. 4. 0 6.28  0. 0.;");
	      geantMk->Do("mode g2tm prin 1;");
	      //  geantMk->Do("next;");
	      //  geantMk->Do("dcut cave z 1 10 10 0.03 0.03;");
	      if (GetOption("Debug") ||GetOption("Debug2")) {
		geantMk->Do("debug on;");
		geantMk->Do("swit 2 3;");
	      }
	    }
    }
    else {
      if (fInFile) {
	TObjArray words;
	ParseString(*fInFile,words);
	TIter nextL(&words);
	TObjString *word = (TObjString *) nextL();
	if (word->GetString().Contains(".fz")) {
	  if (geantMk->SetInputFile(fInFile->Data()) > kStOK) {
	    printf ("File %s cannot be opened. Exit! \n",fInFile->Data());
	    gSystem->Exit(1);
	  }
	}
	else geantMk->Do(fInFile->Data());
      }
    }
  }
}

/// Treats the DbV options used for database timestamp.
/*!
  Re-scan all options and search for dbv options. This method also sorts
  out the string-based database timestamp for reconstruction. Those have
  to be in phase with the geant geometry (see SetGeantOptions()) if 
  simulation is being reconstructed.

  The order matters since a later option would overwrite an earlier one. 
  The mechanism introduced for a dynamic (i.e. not pre-defined) timestamp is that
  it will be used ONLY if there are no other timestamp options. 
  <b>Be aware of this precedence ...</b>
  
 */
void StBFChain::SetDbOptions(){
  Int_t i;
  Int_t Idate=0,Itime=0;

  for (i = 1; i < NoChainOptions; i++) {
    if (fBFC[i].Flag && !strncmp(fBFC[i].Key ,"DbV",3)){
      (void) printf("QAInfo: Found time-stamp %s [%s]\n",fBFC[i].Key,fBFC[i].Comment);
      (void) sscanf(fBFC[i].Comment,"%d/%d",&Idate,&Itime);
    }
  }

  if( ! Idate && FDate){
    (void) printf("QAInfo: Switching to user chosen dynamic time-stamp (MaxEntry) %d %d\n",FDate,FTime);
    (void) printf("QAInfo: Chain may crash if time-stamp is not validated by db interface\n");
    Idate = FDate;
    Itime = FTime;
  }

  StMakerIter nextMaker(this);
  StMaker *maker;
  while ((maker = nextMaker.NextMaker())) {
    if (!strcmp(maker->ClassName(),"St_db_Maker")) {
      St_db_Maker *db = (St_db_Maker *) maker;
        if (GetOption("SD97"))  db->SetDateTime("sd97");
  else {if (GetOption("SD98"))  db->SetDateTime("sd98");
  else {if (GetOption("Y1a"))   db->SetDateTime("year_1a");
  else {if (GetOption("Y1b"))   db->SetDateTime("year_1b");
  else {if (GetOption("Y1s"))   db->SetDateTime("year_1s");
  else {if (GetOption("ES99"))  db->SetDateTime("es99");
  else {if (GetOption("ER99"))  db->SetDateTime("er99");
  else {if (GetOption("DC99"))  db->SetDateTime("dc99");
  else {if (GetOption("Y1d"))   db->SetDateTime("year_1d");
  else {if (GetOption("Y1e"))   db->SetDateTime("year_1e");
  else {if (GetOption("Y1h"))   db->SetDateTime("year_1h");
  else {if (GetOption("Y2000")) db->SetDateTime("year_1h");
  else {if (GetOption("Y2a"))   db->SetDateTime("year_2a");
  else {if (GetOption("Y2b"))   db->SetDateTime("year_2b");
  else {if (GetOption("Y2001")) db->SetDateTime("year_2b");
  else {if (GetOption("Y2001n"))db->SetDateTime("year2001"); // Year_2b ** db ** timestamp does not reflect
                                                             // svt shift. Small hack to make it work.
  }}}}}}}}}}}}}}}

	// Startup date over-write
	if (FDateS){
	  (void) printf("QAInfo: Switching to user chosen dynamic time-stamp (Start) %d %d\n",FDateS,FTimeS);
	  (void) printf("QAInfo: Chain may crash if time-stamp is not validated by db interface\n");
	  db->SetDateTime(FDateS,FTimeS);
	}

	// Show date settings
	gMessMgr->QAInfo() << db->GetName()
			   << " Maker set time = "
			   << db->GetDateTime().GetDate() << "."
			   << db->GetDateTime().GetTime() << endm;

	// MaxEntry over-write
	if (Idate) {
	  db->SetMaxEntryTime(Idate,Itime);
	  cout << "\tSet DataBase max entry time " << Idate << "/" << Itime
	       << " for St_db_Maker(\"" << db->GetName() <<"\")" << endl;
	}
    }
  }
}
//_____________________________________________________________________
/// Creates output-tree branches
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
