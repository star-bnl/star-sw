#ifndef __BigFullChain_h__
#define __BigFullChain_h__
#define __KEEP_TPCDAQ_FCF__ /* remove St_tpcdaq_Maker and StRTSClientFCFMaker. not yet ready */
//#define __NoStrangeMuDst__
//#define __NoDisplay__
Bfc_st BFC[] = { // standard chains
  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  // geometry timestamps are now dynamic. Please see StChain/StMaker    
  {"RY2008","","","db,detDb,NosvtIT,NossdIT"                             ,"","","y2008 for dAu run",kFALSE},
  {"RY2009","","","db,detDb,NosvtIT,NossdIT"                             ,"","","y2009 for p+p run",kFALSE},
  {"ForceGeometry","","","","","",  "Force geometry to overwrite the geometry coming from fz-file", kFALSE},
  // geometry timestamps are now dynamic. Please see StChain/StMaker
  {"NoDb"  ,""  ,"","-db,-tpcDb,-magF"                              ,"","","Take out Db from Chain",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Valid Db    ","Versions   ","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"DbV"         ,""  ,"","db,ry1h"                         ,"","","19940614/0 Db Version for none",kFALSE},
  {"DbV0614"     ,""  ,"","db,ry1h"                        ,"","","20000614/0 Db Version for p00hd",kFALSE},
  {"DbV0624"     ,""  ,"","db,ry1h"                      ,"","","20000624/0 Db Version for p00hd_1",kFALSE},
  {"DbV0713"     ,""  ,"","db,ry1h"                        ,"","","20000713/0 Db Version for p00he",kFALSE},
  {"DbV0727"     ,""  ,"","db,ry1h"                        ,"","","20000727/0 Db Version for p00he",kFALSE},
  {"DbV0819"     ,""  ,"","db,ry1h"                        ,"","","20000819/0 Db Version for p00hg",kFALSE},
  {"DbV1123"     ,""  ,"","db,ry1h"        ,"","","20001123/0 Db wo TpcDriftVel. from StTpcT0Maker",kFALSE},
  {"DbV0523"     ,""  ,"","db,ry1h"        ,"","",                "20010523/0 Db Version for p01he",kFALSE},
  {"DbV1007"     ,""  ,"","db,ry1h"        ,"","",                "20011007/0 Db Version for p01hi",kFALSE},
  {"DbV1107"     ,""  ,"","db,ry1h"        ,"","",          "20011107/0 Db Version for pass1 p01gk",kFALSE},
  {"DbV1211"     ,""  ,"","db,ry1h"        ,"","",           "20011211/0 Db Version for prod p01gl",kFALSE},

  {"AgML"        ,""  ,"","-Agi,-VmcGeo","","StarAgmlLib,StarGeometry,Geometry"
   ,                                                             "alias VmcGeomtry to AgMLGeometry",kFALSE},
  {"Agi" ,""  ,"","-AgML,-VmcGeo","","","alias VmcGeomtry to AgiGeometry (gstar original geometry)",kFALSE},
  {"VmcGeo"      ,""  ,"","-AgML,-Agi"                    ,"",""      ,"alias VmcGeomtry to VmcGeo",kFALSE},
  
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Physics"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"LaserTest"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"PulserSvt"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"alltrigger"  ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"phys_off"    ,"","",""                                  ,"","","Turn off physics in simulation",kFALSE},
  {"hadr_off"    ,"","",""                    ,"","","Turn off hadronic interactions in simulation",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"doEvents"    ,""  ,"","in,StEvent,analysis,NoDb"                                      ,"","","",kFALSE},
  {"MakeMuDst","","","in,StEvent,CMuDST,Tree,nodefault,NoHistos,ReadAll","",""  ,"StEvent => MuDst",kFALSE},
  {"drawDst"     ,""  ,"","in,ry1h,globT,SCL,geant,display,NoDb,TbUtil"                   ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","ITTF,event,analysis,EventQA"                                   ,"","","",kFALSE},
  {"C1default"   ,""  ,"","rich,l0,Cdst,tags,Tree,EvOut,NoHits"               ,"","","Year 1 chain",kFALSE},
  {"C2default"   ,""  ,"","rich,l0,Cdst,tags,Tree,EvOut,ftpc,svt,emcY2"       ,"","","Year 2 chain",kFALSE},
  {"C3default"   ,""  ,"","l0,Cdst,tags,Tree,EvOut,NoHits,ftpc,svt,bbcsim,emcY2"
   ,                                                                 "","","Year 3 simu base chain",kFALSE},
  {"CAdefault"   ,""  ,"","l0,Cdst,tags,Tree,EvOut,NoHits,ftpc,svt,emcY2","","","Assymptotic chain",kFALSE},
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
  {"C2003"       ,""  ,"","y2003,C3default"                            ,"","","Turn on chain Y2003",kFALSE},
  {"C2003X"      ,""  ,"","y2003X,C3default"           ,"","","Turn on chain Y2003X (full B/E EMC)",kFALSE},
  // MDC / Sim chain agregates
  {"mdc3"        ,""  ,"","cy1h,GeantOut"                               ,"","","MDC3 default chain",kFALSE},
  {"MDC4" ,""  ,"","C2001,trs,tpc_daq,Simu,srs,fss,rrs,big,GeantOut","","","Turn on chain for MDC4",kFALSE},
  {"MDC4New"     ,""  ,"","y2001n,C2default,trs,tpc_daq,Simu,srs,fss,rrs,big,GeantOut","",""
   ,                                                 "Turn on chain for MDC4 (for after September)",kFALSE},
  {"PostMDC4"    ,""  ,"","C2001,trs,tpc_daq,Simu,sss,fss,rrs,big,GeantOut"     
   ,                                                                "","","Turn on Post MDC4 chain",kFALSE},
  {"ppMDC4","","","ppOpt,C2001,mwc,trs,tpc_daq,Simu,srs,rrs,big,GeantOut"
   ,                                                                "","","Turn on chain for ppMDC",kFALSE},
  {"dAuMDC"      ,"" ,"","ppOpt,C2003,trs,tpc_daq,Simu,srs,fss,big,GeantOut","","","Chain for d+Au",kFALSE},
  {"dAuMDCa" ,"" ,"","ppOpt,C2003,trs,tpc_daq,Simu,srs,fss,big,GeantOut,est","","","Chain for d+Au",kFALSE},
  {"CComplete"   ,""  ,"","Complete,C2default"             ,"","","Turn on chain for Complete STAR",kFALSE},
  // Detector combined-chains
  {"SvtD"       ,"","","SvtCalDb,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit",    "","","SVT chain for Data",kFALSE},
  // Year 1 chains
  {"P00h"        ,""  ,"","ry1h,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
   ,                                                        "Production chain for summer 2000 data",kFALSE},
  {"P2000"       ,""  ,"","ry2000,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB,NoHits","",""
   ,                                                        "Production chain for summer 2000 data",kFALSE},
  {"B2000" ,"","","ry2000,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,NosvtIT,NossdIT","",""
   ,                                                               "Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2000a"      ,""  ,"","B2000,Corr1","",""              ,"Production chain for summer 2000 data",kFALSE},
  // Year 2 chains.
  // B2001 is a base-chain for 2001 (with tpc+rhic).
  {"B2001"       ,""  ,"","ry2001,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,svtDb","",""
   ,"Base chain for 2001 (tpc+rhic)",kFALSE},
  {"P2001"       ,""  ,"","B2001,l3onl,tofDat,Corr2,OSpaceZ","",""
   ,                                            "Production chain for summer 2001 data (+ l3, tof)",kFALSE},
  {"P2001a"      ,""  ,"","B2001,svt_daq,SvtD,ftpc,l3onl,tofDat,emcDY2,Corr2,OSpaceZ","",""
   ,                            "Production chain for summer 2001 data (+ ftpc, svt, l3, tof, emc)",kFALSE},
  // pp Chains
  {"pp2001","","","ppOpt,B2001,-PreVtx,l3onl,tofDat,emcDY2,Corr2","","" ,"pp 2001 (+ l3, tof, emc)",kFALSE},
  {"pp2001a"     ,""  ,"","pp2001,svt_daq,SvtD,ftpc","",""   ,"pp 2001 (+ ftpc, svt, l3, tof, emc)",kFALSE},
  // Year 3 chains
  // B2003 is a base-chain with tpc only for now
  {"B2003"       ,""  ,"","ry2003,in,tpc_daq,tpc,Physics,Cdst,Kalman,tags,Tree,evout,svtDb","",""
   ,                                                                    "Base chain for 2003 (tpc)",kFALSE},
  {"dau2003"     ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc","",""
   ,              "Production chain for winter 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc)",kFALSE},
  {"dau2003a"    ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd","",""
   ,        "Production chain for winter 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc, trgd)",kFALSE},
  {"pp2003"      , "" ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd","",""
   ,        "Production chain for Spring 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc, trgd)",kFALSE},
  
  {"Idst"        ,""  ,"",              "dst,event,compend,EventQA"   ,"","","Turn on DST for ITTF",kFALSE},
  {"IAna"    ,""  ,"","dEdxY2,Kink2,xi2,CMuDst,analysis","",""  ,"Turn on Xi, Kink, dEdx and MuDst",kFALSE},
  {"BAna"    ,""  ,"","dEdxY2,CMuDst,analysis"          ,"",""            ,"Turn on dEdx and MuDst",kFALSE},
#ifdef __KEEP_TPCDAQ_FCF__
  {"B2003I"      ,"","","ry2003,in,tpc_daq,tpcI,fcf,Physics,Idst,l0,tags,Tree,evout,svtDb"
    ,                                                               "","","Base chain for 2003 ITTF",kFALSE},
#else
  {"B2003I"      ,"","","ry2003,in,TpxRaw,TpxClu,Idst,l0,tags,Tree,evout,svtDb"
   ,                                                               "","","Base chain for 2003 ITTF",kFALSE},
#endif
  {"dau2003i"    ,"","","B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,l3onl,tofDat,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd"
   ,                               "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
  {"pp2003i","","","B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd"
   ,                               "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
  {"B2004"       ,"","","ry2004,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                           "Base chain for 2004 ITTF (tpc+svt)",kFALSE},
  // Notes:
  //  fcf was not added by default to allow switching if needed
  //  there is no PreVtx in tpcI so no need to do -PreVtx for pp chain
  //  SVT is added as base default, svtIT in chains
  {"P2004","" ,"","B2004,IAna,fcf,VFMinuit,l3onl,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2"
   ,          "","","Production chain for 2003/2004 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2004"      ,"" ,"",
   "B2004,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,l3onl,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2"
   ,          "","","Production chain for 2003/2004 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 5 Chains
  {"B2005"       ,"","","ry2005b,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                           "Base chain for 2005 ITTF (tpc+svt)",kFALSE},
  {"B2005a"      ,""       ,"","ry2005b,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout,svtDb,ssdDb","",""
   ,                                                          "Base chain for 2005 ITTF (tpc only)",kFALSE},
  {"B2005b"      ,"" ,"","ry2005f,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                "Base chain for 2005 ITTF Geo f (tpc+svt only)",kFALSE},
  {"P2005"       ,"" ,"",   "B2005,IAna,fcf,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,          "","","Production chain for 2004/2005 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2005b"      ,"" ,"",   "B2005b,IAna,fcf,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,          "","","Production chain for 2004/2005 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005","" ,"","B2005,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,            "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005a","","","B2005a,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4"
   ,            "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005b","","",   "B2005b,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4"
   ,            "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 6 chains - Geometry 2006 not yet ready, starting with y2005d
  {"B2006"       ,""       ,"","ry2005d,in,tpc_daq,tpcI,svt_daq,SvtD,Idst,tags,Tree,evout,ssdDb","",""
   ,                                                           "Base chain for 2006 ITTF (tpc+svt)",kFALSE},
  {"B2006a"      ,""       ,"","ry2005d,in,tpc_daq,tpcI,Idst,tags,Tree,evout,svtDb,ssdDb","",""
   ,                                          "Base chain for 2006 with 2005d geom ITTF (tpc only)",kFALSE},
  {"B2006b"      ,""       ,"","ry2006,in,tpc_daq,tpcI,Idst,l0,tags,Tree,evout,svtDb,ssdDb","",""
   ,                                                          "Base chain for 2006 ITTF (tpc only)",kFALSE},
  {"B2006g"      ,""       ,"","ry2006g,in,tpc_daq,tpcI,Idst,l0,tags,Tree,evout,svtDb,ssdDb","",""
   ,                                                    "Base chain for 2006 ITTF geo g (tpc only)",kFALSE},
  {"pp2006a"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006a,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3"
   ,            "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006b"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006b,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,BeamBack"
   ,            "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006g"      ,"" ,"",     // added 2008 after geometry corrections
   "B2006g,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,BeamBack"
   ,      "","","Production chain for 2005 pp data geo g (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 7 chains - Geometry 2007 hopefully fine
  {"T2007","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,Tree,evout"
   ,                                                             "","","TPC only chain,  2007 ITTF",kFALSE},
  {"B2007","","","ry2007,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,spt,Idst,l0,tags,Tree,evout"
   ,                                                 "","","Base chain for 2007 ITTF (tpc+svt+ssd)",kFALSE},
  {"B2007g","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,spt,Idst,l0,tags,Tree,evout"
   ,                                           "","","Base chain for 2007 ITTF geo g (tpc+svt+ssd)",kFALSE},
  {"P2007"       ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5"
   ,               "","","Production chain for 2007 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  
  {"P2007g"      ,"" ,"",   // chain was set in 2008 to account for missing material
   "B2007g,IAna,KeepSvtHit,hitfilt,VFMinuit2,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5"
   , "","","Production chain for 2007 data, revised 2008 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // startup for calib
  {"P2007a"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr3"
   ,         "","","Production chain for 2007 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2007b"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr4"
   ,         "","","Production chain for 2007 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  //  {"testing"      ,"" ,"",   // just a damned test
  //   "B2006b,sdt20061211,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
  //                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // 
  {"B2008" ,"","","ry2008,in,tpc_daq,tpcI,fcf,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2008 ITTF (tpc)",kFALSE},
  {"B2008a","","","ry2008,in,tpcX,ToFx,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                           "Base chain for 2008 ITTF (tpc+tof)",kFALSE},
  // startup for calib
  {"P2008a"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr3,analysis"
   ,         "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008b"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,         "","","Production chain for 2008 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // or VFPPVnoCTB
  {"pp2008a"      ,"" ,"",   
   "B2008,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,         "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008c"       ,"" ,"",   // ATTENTION: the below chain was used for preliminary results on low energy
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,               "","","Production chain for 2008 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2008c"      ,"" ,"",  // Note: this chain was not used and may be removed
   "B2008,IAna,hitfilt,ppOpt,Minuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,         "","","Production chain for 2008 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // convergence chains
  {"pp2008"     ,"" ,"",   // VFPPV was chosen for p+p as final production chain
   "B2008a,IAna,hitfilt,ppOpt,VFPPV,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,         "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},   
  {"P2008"       ,"" ,"",  // this one is final and official production ready, June 2008
   "B2008a,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,               "","","Production chain for 2008 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  
  //
  // Chains for 2009 run p+p essentially
  // Note that we always need to start with VFMinuit as VFPPV is full of asserts
  //
  {"B2009.1","","","ry2009,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2009 ITTF (tpc)",kFALSE},
  {"B2009.2","","","ry2009a,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2009 ITTF (tpc)",kFALSE},
  
  {"pp2009a"      ,"" ,"",   
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,          "","","Production chain for 2009 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2009b"      ,"" ,"",   
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,    "","","Production chain for 2009 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, no trigger)",kFALSE},
  {"pp2009c"      ,"" ,"",   
   "B2009.2,BAna,hitfilt,ppOpt,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis","",""
   ,          "Production chain for 2009 data - no Corr, no VF (+l3, bcc/fpd, ftpc, e/b-emc, trig)",kFALSE},
  
  
  
  // chains for year 10
  {"B2010","","","ry2010,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2010 ITTF (tpc)",kFALSE},
  
  {"P2010a","" ,"",  // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt
   "B2010,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                "","","Production chain for 2010 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc)",kFALSE},
  {"pp2010a","" ,"", // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt,VFPPVnoCTB
   "B2010,BAna,hitfilt,ppOpt,l3onl,emcDY2,fpd,trgd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,         "","","Production chain for 2010 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, no VF)",kFALSE},   
  
  
  // chains for year 11
  {"B2011","","","ry2011,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2011 ITTF (tpc)",kFALSE},
  
  {"P2011a","" ,"",  // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt
   "B2011,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                "","","Production chain for 2011 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc)",kFALSE},
  {"pp2011a","" ,"", // initial chain - Add some to all of BEmcChkStat,QAalltrigs,btof,Corr3,-hitfilt,VFPPVnoCTB
   "B2011,BAna,hitfilt,ppOpt,l3onl,emcDY2,fpd,trgd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis",
   "","","Production chain for 2011 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, no VF)",kFALSE},   
  
  
  // Other chains/Calibration
  {"LaserCal0","" ,"","db,detDb,tpc_daq,tpcDb,tcl,globT,laser,LaserTest","",""
   ,                                                                "Laser Calibration Chain (tcl)",kFALSE},
  {"LaserCal",""  ,"","db,detDb,tpc_daq,tpcDb,fcf,globT,laser,LaserTest","",""
   ,                                                                      "Laser Calibration Chain",kFALSE},
  {"L3Counter","" ,"","db,detDb,in,l3count","","",                     "L3 Counter extraction pass",kFALSE},
  {"VtxSeedCal","","","ppOpt,ry2001,in,tpc_daq,tpc,globT,-Tree,Physics,-PreVtx,FindVtxSeed,NoEvent,Corr2"
   ,                                                                 "","","Pass0 Vertex evaluator",kFALSE},
  {"SpcChgCal","","","B2004,fcf,Corr3,OSpaceZ2,OShortR,SCEbyE,-Tree,-tags,-EvOut,-EventQA"
   ,                                                            "","","Pass0 SpaceCharge evaluator",kFALSE},
  // New-- DBV20050515,useCDV Old-- Corr3,OSpaceZ2,OShortR,SCEbyE
  {"SpcChgCalG","","","MuDST,fcf,Corr4,OSpaceZ2,OGridLeak3D,SCEbyE,-Tree,-tags,-EvOut,-EventQA"
   ,                "","","Pass0 SpaceCharge evaluator with GridLeak, no geo or tracker dependence",kFALSE},
  {"VtxSeedCalG","","","MuDST,fcf,Corr4,FindEvtVtxSeed,-Tree,-tags,-EvOut,-EventQA"
   ,                                                                 "","","Pass0 Vertex evaluator",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"OPTIONS     ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"ITTF"      ,"","","","","","request to use one of Sti(default), StiCA, Stv,StvCA, ... trackers",kFALSE},
  {"SvtHitFilt"  ,"", "","",                                           "","","SVT Hit filter Maker",kFALSE},
  {"NoHits"      ,""  ,"",""                            ,"","","Don't write hits into Event.Branch",kFALSE},
  {"Kalman"      ,""  ,"",""                            ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Eval"        ,""  ,"","","",""                ,"Turn on evaluation switch for different makers",kFALSE},
  {"Ev03"        ,""  ,"","","",""                                 ,"Turn on alternative V0 method",kFALSE},
  {"off"         ,""  ,"","","",""                                        ,"Turn off default chain",kFALSE},
  {"clearDAQCTB" ,""  ,"","","" ,""                             ,"clear DAQ CTB Hits for embedding",kFALSE},
  {"NoInput"     ,""  ,"","","" ,""                                                ,"No input file",kFALSE},
  {"util"        ,""  ,"","","","StAnalysisUtilities",                   "Load StAnalysisUtilities",kFALSE},
  {"StUtilities" ,""  ,"","","","StUtilities",                                   "Load StUtilities",kFALSE},
  {"FieldOn"     ,""  ,"","MagF"                                   ,"","" ,"Constant nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","MagF"                                          ,"","" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","MagF"                                         ,"","","Half Field option",kFALSE},
  {"ReverseField",""  ,"","MagF"                                      ,"","","Reverse Field option",kFALSE},
  {"NoCintDb"    ,""  ,"",""                                        ,"","","Switch off all Cint Db",kFALSE},
  {"NoStarCintDb",""  ,"",""                                   ,"","","Switch off standard Cint Db",kFALSE},
  {"NoLocalCintDb","" ,"",""                                      ,"","","Switch off local Cint Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoCintCalDb" ,""  ,"","NoLocalCintDb"                         ,"","","Switch off local Cint Db",kFALSE},
  {"dbSnapshot"  ,""  ,"",""                                         ,"","","Create?use dbSnapshot",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug1"      ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug2"      ,""  ,"",""                                            ,"","","Set debug flag = 2",kFALSE},
  {"IdTruth"     ,""  ,"",""              ,"","","Enable IdTruth association in StAssociationMaker",kFALSE},
  {"OldMiniMc"   ,""  ,"",""                           ,"","","Keep pre-2008 convention for MiniMc",kFALSE},
  {"useInTracker",""  ,"","","",""   ,"switch from EGR to ITTF global tracks in StAssociationMaker",kFALSE},
  {"noRepeat"    ,""  ,"",""                                        ,"","","No repeat in Messenger",kFALSE},
  {"noHistos"    ,""  ,"",""                                    ,"","","Disables Attributes histos",kFALSE},
  {"noRunco"     ,""  ,"",""                                     ,"","","Disables Attributes runco",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},
  {"big"         ,""  ,"",""                                         ,"","","Set NwGEANT =20Mwords",kFALSE},
  {"bigbig"      ,""  ,"",""                                         ,"","","Set NwGEANT =40Mwords",kFALSE},
  {"clearmem"    ,""  ,"",""                           				  ,"","","Obsolete",kFALSE},
  {"adcOnly"     ,""  ,"",""                          ,"","","DAQMaker selects only TPC ADC events",kFALSE},
  {"InTree"      ,""  ,"","in",""                                     ,"","bfcTree Input Tree name",kFALSE},
  {"OutTree"     ,""  ,"","Tree",""                                  ,"","bfcTree Output Tree name",kFALSE},
#if 0
  {"DstOut"      ,""  ,"","Tree"                                       ,"","","Write dst to StTree",kFALSE},
#else
  {"DstOut"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"McEvOut"     ,""  ,"","StMcEvent,Tree"                       ,"","","Write StMcEvent to StTree",kFALSE},
  {"EvOut"       ,""  ,"","Tree"                                   ,"","","Write StEvent to StTree",kFALSE},
  {"GeantOut"    ,""  ,"","Tree"                                ,"","","Write g2t tables to StTree",kFALSE},
  {"Simu"        ,""  ,"",""                                                ,"","","Simulated Data",kFALSE},
  {"HitsBranch"  ,""  ,"",""  ,"","","take out points from dst branch and put them into HitsBranch",kFALSE},
  {"paw"         ,""  ,"",""                                      ,"","","Allocate memory for pawc",kFALSE},
  {"AllEvent"    ,""  ,"","Tree"                               ,"","","Write whole event to StTree",kFALSE},
  {"AllTables"   ,""  ,"","",""                                     ,"St_Tables","Load Star Tables",kFALSE},
  
  {"Corr1"       ,""  ,"","AlignSectors,ExB,OBmap,OClock,OPr13","",""
   ,                                                  "... AlignSectors,ExB,OBmap,OClock,OPr13 ...",kFALSE},
  {"Corr2"       ,""  ,"","Corr1,OTwist,OIFC"                     ,"","","...Corr1+OTwist,OIFC ...",kFALSE},
  {"Corr3"       ,""  ,"","AlignSectors,ExB,OBmap2D,OClock,OPr13,OTwist,OIFC","",""
   ,                                    "... AlignSectors,ExB,OBmap2D,OClock,OPr13,OTwist,OIFC ...",kFALSE},
  {"Corr4"       ,""  ,"","Corr3,OShortR"                             ,"","","... Corr3+OShortR...",kFALSE},
  {"Corr5"       ,""  ,"","Corr4,SCEbyE,OGridLeak3D,OSpaceZ2","",""
   ,                                                     "... Corr4+SCEbyE,OGridLeak3D,OSpaceZ2...",kFALSE},
  {"ExB"         ,""  ,"","",""                                       ,"","Activate ExB correction",kFALSE},
  {"EB1"         ,""  ,"","",""                                     ,"","Force ExB configuration 1",kFALSE},
  {"EB2"         ,""  ,"","",""                                     ,"","Force ExB configuration 2",kFALSE},
  {"OBmap"       ,""  ,"","",""                                          ,"","ExB shape correction",kFALSE},
  {"OBmap2D"     ,""  ,"","",""                                      ,"","ExB 2 D shape correction",kFALSE},
  {"OTwist"      ,""  ,"","",""                                          ,"","ExB twist correction",kFALSE},
  {"OClock"      ,""  ,"","",""                                     ,"","Clock/tpc rot. correction",kFALSE},
  {"OPr13"       ,""  ,"","",""                                          ,"","PadRow 13 distortion",kFALSE},
  {"OCentm"      ,""  ,"","",""                                   ,"","Central membrane correction",kFALSE},
  {"OECap"       ,""  ,"","",""                                    ,"","EndCap (curved) correction",kFALSE},
  {"OIFC"        ,""  ,"","",""                                         ,"","Field Cage correction",kFALSE},
  {"OSpaceZ"     ,""  ,"","",""                                      ,"","Space Charge corrections",kFALSE},
  {"OSpaceZ2"    ,""  ,"","",""                                   ,"","Space Charge corrections R2",kFALSE},
  {"OShortR"     ,""  ,"","",""                                       ,"","Shorted Ring correction",kFALSE},
  {"OGridLeak"   ,""  ,"","",""                                          ,"","Grid Leak correction",kFALSE},
  {"OGridLeak3D" ,""  ,"","",""                                       ,"","3D Grid Leak correction",kFALSE},
  {"OGGVoltErr"  ,""  ,"","",""                                   ,"","GG voltage error correction",kFALSE},
  {"OSectorAlign",""  ,"","",""                        ,"","Sector alignment distortion correction",kFALSE},
  {"AlignSectors",""  ,"","",""                            ,"","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"DbRichSca"   ,""  ,"","detdb","","",                    "Force reading of Rich scalers from DB",kFALSE},
  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},
  {"pp"          ,""  ,"","",                            "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ppOpt"       ,""  ,"","TrsPileUp","","",             "pp option without enabling special cases",kFALSE},
  {"TrsPileUp"   ,""  ,"","","","",                                              "Trs pile up mode",kFALSE},
  {"TrsToF"      ,""  ,"","","","",                       "Trs account for particle time of flight",kFALSE},
  {"SvtMatchVtx" ,""  ,"","",""                            ,"","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"VtxOffSet"   ,""  ,"","",""                 ,"","Account Primary Vertex offset from y2000 data",kFALSE},
  {"Calibration" ,""  ,"","",""                                              ,"","Calibration mode",kFALSE},
  {"beamLine"    ,""  ,"","",""                                       ,"","LMV Beam line constrain",kFALSE},
  {"min2trkVtx"     ,""  ,"","",""                 ,"","...only 2 tracks needed for vertex finding",kFALSE},
  // WARNING: introduction of usePct4Vtx with default:false breaks backward compatibility.
  // See related code in StBFChain.cxx for details
  {"usePct4Vtx"     ,""  ,"","",""                ,"","Use Post-Crossing Tracks for vertex finding",kFALSE},
  {"useBTOF4Vtx"    ,""  ,"","",""                 ,"","Use BTOF track matching for vertex ranking",kFALSE},
  {"svt1hit",""  ,"","",""                                     ,"","Use 1 SVT hit only combination",kFALSE},
  {"CtbMatchVtx"    ,""  ,"","VFMinuit",""              ,"","... CTB Matching ON in Vertex Finding",kFALSE},
  {"VFMinuit"       ,""  ,"","genvtx",""                ,"","... Generic VF will use Minuit method",kFALSE},
  {"VFMinuit2"      ,""  ,"","genvtx","",  "","... Generic VF will use Minuit method w/rank mode 2",kFALSE},
  {"VFMinuit3"      ,""  ,"","genvtx","",  "","... Generic VF will use Minuit method w/rank mode 3",kFALSE},
  {"VFFV"           ,""  ,"","genvtx",""                            ,"","... Fixed dummy VF method",kFALSE},
  {"VFMCE"          ,""  ,"","genvtx",""                        ,"","... Fixed vertex from MCEvent",kFALSE},
  {"VFppLMV"        ,""  ,"","genvtx",""                 ,"","...VertexMaker will use ppLMV method",kFALSE},
  {"VFppLMV5"       ,""  ,"","genvtx",""         ,"","...VertexMaker will use ppLMV method (tuned)",kFALSE},
  {"onlcl"  ,""  ,"","",""                                       ,"","Read/use TPC DAQ100 clusters",kFALSE},
  {"onlraw" ,""  ,"","",""                                              ,"","Read/use TPC raw hits",kFALSE},
  {"ezTree" ,""  ,"","",""                                               ,"","Create ezTree branch",kFALSE},
  {"BEmcDebug","" ,"","",""                            ,"","Turn OFF B-EMC hit reconstruction cuts",kFALSE},
  {"BEmcChkStat","" ,"","",""                             ,"","Turn ON status checking in raw data",kFALSE},
  // Those options are for StTpcDbMaker
  {"useLDV" ,""  ,"","",""                                   ,"","... uses laserDV database flavor",kFALSE},
  {"useCDV" ,""  ,"","",""                                       ,"","... uses ofl database flavor",kFALSE},
  {"useNewLDV" ,""  ,"","",""                                    ,"","... uses ofl database flavor",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Tables      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"tables","","","StDbT,ctf_T,ebyeT,emc_T,ftpcT,gen_T,geomT,globT,l3_T,sim_T,svt_T,tpc_T","","","",kFALSE},
  {"StDbT"       ,""  ,"","",""                                   ,"StDb_Tables","Load StDb_Tables",kFALSE},
  {"ctf_T"       ,""  ,"","",""                                     ,"ctf_Tables","Load ctf_Tables",kFALSE},
  {"ebyeT"       ,""  ,"","",""                                   ,"ebye_Tables","Load ebye_Tables",kFALSE},
  {"emc_T"       ,""  ,"","",""                                     ,"emc_Tables","Load emc_Tables",kFALSE},
  {"ftpcT"       ,""  ,"","",""                                   ,"ftpc_Tables","Load ftpc_Tables",kFALSE},
  {"gen_T"       ,""  ,"","",""                                     ,"gen_Tables","Load gen_Tables",kFALSE},
  {"geomT"       ,""  ,"","",""                           ,"geometry_Tables","Load geometry_Tables",kFALSE},
  {"globT"       ,""  ,"","",""                               ,"global_Tables","Load global_Tables",kFALSE},
  {"l3_T"        ,"",  "","",""                                       ,"l3_Tables","Load l3_Tables",kFALSE},
  {"mwc_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"sim_T"       ,""  ,"","",""                                     ,"sim_Tables","Load sim_Tables",kFALSE},
  {"svt_T"       ,""  ,"","",""                                     ,"svt_Tables","Load svt_Tables",kFALSE},
#ifdef __KEEP_TPCDAQ_FCF__
  {"tpc_T"       ,""  ,"","",""                                     ,"tpc_Tables","Load tpc_Tables",kFALSE},
#else
  {"tpc_T"       ,""  ,"","",""                            ,"","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"trg_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"vpd_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Embedding"   ,"","","-Simu"                                              ,"","","Embedding run",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Geometry+Mag","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"pgf77"       ,"" ,"","",""                                                ,"pgf77VMC","Fortran",kFALSE},
  {"rootcern"    ,"" ,"","geant3",""                                   ,"","ROOT minicern OBSOLETE",kFALSE},
  {"minicern"    ,"" ,"","geant3",""                       ,"","STAR addition to minicern OBSOLETE",kFALSE},
  {"mysql"       ,"" ,"","",""                                               ,"mysqlclient","MySQL",kFALSE},
  {"libPhysics"  ,"" ,"","",""                                              ,"libPhysics","TVector",kFALSE},
  {"geant3"      ,"" ,"","",""   ,"Geom,EG,Pythia6,EGPythia6,VMC,geant3","TGeo version of geant321",kFALSE},
  {"geometry"    ,"" ,"","",""                                     ,"geometry","geometry+Mag.Field",kFALSE},
  {"StarMagField","", "","magF"                              ,"","StarMagField","Load StarMagField",kFALSE},
  {"geomNoField" ,"" ,"","-geometry,StarMagField"        ,"","geometryNoField","geometry-Mag.Field",kFALSE},
  {"UseProjectedVertex" ,"" ,"",""                ,"","","Run StBTofCalibMaker w/wo Primary Vertex",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"vpd"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"tls"         ,""  ,"","",""                          "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"rts"         ,""  ,"","",""                                                ,"RTS","load libRTS",kFALSE},
  {"daq"         ,""  ,"","rts",""                         ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                        ,"Geom,StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"BTofUtil"    ,""  ,"","",""                                     ,"StBTofUtil","Load StBTofUtil",kFALSE},
  {"MtdUtil"    ,""  ,"","",                                        "","StMtdUtil","Load StMtdUtil",kFALSE},
  {"StBichsel"   ,""  ,"","",""                         ,"StBichsel","Load Bichsel model for dE/dx",kFALSE},
  {"StEvent"     ,""  ,"","globT,SCL,TRGDef,StBichsel,EmcUtil,TbUtil",""  ,"StEvent","Load StEvent",kFALSE},
  {"SsdUtil"     ,""  ,"","StarMagField,StEvent",""               ,"Geom,StSsdUtil","Load SSD Util",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T,geomT,StDbT",""                      ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"EEmcUtil"    ,""  ,"","",""                                     ,"StEEmcUtil","Load StEEmcUtil",kFALSE},
  {"FgtUtil"     ,""  ,"","",""                                       ,"StFgtUtil","Load StFgtUtil",kFALSE},
#if 0
  {"l3Util"      ,""  ,"","",""                                         ,"Stl3Util","Load Stl3Util",kFALSE},
#else
  {"l3Util"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
#endif
  {"PmdUtil"     ,""  ,"","","",                                       "StPmdUtil","Load StPmdUtil",kFALSE},
  {"QUtils"      ,""  ,"","PmdUtil,EmcUtil","",                      "","Load QA Libs dependencies",kFALSE},
  {"Stu"         ,""  ,"","","",                         "StEventUtilities","Load StEventUtilities",kFALSE},
#ifndef  __NoStrangeMuDst__
  {"MuDSTDeps"   ,"","","StEvent,Stu","","StStrangeMuDstMaker,Tree","Load MuDST misc. dependencies",kFALSE},
#else /* ! __NoStrangeMuDst__  StMuDSTMaker has to be compiled with -D__NO_STRANGE_MUDST__*/
  {"MuDSTDeps"   ,"" ,"","StEvent,Stu",             "","Tree","Load MuDST misc. dependencies (all)",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"MuDST"       ,"" ,"","MuDSTDeps,EmcUtil,TofUtil,BTofUtil,PmdUtil",""
   ,                                                            "StMuDSTMaker","Load MuDST library",kFALSE},
#if 0 /* Always load gstar */
  {"geantL","","","geomT,gen_T,sim_T,StarMagField,geomNoField","","Geom,St_g2t,St_geant_Maker,gstar"
   ,                                                                               "Load GeantLibs",kFALSE},
#else
  {"geantL","","","geomT,gen_T,sim_T,StarMagField,geomNoField","","Geom,St_g2t,St_geant_Maker"
   ,                                                                               "Load GeantLibs",kFALSE},
#endif
  {"gstarLib","","",""                                                 ,"","gstar","Load gstar lib",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"gstar"       ,"geant"  ,"","-fzin,-ntin,-geant,Simu,geantL","St_geant_Maker"
   ,                                        "","gstar for 80 muon tracks with pT = 1GeV in |eta|<4",kFALSE},
  {"tdaq"        ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"miniDAQ"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"fzin"        ,"geant","","Simu,-gstar,-ntin,-geant,geantL","St_geant_Maker",""
   ,                                                                           "read gstar fz-file",kFALSE},
  {"in"         ,""  ,"",""              ,     "StIOMaker","StIOMaker","Read [DAQ|ROOT] input file",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"db"          ,"db"   ,"","StDbT"             ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"detDb","detDb","","db","StDetectorDbMaker","StDetectorDbMaker","Load and run StDetectorDbMaker",kFALSE},
  {"magF"        ,"MagField","","StDbT,db,detDb","StMagFMaker","StarMagField,StMagF"
   ,                                                      "Mag.field map with scale factor from Db",kFALSE},
  {"mtin"        ,"geant"  ,"","-fzin,-geant,-gstar,geantL,gstarLib,-magF","St_geant_Maker"
   ,                                                                "gstar","read event from MuDst",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,detDb,StarMagField,magF"    ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"dbutil"      ,""     ,"","detDb,StDbT"                 ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"svtDb"       ,"svtDb","","tpcDb,SvtCL", "StSvtDbMaker","StSvtDbMaker","Load and run SvtDbMaker",kFALSE},
  {"ssdDb"      ,"ssdDb","","tpcDb,SsdUtil","StSsdDbMaker","StSsdDbMaker","Load and run SsdDbMaker",kFALSE},
  {"svtCalDb"    ,""     ,"","svtDb"         ,"","","Declare Calibrations/svt as while list member",kFALSE},
  {"ssdCalDb"    ,""     ,"","ssdDb"         ,"","","Declare Calibrations/ssd as while list member",kFALSE},
  {"eemcDb"      ,"eeDb" ,"","db",               "StEEmcDbMaker","StEEmcDbMaker","Load EEmcDbMaker",kFALSE},
  {"fmsDb"       ,"fmsDb","","db",                  "StFmsDbMaker","StFmsDbMaker","Load FmsDbMaker",kFALSE},
  
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  // for simulation on fly Event time stamp is set outside of the simulation makers
  {"ntin"        ,"geant"  ,"","paw,-fzin,-geant,-gstar,Simu,geantL,paw","St_geant_Maker"
   ,                                                   "gstar","read event generated Hbook nt-file",kFALSE},
  {"PrepEmbed","","","geantEmb","StPrepEmbedMaker","St_geant_Maker"
   ,                                                             "Prepare kinematics for embedding",kFALSE},
  {"geant"       ,"geant","","geantL"                          ,"St_geant_Maker","","passive GEANT",kFALSE},
  {"geantEmb"    ,"geant","","geantL"                   ,"St_geant_Maker","","GEANT embedding mode",kFALSE},
  {"RootVMC","","","-geant,-fzin,-ntin,StarMagField,-geantL,-geometry,-geomNoField,geant3","","","",kFALSE},
  {"VMCAppl" ,"","","geomT,gen_t,sim_T,tpcDB,svtDb,ssdDb,RootVMC,","","StarVMCApplication","VMC G3",kFALSE},
  {"VMC"         ,"geant","","Simu,VMCAppl,-geant,VmcGeo","StVMCMaker",    "StVMCMaker","VMC Maker",kFALSE},
  {"VMCPassive"  ,"geant","","VMCAppl,VmcGeo","StVMCMaker","StVMCMaker","VMC Maker in Passive Mode",kFALSE},
  {"trg"         ,"trg","l0Chain","trg_T,globT,db","St_trg_Maker","St_trg,St_trg_Maker",
   "trigger analysis for Year 2001-2005 data",kFALSE},
  {"TRGDef"      ,""  ,"","",""                          ,"StTriggerDataMaker","Load StTriggerData",kFALSE},
  {"trgd"        ,"trgd","","TRGDef"  ,"StTriggerDataMaker","StTriggerDataMaker","Get trigger data",kFALSE},
  {"MakeEvent","0Event","","StEvent,tpcDB,detDb","StEventMaker","StEventMaker",
   "<Early StEvent creation>",kFALSE},
  {"LaserAvEv"          ,"","",""                        ,"StLaserAvEventMaker","StLaserAvEvent","",kFALSE},
  {"l0"          ,"l0Chain","","globT,ctf,trg"                              ,"StMaker","StChain","",kFALSE},
  {"ctf"         ,"ctf","l0Chain","ctf_T,db" ,"St_ctf_Maker","St_ctf,St_ctf_Maker","ToF simulation",kFALSE},
  {"mwc"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ppMCTrig"    ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"pp2pp"       ,"","","",                              "St_pp2pp_Maker","St_pp2pp_Maker","pp->pp",kFALSE},
#ifdef __KEEP_TPCDAQ_FCF__
  {"tpc"         ,"","","tcl,tpcI" ,"","","WARNING *** Option is OBSOLETE *** use tcl,tpcI instead",kFALSE},
  {"tpcI" ,"tpcChain","","tpc_T,globT,db,tpcDB,fcf,TpcHitMover","StMaker","StChain","tpc with ITTF",kFALSE},
  {"tpcX" ,"tpcChain","","-tpcI,tpx,MakeEvent"            ,"StMaker","StChain","tpc+tpcx with ITTF",kFALSE},
#else
  {"tpc" ,"","","TpxRaw,TpxClu,tpcI" ,"","","WARNING *** Option is OBSOLETE *** use TpxClu instead",kFALSE},
  {"tpcI" ,"tpcChain","","db,tpcDB,TpcHitMover",                "StMaker","StChain","tpc with ITTF",kFALSE},
  {"tpcX" ,"tpcChain","","-tpcI,tpx,MakeEvent"            ,"StMaker","StChain","tpc+tpcx with ITTF",kFALSE},
#endif 
  {"Trs","Trs","tpcChain","scl,tpcDB,TrsToF,StEvent,EmbeddingShortCut","StTrsMaker","StTrsMaker","",kFALSE},
  {"TpcRS","","tpcChain","scl,tpcDB,-Trs,-EmbeddingShortCut","StTpcRSMaker"
   ,"libMathMore,StdEdxY2Maker,StTpcRSMaker",                          "New Tpc Response Simulator",kFALSE},
  {"EmbeddingShortCut","","","",              "","","Short Cut for StdEdxY2Maker and StTpcHitMover",kFALSE},
  {"StMcEvent"   ,"","","gen_t,sim_T"                                            ,"","StMcEvent","",kFALSE},
  {"McEvent" ,"","","StEvent,tpcDb,EEmcUtil,EmcUtil,StMcEvent","StMcEventMaker","StMcEventMaker","",kFALSE},
  {"Mixer"       ,"tpc_raw","","daq","StMixerMaker"                   ,"StTrsMaker,StMixerMaker","",kFALSE},
  {"St_tpc"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"St_svt"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#ifdef __KEEP_TPCDAQ_FCF__
  {"tpc_daq"  ,"tpc_raw","tpcChain","detDb,tpc_T","St_tpcdaq_Maker","StTrsMaker,St_tpcdaq_Maker","",kFALSE},
  {"tcl"         ,"","fcf","","","",           "WARNING *** Option is OBSOLETE *** use fcf instead",kFALSE},
  {"fcf","","tpcChain","daq,-tcl,tpc_daq,StEvent","StRTSClientFCFMaker","StRTSClientFCF,StRTSClientFCFMaker"
   ,                                                                   "Offline FCF Cluster finder",kFALSE},
#else /* __KEEP_TPCDAQ_FCF__ */
  {"tpc_daq"  ,"","","TpxRaw",        "","","WARNING *** Option is OBSOLETE *** use TpxRaw instead",kFALSE},
  {"tcl","","","TpxRaw,TpxClu,MakeEvent","","","St_tcl_Maker has been replaced by StTpcRTSHitMaker",kFALSE},
  {"fcf"      ,"","","-tcl,tpcX",    "","","StRTSClientFCFMaker has been replaced by StTpcHitMaker",kFALSE},
#endif /* __KEEP_TPCDAQ_FCF__ */
  {"tfs"     ,"","","TpcFastSim","","","WARNING *** Option is OBSOLETE *** use TpcFastSim instead",kFALSE},
  {"TpcFastSim"  ,"tpc_hits","tpcChain","MakeEvent,Simu,-trs,-TpcRS,-tcl,-fcf,-tpc_daq,StEvent,"
   "EmbeddingShortCut",              "StTpcFastSimMaker","St_tcl_Maker","use tfs (no Trs or TpcRS)",kFALSE},
#ifdef __KEEP_TPCDAQ_FCF__
  {"tpx"         ,"tpc_hits","tpcChain","MakeEvent,-trs,-TpcRS,-tcl,-fcf,-tpc_daq,StEvent,rts,detDb"
   ,                  "StTpcHitMaker","StTpcHitMaker","TPC hit reader for tpc + tpx via EVP_READER",kFALSE},
#else
  {"tpx"         ,"tpx_hits","tpcChain","MakeEvent,tpc_T,StEvent,rts,detDb" 
   ,                  "StTpcHitMaker","StTpcHitMaker","TPC hit reader for tpc + tpx via EVP_READER",kFALSE},
#endif
  {"TpxPulser","TpxPulser","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker","TPC+TPX pulser analysis"
   ,                                                                                                kFALSE},
  {"TpxPadMonitor","TpxPadMonitor","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
   ,                                                                         "TPC+TPX pad monitor", kFALSE},
  {"TpxAvLaser","TpxAvLaser","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
   ,                        "TPC+TPX averaging laser events into Sparse histogram on pixels level", kFALSE},
  {"TpxDumpPxls2Nt","TpxDumpPxls2Nt","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
   ,                                                                "TPC+TPX pixel dump to NTuple", kFALSE},
  {"TpxRaw","TpxRaw","tpcChain","rts,detDb,StEvent","StTpcHitMaker","StTpcHitMaker"
   ,              "TPC+TPX convert DAQ Tpc Raw Data to TpcRawEvent used by TpcMixer and/or TpxClu", kFALSE},
  {"TpcMixer","","tpcChain","StEvent,rts,-Mixer,TpxRaw"       ,"StTpcMixerMaker","StTpcHitMaker","",kFALSE},
  {"TpxClu","tpc_hits","tpcChain","rts,tpcDb,detDb,-tpx,-tpc_daq,-fcf","StTpcRTSHitMaker"
   ,"StTpcHitMaker",                                                    "RTS(online) cluster maker",kFALSE},
  {"Velo"        ,"","tpcChain","tpc_T"                             ,"StVeloMaker","StVeloMaker","",kFALSE},
  {"TpcHitFilter","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"TpcHitMover" ,"tpc_hit_mover","tpcChain","tpcDb,StEvent"
   ,                  "StTpcHitMover","StTpcHitMoverMaker","TPC hits coord transform + corrections",kFALSE},
  {"tpt","","ITTF","",                          "","","WARNING *** Option is OBSOLETE *** use ITTF",kFALSE},
  {"tpt_old","","ITTF","",                      "","","WARNING *** Option is OBSOLETE *** use ITTF",kFALSE},
  {"TpcT0"  ,"","","",                                   "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ChargeStep","","","",                                "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"laser"  ,"","","",                                   "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"PreVtx"  ,"","","",                                  "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"svt"         ,"svtChain","","svt_T,SvtCL"                               ,"StMaker","StChain","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain","daq,SvtCL"              ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"","","SvtSSim,SvtOnlSeq"         ,"","","Short cut for SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtSSim","SvtSSimu","svtChain","svtCalDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
   ,                               "StSvtSimulationMaker","StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
  {"SvtEmbed"  ,"","","SvtSSim,SvtEm,SvtOnlSeq"      ,"","","Short cutfor SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtEm","SvtEm","svtChain","svtCalDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit","StSvtEmbeddingMaker"
   ,                                                      "StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
  {"SvtOnlSeq"   ,"SvtOnlSeq","svtChain","svtCalDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
   ,                                          "StSvtOnlineSeqAdjSimMaker","StSvtSimulationMaker","",kFALSE},
  {"srs","","","sfs",                    "","","WARNING *** Option is OBSOLETE use sfs instead ***",kFALSE},
  {"sfs","svt_hits","svtChain","svtDb,Simu,SvtCL,-sss,-SvtSlowSim,StEvent,MakeEvent"
   ,                                    "St_sfs_Maker","St_srs_Maker","Very fast simulator for SVT",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"   ,"SvtClu","svtChain","svt_T,StEvent,SvtCL","StSvtClusterMaker","StSvtClusterMaker","",kFALSE},
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},
  {"SvtVtx"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"stk"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Est"         ,"","","SvtIT",       "","","WARNING *** Option is OBSOLETE *** use SvtIT instead",kFALSE},   
  {"global"      ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},   
  {"Match"       ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},   
  {"Vertex"      ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},   
  {"Primary"     ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},   
  {"V0"          ,"","","V02",            "","","WARNING *** Option is OBSOLETE use V02 instead***",kFALSE},   
  {"Xi"          ,"","","Xi2",            "","","WARNING *** Option is OBSOLETE use Xi2 instead***",kFALSE},   
  {"Kink"        ,"","","Kink2",        "","","WARNING *** Option is OBSOLETE use Kink2 instead***",kFALSE},   
  {"dst"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Fglobal"     ,"","","","",""                              ,"WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Fprimary"    ,"","","","",""                              ,"WARNING *** Option is OBSOLETE ***",kFALSE},
  {"dEdx"        ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"svtdEdx"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Event"       ,  "","","MakeEvent",                          "","","Request to initialize event",kFALSE},
  {"pixFastSim","","","StMcEvent,StEvent"
   ,                               "StPixelFastSimMaker","StPixelFastSimMaker","FastPixelSimulator",kFALSE},
  {"ssddat"      ,"","","ssd_daq"                             ,"","","SSD full chain for Real Data",kFALSE},
  {"ssd_daq","","","ssdCalDb,svt_T,-sls,-spa,ssdUtil","StSsdDaqMaker","StSsdDaqMaker","... SSD Daq",kFALSE},
  {"ssdfast"     ,"","","ssdDb,StMcEvent,StEvent","StSsdFastSimMaker","StSsdFastSimMaker",
   "... SSD fast simulator"                                                                        ,kFALSE},
  {"ssd"         ,"","","ssdCalDb,sls,spa,spt"               ,"","","SSD full chain for simulation",kFALSE},
  {"sls","","","McEvent,Simu,svt_T,SvtCL"
   ,                                "St_sls_Maker","StSsdSimulationMaker", "... SSD slow simulator",kFALSE},
  {"spa"         ,"SpaStrip","","Simu,svt_T,SvtCL,ssdUtil","St_spa_Maker","StSsdSimulationMaker"
   ,                                                                 "... SSD Pedestal Annihilator",kFALSE},
  {"SsdEmbed"   ,"","","","StSsdEmbeddingMaker","StSsdSimulationMaker","... SSD Mixing geom Maker" ,kFALSE},
  {"spt"        ,"","","ssdUtil,svt_T", "StSsdPointMaker","StSsdPointMaker","... SSD Point Creator",kFALSE},
  {"ssdpre"      ,"","","ssdEmbed,spa"                    ,"","","SSD full chain for pre-embedding",kFALSE},
  {"ssdAdd"     ,"","","ssd_daq","StSsdAddMaker","StSsdAddMaker",             "... SSD merge maker",kFALSE},
  {"ssdE"        ,"","","ssdpre,ssdAdd"                       ,"","","SSD full chain for embedding",kFALSE},
  {"emcDY2"   ,"emcRaw","emcY2",
   "daq,eemcDb,EEmcUtil,emc_T,EmcUtil,StEvent,PreEcl,Epc","StEmcRawMaker","StEmcRawMaker"
   ,                                                                    "B/E EMC data common maker",kFALSE},
  {"emcAtoE"  ,"","" ,"db,emcDY2","StEmcADCtoEMaker","StEmcADCtoEMaker", "B-EMC ADC to E converter",kFALSE},
  {"eemcD"       ,"","","","","",                              "WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ZDCVtx"      ,"","","db"                              ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},
  {"emcY2"    ,"emcY2","","emc_T,tpc_T,db,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim"   ,"","emcY2","emc_T,EmcUtil,StMcEvent,MuDST","StEmcSimulatorMaker","StEmcSimulatorMaker",
                                                                           "New simulator for BEMC",kFALSE},
  {"EEfs" ,"eefs","","db,EEmcUtil,MuDst",
                                     "StEEmcFastMaker","StEEmcSimulatorMaker","EEMC fast simulator",kFALSE},
  {"EEss" ,"eess","","EEfs",
                                     "StEEmcSlowMaker","StEEmcSimulatorMaker","EEMC slow simulator",kFALSE},
  // BTOF related chains
  //  {"btof"       ,"BTofChain","","btofDat,vpdCalib,btofMatch,btofCalib","StMaker",
  //                                                                           "StChain","BTOF Chain",kFALSE}, 
  {"BtofDat"   ,"tof_raw","BTofChain","db,BTofutil","StBTofHitMaker","StEvent,StBTofHitMaker",
   "BTOF hit maker",kFALSE},
  {"vpdCalib","","BTofChain","db,BTofUtil","StVpdCalibMaker","StVpdCalibMaker",   "VPD calibration",kFALSE}, 
  
  // MTD related chains
  {"MtdDat"   ,"mtd_raw","MtdChain","db,MtdUtil","StMtdHitMaker","StEvent,StMtdHitMaker"
   ,                                                                                "MTD hit maker",kFALSE},
  // Time Of Flight related options
  {"ToF"       ,"TofChain","","tofDat,tofrMatch,tofpMatch,tofCalib","StMaker","StChain","ToF Chain",kFALSE},
  {"ToFx"      ,"TofChain","","tofXDat,tofrMatch,tofCalib"        ,"StMaker","StChain","ToFx Chain",kFALSE},
  {"tofDat"    ,"tof_raw","TofChain","db,Tofutil","StTofMaker","StEvent,StTofMaker",
                                                                              "TOF Data base chain",kFALSE},
  {"tofXDat"   ,"tof_raw","TofChain","db,Tofutil","StTofHitMaker","StEvent,StTofMaker,StTofHitMaker",
                                                                                    "TOF hit maker",kFALSE},
  
  {"tofsim"    ,"","TofChain","TofUtil","StTofSimMaker","StEvent,StTofMaker,StTofSimMaker",
                                                                                    "TOF Simulator",kFALSE},
  
  {"tofrMatch" ,"","TofChain","db,TofUtil","StTofrMatchMaker","StTofrMatchMaker",
                                                                       "TPC to TOFr track matching",kFALSE},
  {"tofpMatch"   ,"","TofChain","db,TofUtil","StTofpMatchMaker","StTofpMatchMaker",
                                                                       "TPC to TOFp track matching",kFALSE},
  {"tofCalib"   ,"","TofChain","db,TofUtil","StTofCalibMaker","StTofCalibMaker",  "TOF calibration",kFALSE},
  // Filtering - all filters will have the pattern "FiltXXX"
  {"FiltGamma" ,"","","StEvent,StMcEvent,EmcUtil",
                                      "StGammaFilterMaker","StFilterMaker",  "BEmc Gamma filtering",kFALSE},
  {"FiltEemcGamma" ,"","","StEvent,StMcEvent,EmcUtil",
                                  "StEemcGammaFilterMaker","StFilterMaker",  "EEmc Gamma filtering",kFALSE},
  
  
  // fms
  {"fmsdat"     ,"","", "StEvent,fmsdb", 
                                     "StFmsHitMaker","StFmsHitMaker","Fill FMS struct and zero TRG",kFALSE},

  // FGT placeholder
  {"fgtdat"     ,"","", "event,fgtutil","StFgtRawMaker","StFgtRawMaker",          "FGT Data reader",kFALSE},

  
  // Some global Sti stuff including vertexing
  {"genvtx"      ,""  ,"","ctf_T,EEmcUtil","StGenericVertexMaker"
   ,"St_ctf,St_ctf_Maker,Minuit,StGenericVertexMakerNoSti",                 "Generic Vertex Finder",kFALSE},
  {"StiPulls" ,"","",""                                         ,"","", "Request to make Sti Pulls",kFALSE},
  {"StvPulls" ,"","",""                                         ,"","", "Request to make Stv Pulls",kFALSE},
  {"StiLib"   ,"","",""                                                        ,"","Sti","Load Sti",kFALSE},
  {"StiCALib" ,"","",""                                                   ,"","StiCA","Load Sti+CA",kFALSE},
  {"StiTpc"   ,"","","TpcDb,ITTF,tpc_T,dbutil,detDb,StarMagField,magF"   ,"","StiTpc","Load StiTpc",kFALSE},
  {"StiSvt"   ,"",""," "                  ,"","StSvtClassLibrary,StSvtDbMaker,StiSvt","Load StiSvt",kFALSE},
  {"StiSsd"   ,"","",""                           ,"","StSsdUtil,StSsdDbMaker,StiSsd","Load StiSvt",kFALSE},
  {"StiLibs"  ,"","","StiTpc,StiSvt,StiSsd"                         ,"","","Load Sti Detector libs",kFALSE},
  {"VFPPV"          ,""  ,"","-genvtx,ctf_T,EEmcUtil,StiLib","StGenericVertexMaker"                      
   ,            "St_ctf,St_ctf_Maker,Minuit,StGenericVertexMaker","... Pile-up proof vertex finder",kFALSE},
  {"VFPPVnoCTB"     ,""  ,"","-genvtx,ctf_T,EEmcUtil,StiLib","StGenericVertexMaker"
   ,     "St_ctf,St_ctf_Maker,Minuit,StGenericVertexMaker","... Pile-up proof vertex finder, noCTB",kFALSE},
  {"Sti"      ,"Sti","","StiLib,StiLibs,SCL,StEvent,StDbT,TpcIT,compend,tbutil","StiMaker"
   ,                                         "StEventUtilities,StiUtilities,StiMaker","Sti tracker",kFALSE},
  {"StiCA"    ,"StiCA","","-Sti,-StiLib,StiCALib,StiLibs,SCL,StEvent,StDbT,TpcIT,compend,tbutil","StiMaker"
   ,                                "StEventUtilities,libEG,StiUtilities,StiMaker","Sti+CA tracker",kFALSE},
  {"Stv"     ,"Stv","","-TpcIT,-SvtIT,-SsdIT,gen_T,sim_T","StvMaker"
   ,"libHist,libHistPainter,libVMC,StarVMCApplication,StarMiniCern,geant3,GeoTestMaker,StvUtil,Stv,StvMaker" 
   ,                                                                                          "Stv",kFALSE},
  {"StvCA"    ,"StvCA","","Stv","",""                                                      ,"StvCA",kFALSE},
  {"StiVMC"   ,"StiVMC","","-Sti,SCL,StEvent,StDbT,TpcDb,compend","StiVMCMaker"
   ,                                      "StEventUtilities,StiVMC,StiVMCMaker" ,"ITTF VMC tracker",kFALSE},
  {"StiVMCLibs","","","detDb,StarMagField","",                      "","ITTF:load StiVMC libraries",kFALSE},
  {"laserIT"  ,"","","","",                              "TpcIT","use Sti for laser reconstruction",kFALSE},
  {"TpcIT"    ,"","","ITTF"                                              ,"","","Sti tracking: TPC",kFALSE},
  {"SvtIT"    ,"","","ITTF"                                              ,"","","Sti tracking: SVT",kFALSE},
  {"SsdIT"    ,"","","ITTF"                                              ,"","","Sti tracking: SSD",kFALSE},
  {"HpdIT"  ,""  ,"","ITTF",""                               ,"Sti,StiRnD","Sti tracking: Hpd geom",kFALSE},
  {"PixelIT",""  ,"","ITTF",""                             ,"Sti,StiRnD","Sti tracking: Pixel geom",kFALSE},
  {"IstIT"  ,""  ,"","ITTF",""                               ,"Sti,StiRnD","Sti tracking: Ist geom",kFALSE},
  {"NoSvtIT"     ,""  ,"","-SvtIT",""                    ,"","ITTF: track with switch off SVT geom",kFALSE},
  {"NoSsdIT"     ,""  ,"","-SsdIT",""                    ,"","ITTF: track with switch off SSD geom",kFALSE},
  {"skip1row"    ,""  ,"","",""                           ,"","ITTF: skip the first pad row in TPC",kFALSE},
  {"StiRnD"   ,"","","",                                  "","StiRnD", "Load StiRnD shared library",kFALSE},
  {"Alignment"   ,"","","",                   "","", "Sti Tpc Alignment, reconstruction per sector",kFALSE},
  {"Sti"      ,"Sti","","SCL,StEvent,Stu,StiLibs,StDbT,TpcIT,StiUtil,compend,tbutil","StiMaker",
                                                                     "Sti,StiMaker" ,"ITTF tracker",kFALSE},
  {"StiPulls" ,"","","Sti",                                      "","", "Request to make Sti Pulls",kFALSE},
  {"BeamBack" ,"","","StEvent","StBeamBackMaker","StBeamBackMaker"
   ,                                                           "Beam background tracker in the TPC",kFALSE},
  {"dEdxY2"       ,"dEdxY2","","tpcDb,StEvent","StdEdxY2Maker","libMinuit,StdEdxY2Maker"
   ,                                                                 "Bichsel method used for dEdx",kFALSE},
  // final TOF combo
  {"btof"       ,"BTofChain","","btofDat,vpdCalib,btofMatch,btofCalib,geant","StMaker"
   ,                                                                         "StChain","BTOF Chain",kFALSE}, 
  {"btofSim"    ,"","BTofChain","BTofUtil","StBTofSimMaker","StEvent,StBTofHitMaker,StBTofSimMaker"
   ,                                                                               "BTOF Simulator",kFALSE},
  {"mtdSim"    ,"","MtdChain","","StMtdSimMaker",           "StEvent,StMtdSimMaker","MTD Simulator",kFALSE},
  // Options in need to be done after the tracker
  // second wave of BTOF options needed after Sti
  {"btofMatch"  ,"","BTofChain","db,BTofUtil","StBTofMatchMaker","StBTofMatchMaker"
   ,                                                                      "TPC-BTOF track matching",kFALSE},
  {"btofCalib"  ,"","BTofChain","db,BTofUtil","StBTofCalibMaker","StBTofCalibMaker"
   ,                                                                             "BTOF calibration",kFALSE},
  
  {"FindVtxSeed"   ,"FindVtxSeed"   ,"","globT,MuDSTDeps","StVertexSeedMaker"
   ,                                   "StPass0CalibMaker",          "Performs vertex seed finding",kFALSE},
  {"FindEvtVtxSeed","FindEvtVtxSeed","","MuDSTDeps","StEvtVtxSeedMaker"
   ,                               "StPass0CalibMaker","Performs vertex seed finding using StEvent",kFALSE},
  {"Ftpc"      ,"ftpcChain"  ,"","ftpcT,fcl,fpt"                            ,"StMaker","StChain","",kFALSE},
  {"fss"       ,"ftpc_raw","ftpcChain","SCL,Simu","StFtpcSlowSimMaker"
   ,                 "StFtpcSlowSimMaker,StFtpcTrackMaker,StFtpcClusterMaker","FTPC Slow simulator",kFALSE},
  {"Fcl"       ,"ftpc_hits","ftpcChain","SCL,daq","StFtpcClusterMaker"
   ,                                    "StFtpcTrackMaker,StFtpcClusterMaker","FTPC cluster finder",kFALSE},
  {"fpt"      ,"ftpc_tracks","ftpcChain","SCL"
   ,                                       "StFtpcTrackMaker","StFtpcTrackMaker","FTPC Track Maker",kFALSE},
  {"fgain"     ,"","","fcl,fpt","",""
   ,                    "StFtpcClusterMaker and StFtpcTrackMaker will produce gain scan histograms",kFALSE},
  {"fdbg"     ,"","","fcl,fpt","","","StFtpcClusterMaker and StFtpcTrackMaker will write debugfile",kFALSE},
  {"flaser"   ,"","","fcl,fpt"   ,"","","StFtpcClusterMaker and StFtpcTrackMaker for LASERTRACKING",kFALSE},
  
  {"pmdReco"   ,"pmdReco","","PmdUtil,pmdRead,pmdClust"       ,"StMaker","StChain","PMD Reco chain",kFALSE},
  {"pmdRaw"    ,"pmdRaw","","pmdReco"                       ,"","","PMD Reco chain giving raw data",kFALSE},
  {"pmd"       ,"pmd","","pmdSim,pmdClust,pmdDis","StMaker"      ,"StChain", "PMD Simulation chain",kFALSE},
  {"pmdRead"   ,"","","PmdUtil","StPmdReadMaker"            ,"StPmdReadMaker", "DAQ reader for PMD",kFALSE},
  {"pmdSim"    ,"","","PmdUtil","StPmdSimulatorMaker","StPmdSimulatorMaker","Hit Simulator for PMD",kFALSE},
  {"pmdClust"  ,"pmdClust","","","StPmdClusterMaker",    "StPmdClusterMaker","ClusterMaker for PMD",kFALSE},
  {"pmdDis"    ,"pmdDis","PmdClust","","StPmdDiscriminatorMaker"
   ,                                              "StPmdDiscriminatorMaker","Discriminator for PMD",kFALSE},
#ifndef __NoStrangeMuDst__
  {"Kink2"       ,"kink2","","db,MuDST,-kink","StKinkMaker","StSecondaryVertexMaker"
   ,                                                                      "Find Kinks from StEvent",kFALSE},
  {"V02"         ,"v02","","db,MuDST,-V0","StV0FinderMaker","StSecondaryVertexMaker"
   ,                                                                        "Find V0s from StEvent",kFALSE},
  {"Xi2"         ,"xi2","","db,MuDST,-V02,-Xi","StXiFinderMaker","StSecondaryVertexMaker"
   ,                                                                     "Xis AND V0s from StEvent",kFALSE},
#else /* ! __NoStrangeMuDst__ */
  {"Kink2"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"V02"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Xi2"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif /* __NoStrangeMuDst__ */
  {"V0svt"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Xisvt"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"SCEbyE"      ,"scebye","","MuDSTDeps","StSpaceChargeEbyEMaker","StMuDSTMaker,StPass0CalibMaker"
   ,                                                     "Determine EbyE SpaceCharge using StEvent",kFALSE},
  {"SCScalerCal" ,"scscalercal","","MuDSTDeps","StSpaceChargeEbyEMaker","StMuDSTMaker,StPass0CalibMaker"
   ,                                                                "Calibrate SpaceCharge scalers",kFALSE},
  {"PostEmc"     ,"PostChain","","emc_T,tpc_T,db,PreEcl,EmcUtil"            ,"StMaker","StChain","",kFALSE},
  {"PreEcl"      ,"preecl","PostChain","" ,"StPreEclMaker",  "StPreEclMaker","B-EMC Cluster finder",kFALSE},
  {"Epc"         ,"epc","PostChain","PreEcl,EmcUtil" ,"StEpcMaker","StEpcMaker","B-EMC point maker",kFALSE},
  {"fpd"         ,"fpd","","",                  "StFpdMaker","StFpdMaker","FPD/BBC Data base chain",kFALSE}, // yf
  {"rich"        ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Rrs"         ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"rch"         ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"RichPiD"     ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"l3"          ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"l3cl"        ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"l3t"         ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"l3onl"       ,"","",""                            ,"Stl3RawReaderMaker","Stl3RawReaderMaker","",kFALSE},
  {"l3count"     ,"","",""                              ,"Stl3CounterMaker","Stl3RawReaderMaker","",kFALSE},
  {"bbcSim"         ,"","","db","StBbcSimulationMaker"      ,"StBbcSimulationMaker","BBC Simulator",kFALSE},
  {"analysis"    ,"","","StEvent"        ,"StAnalysisMaker","StAnalysisMaker","Example of Analysis",kFALSE},
  {"compend"     ,"","","event,detDb","StEventCompendiumMaker","StEventCompendiumMaker"
   ,                                                             "Fill event summary in ITTF Chain",kFALSE},
  {"TpcAligner"    ,"","","Alignment"         ,"StTpcAlignerMaker","StTpcAlignerMaker","TpcAligner",kFALSE},
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra"         ,"","",""                      ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Flow"        ,"","TagsChain","StEvent,Stu"                      ,"StFlowMaker","StFlowMaker","",kFALSE},
  {"FlowTag"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"FlowAnalysis","","TagsChain","StEvent,Flow"     ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
#ifndef __NoStrangeMuDst__
  {"StrangeTags" ,"","TagsChain","StEvent"            ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
#endif
  {"SpectraTag"  ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"HeavyTags"   ,"","TagsChain","StEVent"                  ,"StHeavyTagMaker","StHeavyTagMaker","",kFALSE},
  {"EbyeScaTags" ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"HighPtTags"  ,"","TagsChain","StEVent"              ,"StHighPtTagsMaker","StHighPtTagsMaker","",kFALSE},
  {"PCollTag"    ,"","",""                              ,"","","WARNING *** Option is OBSOLETE ***",kFALSE},
#ifndef __NoStrangeMuDst__
  {"tags"        ,"","TagsChain",      "globT,Event,StrangeTags,HeavyTags,PCollTag,HighPtTags"
   ,                                        "StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"noTags"      ,"","","-tags,-StrangeTags,-HeavyTags,-PCollTag,-HighPtTags","","","Turn Off tags",kFALSE},
#else /* __NoStrangeMuDst__ */
  {"tags"        ,"","TagsChain","globT,Event,HeavyTags,HighPtTags"
   ,                                        "StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"noTags"      ,"","","-tags,-HeavyTags,-HighPtTags"                       ,"","","Turn Off tags",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"MuDSTChain","MuDSTChain","EMCmDST,CMuDST",""                            ,"StMaker","StChain","",kFALSE},
#ifndef __NoStrangeMuDst__
  {"StrngMuDST","","MuDSTDeps","",              "StStrangeMuDstMaker","","Creates Stangeness MuDST",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"EMCmDST"   ,"","MuDSTChain","MuDst",                "StEmcMicroDstMaker","","Creates EMC MuDST",kFALSE},
#ifndef __NoStrangeMuDst__
  {"CMuDST"    ,"","MuDSTChain","MuDst,StrngMuDST,Tree",    "StMuDstMaker","","Writes Common MuDST",kFALSE},
#else /* ! __NoStrangeMuDst__ */
  {"CMuDST"    ,"","MuDSTChain","MuDst,Tree",               "StMuDstMaker","","Writes Common MuDST",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
#ifndef __NoDisplay__
  {"Display"     ,"","","TbUtil,St_geom"
   ,           "StEventDisplayMaker","StEvent,StEventUtilities,StEventDisplayMaker","Event Display",kFALSE},
#else /* ! __NoDisplay__ */
  {"Display"     ,"","","",       "","","WARNING *** Option is OBSOLETE *** use Ed.C macro instead",kFALSE},
#endif /* __NoDisplay__ */
  {"Mc"          ,"McChain","McEvent","sim_T,globT,McAss,McAna"             ,"StMaker","StChain","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAnaTpc"    ,"","","McAna"                                        ,"","","Mc Analysis for Tpc",kFALSE},
  {"McAnaSvt"    ,"","","McAna"                                        ,"","","Mc Analysis for Svt",kFALSE},
  {"McAnaSsd"    ,"","","McAna"                                        ,"","","Mc Analysis for Ssd",kFALSE},
  {"McAna"       ,"","McChain","McEvent,McAss",          "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"McQa"        ,"","McChain","McEvent",  "StMcQaMaker","StMcQaMaker","QA histogramms for McEvent",kFALSE},
  {"McTpcAna"    ,"","McAnaChain","tpcDb,dbutil,StMcEvent,StEvent"
   ,                                               "StTpcMcAnalysisMaker","StTpcMcAnalysisMaker","",kFALSE},
  {"MiniMcEvent" ,"","","","",                   "StMiniMcEvent","Loads StMiniMcEvent library only",kFALSE},
  {"MiniMcMk"    ,"","","McAss,MiniMcEvent","StMiniMcMaker","StMiniMcMaker"
   ,                                                             "Creates tree in minimc.root file",kFALSE},
  {"SvtMatTree","","","","SvtMatchedTree"
   ,                              "StSvtPoolEventT,StSvtPoolSvtMatchedTree","Create SvtMatchedTree",kFALSE},
  {"LAna"        ,"","","in,detDb,StEvent,tpcDb","StLaserAnalysisMaker"
   ,                                                   "StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  {"SpinTag"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPfind1"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"SpinSortA"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPprojectA","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppDAQfilter1","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPeval1"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"QA"     ,"","",                                   "","","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"EventQA","EventQA","","QUtils,Event","StEventQAMaker"   ,"St_QA_Maker","Filling Y2/Y3 Qa histo",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"QAalltrigs"  ,"", "","",                                     "","","Analyze all triggers in QA",kFALSE},
  {"HitFilt"     ,"", "","",               "StHitFilterMaker","StHitFilterMaker","Hit filter Maker",kFALSE},
  {"KeepTpcHit"  ,"", "","",                          "","","Keep all TPC hits in StHitFilterMaker",kFALSE},
  {"KeepSvtHit"  ,"", "","",                          "","","Keep all SVT hits in StHitFilterMaker",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"logger"      ,""  ,"",""            ,"","","Use log4cxx package to manage the program messages",kFALSE},
  {"NoSimuDb"    ,""  ,"",""                                 ,"","","Switch off Simu Option for DB",kFALSE},
  {"SimuDb"      ,""  ,"","-NoSimuDb"                         ,"","","Switch on Simu Option for DB",kFALSE},
  {"NoOutput"    ,""  ,"","-Tree,-EvOut,noTags"                 ,"","","Suppress Output root files",kFALSE},
  {"EvOutOnly"   ,""  ,"","EvOut,Tree,noTags"                        ,"","","Only event.root files",kFALSE},
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE},
  {"Notpc_daq"   ,""  ,"","-tpc_daq"                                            ,"","","No tpc_daq",kFALSE},
  
  
  {"Calibration chains","------------","-----------","-----------------------------------","","","",kFALSE},
  {"LanaDV","","","MakeEvent,trgd,in,tpc_daq,tpcI,fcf,LaserIT,VFMinuit,Lana,Analysis,Corr4,NosvtIT,NossdIT",
   "",""                                                                                 ,"get LDV",kFALSE},
  {"LanaDVtpx","","","MakeEvent,trgd,in,tpx,TpcHitMover,LaserIT,VFMinuit,Lana,Analysis,Corr4,NosvtIT,NossdIT",
   "",""                                                                        ,"get LDV with TPX",kFALSE},
  {"LaserDV.Chain","","","in,LaserCal,fcf,TpcHitMover,OGridLeak3D,OShortR,OSpaceZ2","","","get LDV",kFALSE},
  
  
  
  {"Production chain from Db","------------","-----------","-----------------------------","","","",kFALSE},
  {"ppITTFsvt.Chain" ,"" ,"","DbV20020802,pp2001,fpd,beamLine,svt_daq,SvtD,est,svtdedx,CMuDst"
   ,"","",""                                                                                       ,kFALSE},
  {"ittfMiniMc.Chain" ,"" ,"","RY2005,in,event,svtDb,ITTF,SvtIT,evOut,InTree,ReadAll,Simu,MiniMcMk"
   ,"","",""                                                                                       ,kFALSE},
  {"dau.ittf.prod.Chain" ,"" ,"","DbV20040520,dau2003i,ITTF,SvtIT,hitfilt,in"	          ,"","","",kFALSE},
  {"ppITTFpyth.Chain" ,"" ,"","trs,Simu,srs,fss,y2004,Idst,tpcI,tcl,ftpc,l0,ctf,Tree,SvtCL,svtDb,ITTF,"
   "genvtx,dEdxY2,geant,tags,bbcSim,tofsim,emcY2,"                                        ,"","","",kFALSE},
  {"auau200run2004.ITTF.Chain" ,"" ,"","P2004,DbV20040917,SCEbyE,ITTF,pmdRaw,hitfilt"	  ,"","","",kFALSE},
  {"pp200run2004.ITTF.Chain" ,"" ,"","pp2004,DbV20040917,beamLine,ITTF,hitfilt"	          ,"","","",kFALSE},
  {"auau200y2004.ITTF.Chain" ,"" ,"","P2004,DbV20040804,SCEbyE,ITTF,pmdRaw,hitfilt"       ,"","","",kFALSE},
  {"pp.2005prod.ITTF.Chain","","","DbV20050816,pp2005a,ITTF,OSpaceZ2,OGridLeak3D,hitfilt" ,"","","",kFALSE},
  {"pp.pythia2.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2004y,Idst,l0,ctf,tpcI,tcl,ftpc,Tree,logger,ITTF,"
   "VFPPV,bbcSim,tofsim,tags,emcY2,EEfs,evout,geantout,big,fzin,MiniMcMk,eemcDb,beamLine,"
   "sdt20050727"                	                                                  ,"","","",kFALSE},
  {"cucu.ITTF.noSvt.Chain" ,"" ,"",   "P2005,DbV20060421,useCDV,ITTF,tofDat,-svtIT,"
   "SCEbyE,OGridLeak,OShortR,OSpaceZ2,hitfilt"                                            ,"","","",kFALSE},
  {"cucu62.D0.nosvt.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,l0,ctf,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,VFMCE,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,fzin,"
   "MiniMcMk"          	                                                                  ,"","","",kFALSE},
  {"cucu.ITTF.Chain" ,"" ,"","P2005,DbV20060524,useCDV,MakeEvent,ITTF,tofDat,ssddat,spt,SsdIt,"
   "SCEbyE,OGridLeak3D,OShortR,OSpaceZ2,KeepSvtHit,hitfilt"                               ,"","","",kFALSE},
  {"cucu62.D0.prod.ITTF.Chain" ,"" ,"","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,l0,ctf,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"      	                                                          ,"","","",kFALSE},
  {"cucu200.D0.prod.ITTF.Chain" ,"" ,"","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,l0,ctf,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                                  ,"","","",kFALSE},
  {"cucu200.D0.nosvt.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"      	                                                          ,"","","",kFALSE},
  {"cucu.ITTF.test2.Chain","" ,"","P2005,DbV20060620,useCDV,MakeEvent,ITTF,tofDat,ssddat,spt,SsdIt,"
   "SCEbyE,OGridLeak3D,OShortR,OSpaceZ2,KeepSvtHit,hitfilt"                               ,"","","",kFALSE},
  {"cucu200.D0.prod2.ITTF.Chain","","","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                                  ,"","","",kFALSE},
  {"cucu200.D0.nosvt.prod2.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                                  ,"","","",kFALSE},
  {"ppJpsi.prod2006.ITTF.Chain","","","DbV20060729,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt",
   "","",""                                                                                        ,kFALSE},
  {"pp200.Upmix.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,tcl,ftpc,Tree,logger,"
   "ITTF,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk,beamLine"                                                               ,"","","",kFALSE},
  {"pp2006prod.ITTF.Chain" ,"" ,"","DbV20060915,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt","","","",kFALSE},
  {"pp2006prod2.ITTF.Chain","" ,"","DbV20061021,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt","","","",kFALSE},
  {"auauUpgr05.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr05,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,"
   "IdTruth,tags,bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                     ,"","","",kFALSE},
  {"auauUpgr01.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr01,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr07.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr07,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"pp2006prod3.ITTF.Chain" ,"" ,"","DbV20060915,pp2006b,ITTF,hitfilt"	                  ,"","","",kFALSE},
  {"auauUpgr08.prod.ITTF.Chain" ,"" ,"","trs,Simu,upgr08,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr06.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr06,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,HpdIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr09.prod.ITTF.Chain" ,"" ,"","trs,Simu,upgr09,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr10.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr10,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr11.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr11,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                                  ,"","","",kFALSE},
  {"auauUpgr06.prod2.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr06,pixFastSim,Idst,IAna,tpcI,tcl,Tree,logger,"
   "ITTF,StiRnd,PixelIT,HpdIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut" ,"" ,"",""                                ,kFALSE},
  {"Test.ITTF","","","svtIT,ssdIT,ITTF,event,analysis,EventQA,tags,Tree,EvOut,StarMagField,FieldOn"
   ",IDST,CMuDst,Tree,analysis"                                                           ,"","","",kFALSE},
  {"Test.reco.ITTF","","","MakeEvent,tpcI,tcl,ftpc,SvtCL,Test.ITTF"                       ,"","","",kFALSE},
  {"Test.fast.ITTF","","","tfs,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,SvtCL,"
   "tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,ITTF,Idst,event,analysis,EventQA,tags,Tree,logger,EvOut,"
   "StarMagField,FieldOn,IAna,CMuDst"                                                     ,"","","",kFALSE},
  {"Test.VeryFast.ITTF","","","TpcFastSim,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,"
   "SvtCL,tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,ITTF,Idst,event,analysis,EventQA,tags,Tree,logger,"
   "EvOut,StarMagField,FieldOn,IAna,CMuDst"                                               ,"","","",kFALSE},
  {"Test.default.ITTF","","","TpcRS,Simu,sss,svt,ssd,fss,bbcSim,IdTruth,MakeEvent,genvtx,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"Test.default.y2005g.ITTF","","","Test.default.ITTF,sdt20050130,noSimuDb"              ,"","","",kFALSE},
  {"Test.default.y2007g.ITTF","","","Test.default.ITTF,sdt20070322,noSimuDb"              ,"","","",kFALSE},
  {"Test.fast.y2005g.ITTF","","","Test.fast.ITTF,sdt20050130,noSimuDb"                    ,"","","",kFALSE},
  
  {"Test.reco.StiVMC","","","MakeEvent,tpcI,tcl,ftpc,SvtCL,Test.StiVMC"                   ,"","","",kFALSE},
  {"Test.default.StiVMC","","","TpcRS,Simu,sss,svt,ssd,fss,bbcSim,IdTruth,MakeEvent,"
   "miniMcMk,McAna,Test.reco.StiVMC,CMuDst"                                               ,"","","",kFALSE},
  {"Test.StiVMC","","","TpcRS,StiVMC,event,analysis,EventQA,tags,EvOut,StarMagField,FieldOn"
   ",IDST,CMuDst,Tree,logger,analysis"                                                    ,"","","",kFALSE},
  {"Test.VeryFast.StiVMC","","","TpcFastSim,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,"
   "SvtCL,tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,StiVMC,Idst,event,analysis,EventQA,tags,Tree,logger,"
   "EvOut,StarMagField,FieldOn,IAna,CMuDst"                                               ,"","","",kFALSE},
  {"Test.fast.StiVMC","","","tfs,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,SvtCL,"
   "tpc_T,globT,tls,db,tpcDB,svtDb,StiVMC,Idst,event,analysis,EventQA,tags,Tree,logger,EvOut,"
   "StarMagField,FieldOn,IAna,CMuDst"                                                     ,"","","",kFALSE},
  {"Test.fast.y2007g.ITTF","","","Test.fast.ITTF,sdt20070322,noSimuDb"                    ,"","","",kFALSE},
  {"Test.VeryFast.y2005g.ITTF","","","Test.VeryFast.ITTF,sdt20050130,noSimuDb"            ,"","","",kFALSE},
  {"Test.VeryFast.y2007g.ITTF","","","Test.VeryFast.ITTF,sdt20070322,noSimuDb"            ,"","","",kFALSE},
  {"Test.default.StiVMC","","","TpcRS,Simu,sss,svt,ssd,fss,bbcSim,IdTruth,MakeEvent,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"Test.y2009.ITTF","","","Test.default.ITTF,y2009,TpcRS,sdt20090428.141700"             ,"","","",kFALSE},
  {"Test.y2009.StiVMC","","","Test.default.StiVMC,y2009,TpcRS,sdt20090428.141700,noSimuDb","","","",kFALSE},
  {"Test.fast.y2005g.StiVMC","","","Test.fast.StiVMC,sdt20050130,noSimuDb"                ,"","","",kFALSE},
  {"Test.VeryFast.y2005g.StiVMC","","","Test.VeryFast.StiVMC,sdt20050130,noSimuDb"        ,"","","",kFALSE},
  {"Test.VeryFast.y2007g.StiVMC","","","Test.VeryFast.StiVMC,sdt20070322,noSimuDb"        ,"","","",kFALSE},
  {"Test.default.Fast.ITTF","","","tfs,sfs,ssdFast,IdTruth,MakeEvent,miniMcMk,Test.ITTF",
   ""                                                                                        ,"","",kFALSE},
  {"Test.srs.ITTF","","",   "TpcRS,Simu,srs,svt,ssd,fss,bbcSim,emcY2,McEvOut,GeantOut,IdTruth,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"nightly test (dev) chains","-----------","-----------","-----------------------------","","","",kFALSE},
  // Data test chains
  {"Test.year_2000_central","","","p2000,VFMinuit,logger,CMuDst,NosvtIT,NossdIT","","",
   "/star/rcf/test/daq/2000/09/st_physics_1248022_raw_0001.daq",                                    kFALSE},
  {"Test.year_2000_minbias","","","p2000,VFMinuit,logger,CMuDst,NosvtIT,NossdIT","","",
   "/star/rcf/test/daq/2000/08/st_physics_1229021_raw_0003.daq",                                    kFALSE},
  {"Test.year_2001_central","","","P2001a,VFMinuit,v02,xi2,ZDCvtx,logger,CMuDst,NosvtIT,NossdIT","","",
   "/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq",                                   kFALSE},
  {"Test.year_2001_minbias","","","P2001a,VFMinuit,v02,xi2,ZDCvtx,logger,CMuDst,NosvtIT,NossdIT","","",
   "/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq",                                   kFALSE},
  {"Test.year_2001_ppMinBias","","","pp2001a,genvtx,v02,xi2,fpd,logger,CMuDst,NossdIT","","",
   "/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq",                                   kFALSE},
  {"Test.year_2003_dAuMinBias","","","DbV20040520,dau2003i,logger,ITTF,in,-SvtIT,NossdIT","","",
   "/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",                                kFALSE},
  {"Test.year_2003_ppMinBias","","","pp2003,VFPPV,v0,xi,l3onl,beamLine,logger,CMuDst,-svtIT,NossdIT,Corr2"
   ,"","","/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq",                         kFALSE},
  {"Test.year_2004_AuAu_prodHigh","","","P2004,DbV20041213,ITTF,pmdRaw,logger,clearmem,-SvtIT,-SsdIT","","",
   "/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",                                kFALSE},
  {"Test.year_2004_AuAu_prodLow","","","P2004,DbV20041213,ITTF,-SvtIT,-SsdIT,pmdRaw,logger,clearmem","","",
   "/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",                                kFALSE},
  {"Test.year_2004_AuAuMinBias","","","P2004,DbV20041213,ITTF,-SsdIt,-SvtIt,pmdRaw,logger,clearmem,","","",
   "/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",                                kFALSE},
  {"Test.year_2004_prodPP","","","pp2004,DbV20041213,beamLine,ITTF,logger","","",
   "/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",                                kFALSE},
  {"Test.year_2005_CuCu200_HighTower","","","P2005,tofDat,logger,MakeEvent,ITTF,ssddat,spt,SsdIt,SvtIt,"
   "pmdRaw,OShortR,OSpaceZ2","","","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq",kFALSE},
  {"Test.year_2005_CuCu200_MinBias","","","P2005,tofDat,logger,MakeEvent,ITTF,ssddat,spt,SsdIt,SvtIt,"
   "pmdRaw,OShortR,OSpaceZ2","","","/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq",kFALSE},
  {"Test.year_2005_CuCu22_MinBias","","","P2005,tofDat,logger,MakeEvent,ITTF,ssddat,spt,SsdIt,SvtIt,pmdRaw,"
   "OShortR,OSpaceZ2","","","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq",       kFALSE},
  {"Test.year_2005_CuCu62_MinBias","","","P2005,tofDat,logger,MakeEvent,ITTF,ssddat,spt,SsdIt,SvtIt,pmdRaw,"
   "OShortR,OSpaceZ2","","","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq",       kFALSE},
  {"Test.year_2005_ppProduction","","","pp2005a,tofdat,ITTF,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2005/171/st_physics_6171062_raw_2040010.daq",                                kFALSE},
  {"Test.year_2006_ppProdLong","","","pp2006b,ITTF,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2006/155/7155010/st_physics_7155010_raw_1020003.daq",                        kFALSE},
  {"Test.year_2006_ppProdTrans","","","pp2006b,ITTF,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2006/129/7129023/st_physics_7129023_raw_1020003.daq",                        kFALSE},
  {"Test.year_2007_2007ProductionMinBias","","","DbV20080418,B2007g,ITTF,IAna,KeepSvtHit,hitfilt,VFMinuit3,"
   "l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,pmdReco,Corr4,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",                        kFALSE},
  {"Test.year_2008_ppProduction2008","","","DbV20080712,pp2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine","","",
   "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",                                kFALSE},
  {"Test.year_2008_production_dAu2008","","","DbV20080712,P2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine","","",
   "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",                                kFALSE},
  {"Test.year_2009_production2009_200Gev_Hi","","","pp2009c,ITTF,VFPPVnoCTB,beamLine,BEmcChkStat,btofDat,"
   "Corr4,OSpaceZ2,OGridLeak3D","","","/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq"
   ,                                                                                                kFALSE},
  {"Test.year_2009_production2009_500GeV","","","pp2009c,ITTF,VFPPVnoCTB,beamLine,BEmcChkStat,btofDat,"
   "Corr4,OSpaceZ2,OGridLeak3D","","","/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq"
   ,                                                                                                kFALSE},
  {"Test.year_2010_auau11_production","","","P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",                         kFALSE},
  {"Test.year_2010_auau200_production","","","P2010a,btof,BEmcChkStat,pmdReco,Corr4,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",                         kFALSE},
  {"Test.year_2010_auau39_production","","","P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",                         kFALSE},
  {"Test.year_2010_auau62_production","","","P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",                         kFALSE},
  {"Test.year_2010_auau7_production","","","P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2010/138/st_physics_11138001_raw_2020001.daq",                         kFALSE},
  // MC chains
  {"Test.MC.year_2000_hc_standard","","","trs,TpxClu,rrs,y1h,Idst,rich,IAna,l0,tpcI,fcf,Tree,logger,ITTF,"
   "NosvtIT,NossdIT,-Kink2,genvtx,geant,evout,IdTruth,tags,GeantOut,big,fzin,MiniMcMk","","","/star/rcf/"
   "simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz"
   ,                                                                                                kFALSE},
  {"Test.MC.year_2001_hc_standard","","","trs,TpxClu,fss,rrs,y2001n,Idst,rich,IAna,l0,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,-Kink2,genvtx,geant,evout,IdTruth,tags,emcY2,GeantOut,big,fzin,MiniMcMk","","",
   "/star/rcf/simu/cocktail/hadronic/default/standard/year2001/hadronic_on/Gstardata/hc_standard.40_evts.fz"
   ,                                                                                                kFALSE},
  {"Test.MC.year_2001_pp_minbias","","","trs,TpxClu,rrs,fss,y2001n,Idst,rich,IAna,l0,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,-Kink2,genvtx,geant,evout,IdTruth,tags,emcY2,MiniMcMk,GeantOut,logger,big,fzin","","",
   "/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/gstardata/pds0200_04_12812evts.fzd"
   ,                                                                                                kFALSE},
  {"Test.MC.year_2003_dau_minbias","","","trs,TpxClu,fss,y2003,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,ITTF,"
   "genvtx,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,fzin,MiniMcMk,logger,clearmem","","",
   "/star/rcf/simu/rcf1197_05_5940evts.fzd",                                                        kFALSE},
  {"Test.MC.year_2004_auau_central","","","trs,TpxClu,srs,fss,y2004a,Idst,BAna,l0,tpcI,fcf,ftpc,Tree,logger,ITTF,"
   "Sti,genvtx,SvtIT,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big,fzin,MiniMcMk,clearmem","","",
   "/star/rcf/simu/rcf1209_05_80evts.fzd",                                                          kFALSE},
  {"Test.MC.year_2004_auau_minbias","","","trs,TpxClu,srs,fss,y2004,Idst,BAna,l0,tpcI,fcf,ftpc,Tree,ITTF,"
   "genvtx,SvtIt,geant,evout,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,logger,fzin,MiniMcMk,clearmem",
   "","","/star/rcf/simu/rcf1207_01_225evts.fzd",                                                   kFALSE},
  {"Test.MC.year_2005_cucu200_minbias","","","trs,TpxClu,srs,fss,ssd,y2005x,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,SvtCL,"
   "svtDb,logger,ITTF,SsdIt,SvtIt,genvtx,geant,evout,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,fzin,"
   "MiniMcMk,clearmem","","","/star/rcf/simu/rcf1216_05_200evts.fzd",                               kFALSE},
  {"Test.MC.year_2005_cucu62_minbias","","","trs,TpxClu,srs,fss,ssd,y2005x,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,SvtCL,"
   "svtDb,logger,ITTF,SsdIt,SvtIt,genvtx,geant,evout,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,fzin,"
   "MiniMcMk,clearmem","","","/star/rcf/simu/rcf1237_01_500evts.fzd",                               kFALSE},
  {"Test.MC.year_2006_pp200_minbias","","","trs,TpxClu,fss,y2006h,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,ITTF,"
   "genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,fzin,"
   "MiniMcMk,clearmem","","","/star/rcf/simu/rcf9991_01_1000evts.fzd",                              kFALSE},
  {"Test.MC.year_2007_auau200_central","","","trs,TpxClu,srs,ssd,fss,y2007,Idst,BAna,l0,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,SvtIt,SsdIt,genvtx,MakeEvent,IdTruth,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big,"
   "fzin,MiniMcMk,clearmem","","","/star/rcf/simu/rcf1296_02_100evts.fzd",                          kFALSE},
  {"Test.MC.year_2008_dau200_minbias","","","trs,TpxClu,fss,y2008a,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,ITTF,"
   "Sti,genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,fzin,"
   "MiniMcMk,clearmem","","",   "/star/rcf/simu/rcf9066_20_1000evts.fzd",                           kFALSE},
  {"Test.MC.year_2008_pp200_minbias","","","trs,TpxClu,fss,y2008a,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,ITTF,"
   "genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,fzin,"
   "MiniMcMk,clearmem","","","/star/rcf/simu/rcf9992_01_1000evts.fzd",                              kFALSE},
  {"Test.MC.year_2009_pp200_minbias","","","tpcRS,TpxClu,fss,y2009a,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,"
   "fzin,MiniMcMk,clearmem","",""
   ,"/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2009/pp200_minbias/rcf99001_10_500evts.fzd",        kFALSE},
  {"Test.MC.year_2009_pp500_minbias","","","tpcRS,TpxClu,fss,y2009a,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,"
   "fzin,MiniMcMk,clearmem","",""
   ,"/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2009/pp500_minbias/rcf99002_10_500evts.fzd",        kFALSE},
  {"Test.MC.year_2010_auau11_minbias","","","tpcRS,y2010a,MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,BAna,l0,Tree,"
   "logger,genvtx,tpcDB,TpcHitMover,TpxClu,bbcSim,btofsim,tags,emcY2,EEfs,evout,-dstout,IdTruth,"
   "geantout,big,fzin,MiniMcMk,clearmem","",""
   ,"/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2010/auau11_minbias/rcf10031_1_100evts.fzd",        kFALSE},
  {"Test.MC.year_2010_auau200_minbias","","","tpcRS,y2010,MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,BAna,l0,Tree,"
   "logger,genvtx,tpcDB,TpcHitMover,TpxClu,bbcSim,btofsim,tags,emcY2,EEfs,evout,-dstout,IdTruth,"
   "geantout,big,fzin,MiniMcMk,clearmem","","","/star/rcf/simu/rcf9068_305_100evts.fzd",            kFALSE},
  {"Test.MC.year_2010_auau39_minbias","","","tpcRS,y2010a,MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,BAna,l0,Tree,"
   "logger,genvtx,tpcDB,TpcHitMover,TpxClu,bbcSim,btofsim,tags,emcY2,EEfs,evout,-dstout,IdTruth,"
   "geantout,big,fzin,MiniMcMk,clearmem","","",
   "/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2010/auau39_minbias/rcf10032_1_100evts.fzd",         kFALSE},
  {"Test.MC.year_2010_auau62_minbias","","","tpcRS,y2010a,MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,BAna,l0,Tree,"
   "logger,genvtx,tpcDB,TpcHitMover,TpxClu,bbcSim,btofsim,tags,emcY2,EEfs,evout,-dstout,IdTruth,"
   "geantout,big,fzin,MiniMcMk,clearmem","","",
   "/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2010/auau62_minbias/rcf10033_1_100evts.fzd",         kFALSE},
  {"Test.MC.year_2010_auau7_minbias","","","tpcRS,y2010a,MakeEvent,ITTF,NoSsdIt,NoSvtIt,Idst,BAna,l0,Tree,"
   "logger,genvtx,tpcDB,TpcHitMover,TpxClu,bbcSim,btofsim,tags,emcY2,EEfs,evout,-dstout,IdTruth,"
   "geantout,big,fzin,MiniMcMk,clearmem","","",
   "/star/rcf/test/dev/trs_sl302.ittf/Sun/year_2010/auau7_minbias/rcf10030_1_100evts.fzd",          kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Aliased     ","time stamps","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE}
};
#endif /* __BigFullChain_h__ */
