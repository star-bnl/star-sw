//#define __CLEANUP__ not yet ready
#ifndef __BFC2__
Bfc_st BFC[] = { // standard chains
#else /* __BFC2__ */
Bfc_st BFC2[] = { // ITTF Chains
#endif /* __BFC2__ */
  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"TIME STAMPS ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  // geometry timestamps are now dynamic. Please see StChain/StMaker    
  {"SD97"  ,"","","db,detDb"                                  ,"","","Turn on 1997 test parameters",kFALSE},
  {"SD98"  ,"","","db,detDb"                                  ,"","","Turn on 1998 test parameters",kFALSE},
  {"Y1a"   ,"","","db,detDb"                 ,"","","YEAR_1A  approximation to year1: TPC+CTB+FTPC",kFALSE},
  {"Y1b"   ,"","","db,detDb"                 ,"","","YEAR_1B: TPC+CTB+FTPC+calo patch+RICH, no svt",kFALSE},
  {"Y1s"   ,"","","db,detDb"                ,"","","YEAR_1S  started in summer: TPC, CONE, AL pipe",kFALSE},
  {"Y1d"   ,"","","db,detDb"      ,"","","YEAR_1D  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"   ,"","","db,detDb"      ,"","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"Y1e"   ,"","","db,detDb"      ,"","","YEAR_1E  even better y1:TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"ES99"  ,"","","db,detDb"            ,"","","Turn on 1999 engineering run simulation parameters",kFALSE},
  {"ER99"  ,"","","db,detDb"             ,"","","Turn on 1999 engineering run real data parameters",kFALSE},
  {"DC99"  ,"","","db,detDb"         ,"","","Turn on December 1999 engineering run real parameters",kFALSE},
  {"Y1h"   ,"","","db,detDb",    "","","YEAR_1H fantastic y1:TPC+CTB+FTPC+RICH+caloPatch+svtLadder",kFALSE},
  {"year2000" ,"","","Y2000"                                                              ,"","","",kFALSE},
  {"year2001" ,"","","Y2001"                                                              ,"","","",kFALSE},
  {"year2002" ,"","","db,detDb"                                                           ,"","","",kFALSE},
  {"year2003" ,"","","Y2003"                                                              ,"","","",kFALSE},
  {"Y2000" ,"","","db,detDb"                ,"","","actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder",kFALSE},
  {"RY1h"  ,"","","db,detDb,VtxOffSet"                      ,"","","Real data with Year1h geometry",kFALSE},
  {"RY2000","","","db,detDb,VtxOffSet"      ,"","","actual 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2000a","","","db,detDb"            ,"","","alternate 2000: Real data with Year2000 geometry ",kFALSE},
  {"RY2001","","","db,detDb"                ,"","","actual 2001: Real data with Year2001 geometry ",kFALSE},
  {"RY2003","","","db,detDb"                   ,"","","actual 2003: Real data with Year3 geometry ",kFALSE},
  {"RY2003a","","","db,detDb"                         ,"","","Real data with Year3 study geometry ",kFALSE},
  {"RY2003b","","","db,detDb"                         ,"","","Real data with Year3 study geometry ",kFALSE},
  {"RY2003c","","","db,detDb"                ,"","","Real data with Year3 study geometry, new SVT ",kFALSE},
  {"RY2003X","","","db,detDb"           ,"","","tempative 2003: Real data with Year4 trigger study",kFALSE},
  {"RY2004" ,"","","db,detDb"                          ,"","","Real data with Year4 study geometry",kFALSE},
  {"RY2004a","","","db,detDb"                          ,"","","Real data with Year4 study geometry",kFALSE},
  {"RY2004b","","","db,detDb"                          ,"","","Real data with Year4 study geometry",kFALSE},
  {"RY2004X","","","db,detDb"                             ,"","","Year4 full barrel study geometry",kFALSE},
  {"RY2004Y","","","db,detDb"  ,"","","Year4 full Barrel+TPC backplane+rad length+SVT copper cable",kFALSE},
  {"RY2004c","","","db,detDb"                                         ,"","","Year4 geometry fixed",kFALSE},
  {"RY2004d","","","db,detDb"                                  ,"","","Year4 geometry with ne wSVT",kFALSE},
  {"RY2005" ,"","","db,detDb"                          ,"","","Real data with Year5 study geometry",kFALSE},
  {"RY2005x","","","db,detDb"                          ,"","","Real data with Year5 study geometry",kFALSE},
  {"RY2005b","","","db,detDb"                          ,"","","Real data with Year5 study geometry",kFALSE},
  {"RY2005c","","","db,detDb"                                      ,"","","the best Year5 geometry",kFALSE},
  {"RY2005d","","","db,detDb"                                             ,"","","y2005c + new SVT",kFALSE},
  {"RY2006","","","db,detDb"                                             ,"","","y2006 for p+p run",kFALSE},
  {"RY2007","","","db,detDb"                                            ,"","","y2007 for AuAu run",kFALSE},

  {"Y2a"   ,"","","db,detDb"                                  ,"","","Old (CDR time) complete STAR",kFALSE},
  {"Y2b"   ,"","","db,detDb"       ,"","","2001 geometry 1st guess:TPC+CTB+FTPC+RICH+CaloPatch+SVT",kFALSE},
  {"Y2001" ,"","","db,detDb"      ,"","","year2001: geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Y2001n","","","db,detDb","","",  "year2001: new geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD",kFALSE},
  {"Y2003" ,"","","db,detDb","","",
                               "year2003: new geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL",kFALSE},
  {"Y2003X" ,"","","db,detDb","","",
                  "y2003X: new geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL - Full B/E EMC",kFALSE},
  {"Y2003a" ,"","","db,detDb","","",
                                  "Year2003 geometry with corrected barrel EMC and SVT layer radii",kFALSE},
  {"Y2003b" ,"","","db,detDb","","",
    "Year2003 geometry with corrected barrel EMC and SVT layer radii and extra material in the SVT",kFALSE},
  {"Y2003c" ,"","","db,detDb","","",
"Year2003 geometry with corrected barrel EMC and SVT layer radii and extra material in the new SVT",kFALSE},
    
  {"Y2004"  ,"","","db,detDb","","",                            
       "Initial Yea4 geometry - TPC, SVT, SSD, CTB(TOF), FTPC, EMC, ECAL, PMD, BBC, ZDC, FPD, pVPD",kFALSE},
  {"Y2004a" ,"","","db,detDb","","",                                      "Y2004 with PMD adjusted",kFALSE},
  {"Y2004b" ,"","","db,detDb","","",                                        "Y2004a + SSD materiau",kFALSE},
  {"Y2004x" ,"","","db,detDb","","",                 "Y2004 with full barrel EMC and two caps ECAL",kFALSE},
  {"Y2004y" ,"","","db,detDb","","",  "Y2004 full barrel EMC and two caps ECAL+TPC back+SVT cables",kFALSE},
  {"Y2004c" ,"","","db,detDb","","",                                     "Y2004a + SSD materiau V2",kFALSE},
  {"Y2004d" ,"","","db,detDb","","",                                             "Y2004c + new SVT",kFALSE},

  {"Y2005"  ,"","","db,detDb","","",                                      "Initial Year5 geometry", kFALSE},
  {"Y2005x" ,"","","db,detDb","","",                              "Full barrel EMC Year5 geometry", kFALSE},
  {"Y2005b" ,"","","db,detDb","","",        "Year5 geometry + corrections SVT, FTPC gas + SSD y4c", kFALSE},
  {"Y2005c" ,"","","db,detDb","","",   "Year5 geometry + more corrections SVT, FTPC gas + SSD y4c", kFALSE},
  {"Y2005d" ,"","","db,detDb","","",                                            "y2005c + new SVT", kFALSE},
  {"ForceGeometry","","","","","",  "Force geometry to overwrite the geometry caming from fz-file", kFALSE},
  // geometry timestamps are now dynamic. Please see StChain/StMaker    
  {"Complete","","","db,detDb"            ,"","","complete: new (currently foreseen) complete STAR",kFALSE},
  {"Ist1"    ,"","","db,detDb"                                   ,"","","Development geometry STAR",kFALSE},

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

  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#ifndef __CLEANUP__
  {"Physics"     ,"","","trg"                                        ,"","","Select Physics events",kFALSE},
  {"LaserTest"   ,"","","trg"                                          ,"","","Select Laser events",kFALSE},
  {"PulserSvt"   ,"","","trg"                                     ,"","","Select SVT Pulser events",kFALSE},
  {"alltrigger"  ,"","","trg"                              ,"","","Select all events (no trig sel)",kFALSE},
#else
  {"Physics"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"LaserTest"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"PulserSvt"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"alltrigger"  ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"phys_off"    ,"","",""                                  ,"","","Turn off physics in simulation",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"doEvents"    ,""  ,"","in,StEvent,analysis,NoDb"                                      ,"","","",kFALSE},
  {"drawDst"     ,""  ,"","in,ry1h,globT,SCL,geant,display,NoDb,TbUtil"                   ,"","","",kFALSE},
  {"Cdst"        ,""  ,"","global,dst,event,analysis,EventQA"                             ,"","","",kFALSE},
  {"C1default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits"    ,"","","Year 1 chain",kFALSE},
  {"C2default"   ,""  ,"","tpc,rich,l0,Cdst,Kalman,tags,Tree,EvOut,ftpc,svt,emcY2"
                                                                              ,"","","Year 2 chain",kFALSE},
  {"C3default"   ,""  ,"","tpc,l0,Cdst,Kalman,tags,Tree,EvOut,NoHits,ftpc,svt,bbcsim,emcY2"
                                                                    ,"","","Year 3 simu base chain",kFALSE},
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
  {"C2003"       ,""  ,"","y2003,C3default"                            ,"","","Turn on chain Y2003",kFALSE},
  {"C2003X"      ,""  ,"","y2003X,C3default"           ,"","","Turn on chain Y2003X (full B/E EMC)",kFALSE},
  // MDC / Sim chain agregates
  {"mdc3"        ,""  ,"","cy1h,GeantOut"                               ,"","","MDC3 default chain",kFALSE},
  {"MDC4"        ,""  ,"","C2001,trs,srs,fss,rrs,big,GeantOut"      ,"","","Turn on chain for MDC4",kFALSE},
  {"MDC4New"     ,""  ,"","y2001n,C2default,trs,srs,fss,rrs,big,GeantOut","","",
                                                     "Turn on chain for MDC4 (for after September)",kFALSE},
  {"PostMDC4"    ,""  ,"","C2001,trs,sss,fss,rrs,big,GeantOut"     ,"","","Turn on Post MDC4 chain",kFALSE},
#ifndef __CLEANUP__  
  {"ppMDC4"      ,""  ,"","ppOpt,C2001,-PreVtx,mwc,trs,srs,rrs,big,GeantOut",
                                                                    "","","Turn on chain for ppMDC",kFALSE},
#endif
  {"dAuMDC"      ,""  ,"","ppOpt,C2003,-PreVtx,trs,srs,fss,big,GeantOut","",""    ,"Chain for d+Au",kFALSE},
#ifndef __CLEANUP__
  {"dAuMDCa"     ,""  ,"","ppOpt,C2003,-PreVtx,trs,srs,fss,big,GeantOut,est","","","Chain for d+Au",kFALSE},
#endif
  {"CComplete"   ,""  ,"","Complete,C2default"             ,"","","Turn on chain for Complete STAR",kFALSE},
  // Detector combined-chains
  {"SvtD"       ,"","","SvtDb,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit",       "","","SVT chain for Data",kFALSE},
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
  {"pp2001"      ,""  ,"","ppOpt,B2001,-PreVtx,l3onl,tofDat,emcDY2,Corr2","",""
                                                                        ,"pp 2001 (+ l3, tof, emc)",kFALSE},
  {"pp2001a"     ,""  ,"","pp2001,svt_daq,SvtD,ftpc","",""
                                                             ,"pp 2001 (+ ftpc, svt, l3, tof, emc)",kFALSE},
  // Year 3 chains
  // B2003 is a base-chain with tpc only for now
  {"B2003"       ,""  ,"","ry2003,in,tpc_daq,tpc,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                       ,"Base chain for 2003 (tpc)",kFALSE},
  {"dau2003"     ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc","",""
                 ,"Production chain for winter 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc)",kFALSE},
  {"dau2003a"    ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd","",""
           ,"Production chain for winter 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc, trgd)",kFALSE},
  {"pp2003"      , "" ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd","",""
           ,"Production chain for Spring 2003 data (+ tof, bcc/fpd, svt (no est), ftpc, emc, trgd)",kFALSE},
#ifdef __BFC2__
  {"Idst"        ,""  ,"",              "dst,event,compend,EventQA"   ,"","","Turn on DST for ITTF",kFALSE},
  {"IAna"    ,""  ,"","dEdxY2,Kink2,xi2,CMuDst,analysis","","","Turn on User Maker, dEdx and MuDst",kFALSE},
  {"B2003I"      ,"","","ry2003,in,tpc_daq,tpcI,fcf,Physics,Idst,l0,tags,Tree,evout"
                                                                  ,"","","Base chain for 2003 ITTF",kFALSE},
  {"dau2003i"    ,"","","B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd",
                                   "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
  {"pp2003i"     ,""  ,"",
                  "B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd",
                                   "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
#endif /* __BFC2__ */
#ifndef __BFC2__
  // Year 4 chains (2003/2004) 
  {"B2004"       ,""  ,"","ry2004,in,tpc_daq,tpc,svt_daq,SvtD,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                   ,"Base chain for 2004 (tpc+svt)",kFALSE},
  {"P2004"       ,""     ,"","B2004,l3onl,fcf,ToF,emcDY2,fpd,Corr4,ftpc,trgd,ZDCvtx,OSpaceZ2","",""
                ,"Production chain for winter 2003/2004 data (+ l3, tof, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  {"pp2004"     ,""   ,"",
   "B2004,fcf,ppOpt,VFppLMV5,-PreVtx,l3onl,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,OSpaceZ2",
                    "","","Production chain for 2004 pp data (+ l3, tof, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  {"B2005"       ,""  ,"","ry2005b,in,tpc_daq,tpc,svt_daq,SvtD,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                   ,"Base chain for 2005 (tpc+svt)",kFALSE},
#ifndef __CLEANUP__
  {"B2005a"      ,""  ,"","ry2005b,in,tpc_daq,tpc,Physics,Cdst,-SvtDedx,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2005 (tpc only)",kFALSE},
#else
  {"B2005a"      ,""  ,"","ry2005b,in,tpc_daq,tpc,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2005 (tpc only)",kFALSE},
#endif
  {"P2005"       ,""     ,"","B2005,l3onl,fcf,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3","",""
                     ,"Production chain for winter 2004/2005 data (+ l3, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  {"pp2005"     ,""   ,"",
     "B2005,fcf,ppOpt,VFppLMV5,-PreVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3",
                         "","","Production chain for 2005 pp data (+ l3, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  {"pp2005a"      ,"" ,"",
   "B2005a,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},  
#else /* __BFC2__ */
  {"B2004"       ,""        ,"","ry2004,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout","",""
                                                              ,"Base chain for 2004 ITTF (tpc+svt)",kFALSE},
  // Notes:
  //  fcf was not added by default to allow switching if needed
  //  there is no PreVtx in tpcI so no need to do -PreVtx for pp chain
  //  SVT is added as base default, svtIT in chains
  {"P2004"       ,"" ,"",
     "B2004,IAna,fcf,VFMinuit,l3onl,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2",
              "","","Production chain for 2003/2004 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2004"      ,"" ,"",
     "B2004,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,l3onl,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2",
              "","","Production chain for 2003/2004 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 5 Chains
  {"B2005"       ,""       ,"","ry2005b,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout","",""
                                                              ,"Base chain for 2005 ITTF (tpc+svt)",kFALSE},
#ifndef __CLEANUP__
  {"B2005a"      ,""       ,"","ry2005b,in,tpc_daq,tpcI,Physics,Idst,-SvtDedx,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2005 ITTF (tpc only)",kFALSE},
#else
  {"B2005a"      ,""       ,"","ry2005b,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2005 ITTF (tpc only)",kFALSE},
#endif
  {"P2005"       ,"" ,"",
     "B2005,IAna,fcf,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3",
              "","","Production chain for 2004/2005 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005"      ,"" ,"",
     "B2005,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005a"      ,"" ,"",
   "B2005a,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  // Year 6 chains - Geometry 2006 not yet ready, starting with y2005d
  {"B2006"       ,""       ,"","ry2005d,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout","",""
                                                              ,"Base chain for 2006 ITTF (tpc+svt)",kFALSE},
#ifndef __CLEANUP__
  {"B2006a"      ,""       ,"","ry2005d,in,tpc_daq,tpcI,Physics,Idst,-SvtDedx,l0,tags,Tree,evout","",""
                                             ,"Base chain for 2006 with 2005d geom ITTF (tpc only)",kFALSE},
  {"B2006b"      ,""       ,"","ry2006,in,tpc_daq,tpcI,Physics,Idst,-SvtDedx,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2006 ITTF (tpc only)",kFALSE},
#else
  {"B2006a"      ,""       ,"","ry2005d,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout","",""
                                             ,"Base chain for 2006 with 2005d geom ITTF (tpc only)",kFALSE},
  {"B2006b"      ,""       ,"","ry2006,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2006 ITTF (tpc only)",kFALSE},
#endif

  {"pp2006a"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006a,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006b"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006b,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,BeamBack",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},


  {"B2007","","","ry2007,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,spt,Physics,Idst,l0,tags,Tree,evout",
                                                        "","","Base chain for 2007 ITTF (tpc+svt+ssd)",kFALSE},
  {"P2007"       ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,skip1row,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5",
                      "","","Production chain for 2007 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  //  {"testing"      ,"" ,"",   // just a damned test
  //   "B2006b,sdt20061211,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
  //                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},


#endif /* __BFC2__ */
  // Other chains/Calibration
  {"LaserCal0","" ,"","db,detDb,tpc_daq,tpcDb,tcl,globT,laser,LaserTest","","",
                                                                    "Laser Calibration Chain (tcl)",kFALSE},
  {"LaserCal",""  ,"","db,detDb,tpc_daq,tpcDb,fcf,globT,laser,LaserTest","","",
                                                                          "Laser Calibration Chain",kFALSE},
  {"L3Counter","" ,"","db,detDb,in,l3count","","",                     "L3 Counter extraction pass",kFALSE},
  {"VtxSeedCal","","","ppOpt,ry2001,in,tpc_daq,tpc,global,-Tree,Physics,-PreVtx,FindVtxSeed,NoEvent,Corr2",
                                                                     "","","Pass0 Vertex evaluator",kFALSE},
  {"SpcChgCal","","","B2004,fcf,Corr3,OSpaceZ2,OShortR,SCEbyE,-Tree,-tags,-EvOut,-EventQA",
                                                                "","","Pass0 SpaceCharge evaluator",kFALSE},

  // New-- DBV20050515,useCDV Old-- Corr3,OSpaceZ2,OShortR,SCEbyE
  {"SpcChgCalG","","","MuDST,fcf,Corr4,OSpaceZ2,OGridLeak3D,SCEbyE,-Tree,-tags,-EvOut,-EventQA",
                                                  "","","Pass0 SpaceCharge evaluator with GridLeak",kFALSE},

  {"VtxSeedCalG","","","MuDST,fcf,Corr4,FindEvtVtxSeed,-Tree,-tags,-EvOut,-EventQA",       
                                                                     "","","Pass0 Vertex evaluator",kFALSE},


  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"OPTIONS     ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#ifdef __BFC2__
  {"ITTF"        ,""  ,"","Sti",                                         "","","Turn on ITTF chain",kFALSE},
#endif /* __BFC2__ */
  {"SvtHitFilt"  ,"", "","",                                           "","","SVT Hit filter Maker",kFALSE},
  {"NoHits"      ,""  ,"",""                            ,"","","Don't write hits into Event.Branch",kFALSE},
  {"Kalman"      ,""  ,"","geant"                                                         ,"","","",kFALSE},
  {"Eval"        ,""  ,"","","",""                ,"Turn on evaluation switch for different makers",kFALSE},
  {"Ev03"        ,""  ,"","","",""                                 ,"Turn on alternative V0 method",kFALSE},
  {"off"         ,""  ,"","","",""                                        ,"Turn off default chain",kFALSE},
  {"clearDAQCTB" ,""  ,"","","" ,""                             ,"clear DAQ CTB Hits for embedding",kFALSE},
  {"NoInput"     ,""  ,"","","" ,""                                                ,"No input file",kFALSE},
  {"util"        ,""  ,"","","","StAnalysisUtilities",                   "Load StAnalysisUtilities",kFALSE},
  {"FieldOn"     ,""  ,"","MagF"                                   ,"","" ,"Constant nominal field",kFALSE},
  {"FieldOff"    ,""  ,"","MagF"                                          ,"","" ,"No Field option",kFALSE},
  {"HalfField"   ,""  ,"","MagF"                                         ,"","","Half Field option",kFALSE},
  {"ReverseField",""  ,"","MagF"                                      ,"","","Reverse Field option",kFALSE},
  {"NoCintDb"    ,""  ,"",""                                   ,"","","Switch off standard Cint Db",kFALSE},
  {"NoCintCalDb" ,""  ,"",""                                      ,"","","Switch off local Cint Db",kFALSE},
  {"NoMySQLDb"   ,""  ,"",""                                           ,"","","Switch off MySQL Db",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug1"      ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug2"      ,""  ,"",""                                            ,"","","Set debug flag = 2",kFALSE},
  {"IdTruth"     ,""  ,"",""              ,"","","Enable IdTruth association in StAssociationMaker",kFALSE},
  {"useInTracker",""  ,"","","",""    ,"switch from EGR to Sti global tracks in StAssociationMaker",kFALSE},
  {"noRepeat"    ,""  ,"",""                                        ,"","","No repeat in Messenger",kFALSE},
  {"useInTracker",""  ,"","","",""    ,"switch from EGR to Sti global tracks in StAssociationMaker",kFALSE},
  {"noHistos"    ,""  ,"",""                                    ,"","","Disables Attributes histos",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},
  {"big"         ,""  ,"",""                                         ,"","","Set NwGEANT =20Mwords",kFALSE},
  {"bigbig"      ,""  ,"",""                                         ,"","","Set NwGEANT =40Mwords",kFALSE},
#ifdef __BFC2__
  {"clearmem"    ,""  ,"",""                            ,"","","Instruct StiMaker to clear factory",kFALSE},
#endif /* __BFC2__ */
  {"InTree"      ,""  ,"","in",""                                     ,"","bfcTree Input Tree name",kFALSE},
  {"OutTree"     ,""  ,"","Tree",""                                  ,"","bfcTree Output Tree name",kFALSE},
  {"DstOut"      ,""  ,"","Tree"                                       ,"","","Write dst to StTree",kFALSE},
  {"McEvOut"     ,""  ,"","StMcEvent,Tree"                       ,"","","Write StMcEvent to StTree",kFALSE},
  {"EvOut"       ,""  ,"","Tree"                                   ,"","","Write StEvent to StTree",kFALSE},
  {"GeantOut"    ,""  ,"","Tree"                                ,"","","Write g2t tables to StTree",kFALSE},
  {"Simu"        ,""  ,"",""                                                ,"","","Simulated Data",kFALSE},
  {"HitsBranch"  ,""  ,"",""  ,"","","take out points from dst branch and put them into HitsBranch",kFALSE},
  {"paw"         ,""  ,"",""                                      ,"","","Allocate memory for pawc",kFALSE},
  {"AllEvent"    ,""  ,"","Tree"                               ,"","","Write whole event to StTree",kFALSE},
  {"AllTables"   ,""  ,"","",""                                     ,"St_Tables","Load Star Tables",kFALSE},
    
  {"Corr1"       ,""  ,"","AlignSectors,ExB,OBmap,OClock,OPr13","","",
                                                      "... AlignSectors,ExB,OBmap,OClock,OPr13 ...",kFALSE},
  {"Corr2"       ,""  ,"","Corr1,OTwist,OIFC"                     ,"","","...Corr1+OTwist,OIFC ...",kFALSE},
  {"Corr3"       ,""  ,"","AlignSectors,ExB,OBmap2D,OClock,OPr13,OTwist,OIFC","","",
                                        "... AlignSectors,ExB,OBmap2D,OClock,OPr13,OTwist,OIFC ...",kFALSE},
  {"Corr4"       ,""  ,"","Corr3,OShortR"                             ,"","","... Corr3+OShortR...",kFALSE},
  {"Corr5"       ,""  ,"","Corr4,SCEbyE,OGridLeak3D,OSpaceZ2","","",
                                                         "... Corr4+SCEbyE,OGridLeak3D,OSpaceZ2...",kFALSE},
  
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
  {"AlignSectors",""  ,"","",""                          ,"","Activate Sector Alignment correction",kFALSE},
    
  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},

  {"pp"          ,""  ,"","",                            "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ppOpt"       ,""  ,"","TrsPileUp","","",             "pp option without enabling special cases",kFALSE},
  {"TrsPileUp"   ,""  ,"","","","",                                              "Trs pile up mode",kFALSE},
  {"TrsToF"      ,""  ,"","","","",                       "Trs account for particle time of flight",kFALSE},

#ifndef __BFC2__
  {"SvtMatchVtx" ,""  ,"","",""                ,"","Use SVT matched tracks to find  Primary Vertex",kFALSE},
#endif /* ! __BFC2__ */

  {"VtxOffSet"   ,""  ,"","",""                 ,"","Account Primary Vertex offset from y2000 data",kFALSE},
  {"Calibration" ,""  ,"","",""                                              ,"","Calibration mode",kFALSE},
  {"beamLine"    ,""  ,"","",""                                       ,"","LMV Beam line constrain",kFALSE},

#ifndef __BFC2__
  // This option does nothing in TPT mode
  //{"CtbMatchVtx" ,""  ,"","",""                       ,"","... CTB Matching ON in Vertex Finding",kFALSE},
  // this option will not be enabled in non-ITTF mode
  {"VFPPV"         ,""  ,"","VFppLMV5",""             ,"","... WARNING VFPPV is NOT valid with TPT",kFALSE},

  {"VFppLMV"       ,""  ,"","",""                        ,"","...VertexMaker will use ppLMV method",kFALSE},
  {"VFppLMV5"      ,""  ,"","",""                ,"","...VertexMaker will use ppLMV method (tuned)",kFALSE},

#else /* __BFC2__ */
  {"CtbMatchVtx"    ,""  ,"","genvtx",""                ,"","... CTB Matching ON in Vertex Finding",kFALSE},
  {"VFPPV"          ,""  ,"","genvtx",""                      ,"","... Pile-up proof vertex finder",kFALSE},
  {"VFPPVnoCTB"     ,""  ,"","genvtx",""               ,"","... Pile-up proof vertex finder, noCTB",kFALSE},
  {"VFMinuit"       ,""  ,"","genvtx",""                ,"","... Generic VF will use Minuit method",kFALSE},
  {"VFFV"           ,""  ,"","genvtx",""                            ,"","... Fixed dummy VF method",kFALSE},
  {"VFMCE"          ,""  ,"","genvtx",""                        ,"","... Fixed vertex from MCEvent",kFALSE},

  {"VFppLMV"        ,""  ,"","genvtx",""                 ,"","...VertexMaker will use ppLMV method",kFALSE},
  {"VFppLMV5"       ,""  ,"","genvtx",""         ,"","...VertexMaker will use ppLMV method (tuned)",kFALSE},
#endif /* __BFC2__ */


  {"onlcl"  ,""  ,"","",""                                       ,"","Read/use TPC DAQ100 clusters",kFALSE},
  {"onlraw" ,""  ,"","",""                                              ,"","Read/use TPC raw hits",kFALSE},
    
  {"ezTree" ,""  ,"","",""                                               ,"","Create ezTree branch",kFALSE},
  {"BEmcDebug","" ,"","",""                            ,"","Turn OFF B-EMC hit reconstruction cuts",kFALSE},
    
    // Those options are for StTpcDbMaker
  {"useLDV" ,""  ,"","",""                                   ,"","... uses laserDV database flavor",kFALSE},
  {"useCDV" ,""  ,"","",""                                       ,"","... uses ofl database flavor",kFALSE},
    
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Tables      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#ifndef __CLEANUP__
  {"tables"      ,""  ,"",
     "StDbT,ctf_T,ebyeT,emc_T,ftpcT,gen_T,geomT,globT,l3_T,mwc_T,sim_T,svt_T,tpc_T,trg_T,vpd_T",
                                                                                           "","","",kFALSE},
#else
  {"tables","","","StDbT,ctf_T,ebyeT,emc_T,ftpcT,gen_T,geomT,globT,l3_T,sim_T,svt_T,tpc_T","","","",kFALSE},
#endif
  {"StDbT"       ,""  ,"","",""                                   ,"StDb_Tables","Load StDb_Tables",kFALSE},
  {"ctf_T"       ,""  ,"","",""                                     ,"ctf_Tables","Load ctf_Tables",kFALSE},
  {"ebyeT"       ,""  ,"","",""                                   ,"ebye_Tables","Load ebye_Tables",kFALSE},
  {"emc_T"       ,""  ,"","",""                                     ,"emc_Tables","Load emc_Tables",kFALSE},
  {"ftpcT"       ,""  ,"","",""                                   ,"ftpc_Tables","Load ftpc_Tables",kFALSE},
  {"gen_T"       ,""  ,"","",""                                     ,"gen_Tables","Load gen_Tables",kFALSE},
  {"geomT"       ,""  ,"","",""                           ,"geometry_Tables","Load geometry_Tables",kFALSE},
  {"globT"       ,""  ,"","",""                               ,"global_Tables","Load global_Tables",kFALSE},
  {"l3_T"        ,"",  "","",""                                       ,"l3_Tables","Load l3_Tables",kFALSE},
#ifndef __CLEANUP__
  {"mwc_T"       ,""  ,"","",""                                     ,"mwc_Tables","Load mwc_Tables",kFALSE},
#else
  {"mwc_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"sim_T"       ,""  ,"","",""                                     ,"sim_Tables","Load sim_Tables",kFALSE},
  {"svt_T"       ,""  ,"","",""                                     ,"svt_Tables","Load svt_Tables",kFALSE},
  {"tpc_T"       ,""  ,"","",""                                     ,"tpc_Tables","Load tpc_Tables",kFALSE},
#ifndef __CLEANUP__
  {"trg_T"       ,""  ,"","",""                                     ,"trg_Tables","Load trg_Tables",kFALSE},
  {"vpd_T"       ,""  ,"","",""                                     ,"vpd_Tables","Load vpd_Tables",kFALSE},
#else
  {"trg_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"vpd_T"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
#ifdef __BFC2__
  {"NoSvtIT"     ,""  ,"","-SvtIT",""                    ,"","ITTF: track with switch off SVT geom",kFALSE},
  {"SvtIT"       ,""  ,"","",""                                    ,"","ITTF: track using SVT geom",kFALSE},
  {"SsdIT"       ,""  ,"","",""                                    ,"","ITTF: track using SSD geom",kFALSE},
  {"HpdIT"       ,""  ,"","StiRnD",""                              ,"","ITTF: track using Hpd geom",kFALSE}
  {"PixelIT"     ,""  ,"","StiRnD",""                            ,"","ITTF: track using Pixel geom",kFALSE},
  {"IstIT"       ,""  ,"","StiRnD",""                              ,"","ITTF: track using Ist geom",kFALSE},
  {"skip1row"    ,""  ,"","",""                           ,"","ITTF: skip the first pad row in TPC",kFALSE},
#endif /* __BFC2__ */
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Geometry+Mag","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"pgf77"    ,"" ,"","",""                                                   ,"pgf77VMC","Fortran",kFALSE},
  {"minicern"    ,"" ,"","",""                                               ,"minicern","minicern",kFALSE},
  {"mysql"    ,"" ,"","",""                                                  ,"mysqlclient","MySQL",kFALSE},
  {"geometry"    ,"" ,"","",""                                     ,"geometry","geometry+Mag.Field",kFALSE},
  {"StarMagField","", "","magF"                              ,"","StarMagField","Load StarMagField",kFALSE},
  {"geomNoField" ,"" ,"","-geometry,StarMagField"        ,"","geometryNoField","geometry-Mag.Field",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#ifndef __CLEANUP__
  {"vpd"         ,""  ,"","vpd_T",""                                                   ,"St_vpd","",kFALSE},
#else
  {"vpd"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"tls"         ,""  ,"","",""                                                           ,"tls","",kFALSE},
  {"daq"         ,""  ,"","",""                            ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                             ,"StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TRGDef"      ,""  ,"","",""                          ,"StTriggerDataMaker","Load StTriggerData",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"StBichsel"   ,""  ,"","",""                         ,"StBichsel","Load Bichsel model for dE/dx",kFALSE},
  {"StEvent"     ,""  ,"","globT,SCL,TRGDef,StBichsel",""                 ,"StEvent","Load StEvent",kFALSE},
  {"SsdUtil"     ,""  ,"","",""                                        ,"StSsdUtil","Load SSD Util",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T,geomT,StDbT",""                      ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"EEmcUtil"    ,""  ,"","",""                                     ,"StEEmcUtil","Load StEEmcUtil",kFALSE},
  {"l3Util"      ,""  ,"","",""                                         ,"Stl3Util","Load Stl3Util",kFALSE},
  {"PmdUtil"     ,""  ,"","","",                                       "StPmdUtil","Load StPmdUtil",kFALSE},
  {"QUtils"      ,""  ,"","PmdUtil,EmcUtil","",                      "","Load QA Libs dependencies",kFALSE},
  {"MuDSTDeps"   ,""  ,"","StEvent","","Physics,StEventUtilities,StStrangeMuDstMaker",
                                                              "Load MuDST misc. dependencies (all)",kFALSE},
  {"MuDST"       ,"" ,"","MuDSTDeps,EmcUtil,TofUtil,PmdUtil","","StMuDSTMaker","Load MuDST library",kFALSE},
  {"geantL","","","geomT,gen_T,sim_T,StarMagField,geomNoField","","Geom,St_g2t,St_geant_Maker"
                                                                                  ,"Load GeantLibs",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"gstar"       ,"geant"  ,"","-fzin,-ntin,-geant,Simu,geantL","St_geant_Maker", 
                                      "","gstar for 80 muon tracks with pT = 1GeV in |eta|<4",kFALSE},
  {"tdaq"        ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"miniDAQ"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"fzin"        ,"geant","","Simu,-gstar,-ntin,-geant,geantL","St_geant_Maker","",
                                                                               "read gstar fz-file",kFALSE},
  {"in"         ,""  ,"",""              ,     "StIOMaker","StIOMaker","Read [DAQ|ROOT] input file",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"dbutil"      ,""     ,"","StDbT"                       ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"db"          ,"db"   ,"","StDbT"             ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"magF"        ,"MagField","","StDbT,db","StMagFMaker","StarMagField,StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"svtDb"    ,"svtDb","","SvtCL,dbutil,db","StSvtDbMaker","StSvtDbMaker","Load and run SvtDbMaker",kFALSE},
  {"ssdDb"       ,"ssdDb","","SsdUtil,db","StSsdDbMaker","StSsdDbMaker","Load and run StSsdDbMaker",kFALSE},
  {"detDb"       ,"","","","StDetectorDbMaker","StDetectorDbMaker","Load and run StDetectorDbMaker",kFALSE},
  {"eemcDb"      ,"eeDb" ,"","db",                               "StEEmcDbMaker","StEEmcDbMaker","",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"trgd"        ,"trgd","","TRGDef"  ,"StTriggerDataMaker","StTriggerDataMaker","Get trigger data",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
//{"Event","","",",StEvent,tpcDB,detDb","StEventMaker",",StEventMaker","<StEvent creation/filling>",kFALSE},
//{"MakeEvent","","","Event",""                             ,"","WARNING *** Option is OBSOLETE ***"kFALSE},
// for simulation on fly Event time stamp is set outside of the simulation makers
  {"ntin"        ,"geant"  ,"","paw,-fzin,-geant,-gstar,Simu,geantL,paw","St_geant_Maker",
                                                       "gstar","read event generated Hbook nt-file",kFALSE},
  {"geant"       ,"geant","","geantL"                          ,"St_geant_Maker","","passive GEANT",kFALSE},
  {"RootVMC","","" ,"-geant,-fzin,-ntin,StarMagField,-geantL,-geometry,-geomNoField","",
                                         "Geom,VMC,Physics,EG,Pythia6,EGPythia6,minicern,geant3","",kFALSE},
  {"VMCAppl"  ,"","","geomT,gen_t,sim_T,RootVMC",""                  ,"StarVMCApplication","VMC G3",kFALSE},
  {"VMC"         ,"geant","","Simu,VMCAppl,-geant","StVMCMaker",           "StVMCMaker","VMC Maker",kFALSE},
  {"VMCPassive"  ,"geant","","VMCAppl"    ,"StVMCMaker",   "StVMCMaker","VMC Maker in Passive Mode",kFALSE},
  {"StMcEvent"   ,"","","gen_t,sim_T",                                              ,"StMcEvent","",kFALSE},
  {"McEvent"     ,"","","StEvent,EEmcUtil,EmcUtil,StMcEvent","StMcEventMaker"  ,"StMcEventMaker","",kFALSE},
  {"MakeEvent","0Event","","StEvent,tpcDB,detDb","StEventMaker","StEventMaker",
                                                                         "<Early StEvent creation>",kFALSE},

#ifndef __CLEANUP__ 
  {"l0"          ,"l0Chain","","trg_T,globT,ctf,trg"                        ,"StMaker","StChain","",kFALSE},
#else
  {"l0"          ,"l0Chain","","globT,ctf,trg"                              ,"StMaker","StChain","",kFALSE},
#endif
  {"ctf"         ,"ctf","l0Chain","ctf_T,db"               ,"St_ctf_Maker","St_ctf,St_ctf_Maker","",kFALSE},
#ifndef __CLEANUP__
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE},
  {"trg"         ,"trg","l0Chain","trg_T,globT,db"         ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"ppMCTrig"    ,"ppMC_trig1","l0Chain",""
                         ,"StppTrigMaker","StppSpin","Add emulation of pp Trigger based on CTB+MWC",kFALSE},
#else
  {"mwc"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"trg"         ,"trg","l0Chain","globT,db"               ,"St_trg_Maker","St_trg,St_trg_Maker","",kFALSE},
  {"ppMCTrig"    ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
    
  {"tpc"         ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StMaker","StChain","",kFALSE},
  {"tpcI" ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,TpcHitMover","StMaker","StChain","tpc with ITTF",kFALSE},
  {"Trs"         ,"Trs","tpcChain","scl,tpcDB,tpc_daq,Simu"           ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"TrsMini"     ,"","tpcChain","scl,tpcDB,-Trs,-tpc_daq,Simu","StTrsMiniMaker","StTrsMiniMaker","",kFALSE},
    
  {"Mixer"       ,"tpc_raw","","","StMixerMaker"  ,"StDaqLib,StDAQMaker,StTrsMaker,StMixerMaker","",kFALSE},
  {"St_tpc"      ,"","","tpc_T,tpcDb"                                               ,"","St_tpc","",kFALSE},
  {"St_svt"      ,"","","svt_T,svtDb"                                               ,"","St_svt","",kFALSE},
  //  {"StGlobal"     ,"","","globT",                                                 ,"","St_global","",kFALSE},
  {"tpc_daq"     ,"tpc_raw","tpcChain","detDb,tpc_T"        ,"St_tpcdaq_Maker","St_tpcdaq_Maker","",kFALSE},
  {"tfs"         ,"","tpcChain","Simu,tcl"                         ,"","","use tfs (no StTrsMaker)",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tls,St_tpc,StEvent","St_tcl_Maker","St_tcl_Maker",
                                                                        "Cluster Finder (from raw)",kFALSE},
  {"fcf"         ,"","tpcChain","daq,-tcl",      "StRTSClientFCFMaker","StRTSClientFCF,StRTSClientFCFMaker",
                                                                       "Offline FCF Cluster finder",kFALSE},
  {"Velo"        ,"","tpcChain","tpc_T,tls"                         ,"StVeloMaker","StVeloMaker","",kFALSE},
#ifndef __CLEANUP__  
  {"TpcHitFilter","tpc_hit_filter","tpcChain",""    ,"StTpcHitFilterMaker","StTpcHitFilterMaker","",kFALSE},
#endif
  {"TpcHitMover" ,"tpc_hit_mover","tpcChain","StEvent",
                      "StTpcHitMover","StTpcHitMoverMaker","TPC hits coord transform + corrections",kFALSE},
  {"tpt"   ,"tpc_tracks","tpcChain","tls,St_tpc,TpcHitMover",      "St_tpt_Maker","St_tpt_Maker","",kFALSE},
  {"tpt_old"     ,"tpc_tracks","tpcChain","St_tpc,tls",            "St_tpt_Maker","St_tpt_Maker","",kFALSE},
  {"TpcT0"  ,"TpcT0","","ctf_T,ftpcT,tls,St_tpc,St_svt,tpc_daq,kalman,StEvent","StTpcT0Maker",
                            "St_tcl_Maker,St_tpt_Maker,St_global,St_dst_Maker,StPass0CalibMaker","",kFALSE},
#ifndef __CLEANUP__ 
  {"ChargeStep","","","tpc_T,globT,tls,db,tpcDB,tpc_daq","StChargeStepMaker","StChargeStepMaker","",kFALSE},
#endif
  {"laser"       ,"tpc_tracks","LaserTest,tpcChain","tdaq,tpc,-tpt,-PreVtx"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},
  {"PreVtx"      ,"","tpcChain","tpt,SCL,sim_T,St_tpc,St_svt,ftpcT,ctf_T",
                                                     "StPreVertexMaker","St_global,St_dst_Maker","",kFALSE},
#ifndef __CLEANUP__ 
  {"svt"         ,"svtChain","","svt_T,SvtCL,Est"                           ,"StMaker","StChain","",kFALSE},
#else
  {"svt"         ,"svtChain","","svt_T,SvtCL"                               ,"StMaker","StChain","",kFALSE},
#endif
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"","","SvtSSim,SvtOnlSeq"         ,"","","Short cut for SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtSSim","SvtSSimu","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                  ,"StSvtSimulationMaker","StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
    
  {"SvtEmbed"  ,"","","SvtSSim,SvtEm,SvtOnlSeq"      ,"","","Short cutfor SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtEm","SvtEm","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit","StSvtEmbeddingMaker",
                                                          "StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
  {"SvtOnlSeq"   ,"SvtOnlSeq","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                             ,"StSvtOnlineSeqAdjSimMaker","StSvtSimulationMaker","",kFALSE},
    
  {"srs"         ,"svt_hits","svtChain","svtDb,tls,Simu,St_tpc,St_svt,SvtCL,-sss,-SvtSlowSim,StEvent"
                                                ,"St_srs_Maker","StSvtClusterMaker,St_srs_Maker","",kFALSE},
  {"svt_daq"     ,"svt_raw","svtChain","SvtCL"                  ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"   ,"SvtClu","svtChain","svt_T,StEvent,SvtCL","StSvtClusterMaker","StSvtClusterMaker","",kFALSE},
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},
#ifndef __CLEANUP__  
  {"SvtVtx"      ,"SvtVtx","SvtChain",""           ,"StSvtVertexFinderMaker","StSvtClusterMaker","",kFALSE},

  {"stk"        ,"svt_tracks","svtChain","tls,St_tpc,St_svt,SvtCL","St_stk_Maker","St_stk_Maker","",kFALSE},
  {"Est"         ,"","svtChain","St_svt"                    ,"StEstMaker","St_global,StEstMaker","",kFALSE},
#endif
  {"global"      ,"globalChain","","globT,St_tpc,St_svt,Match,vertex,primary,dst,SCL,dEdxY2"
                                                                            ,"StMaker","StChain","",kFALSE},
  {"Match"       ,"match","globalChain","SCL,tpc_T,St_svt,tls"
                                                        ,"StMatchMaker","St_global,St_dst_Maker","",kFALSE},
  //{"point"     ,"point","globalChain","SCL,tables,tls","StPointlMaker","St_global,St_dst_Maker","",kFALSE},
  {"Vertex"     ,"Vertex","globalChain","SCL,St_svt,tls"
                                   ,"StVertexMaker","St_global,St_dst_Maker","Primary Vertex finder",kFALSE},
  {"Primary"  ,"primary","globalChain","SCL,St_svt,tls","StPrimaryMaker","St_global,St_dst_Maker","",kFALSE},
  {"V0"         ,"v0","globalChain","SCL,St_svt,tls"        ,"StV0Maker","St_global,St_dst_Maker","",kFALSE},
  {"Xi"         ,"xi","globalChain","SCL,St_svt,tls"        ,"StXiMaker","St_global,St_dst_Maker","",kFALSE},
  {"Kink","kink","globalChain"     ,"SCL,tls,St_svt"  ,"StOldKinkMaker" ,"St_global,St_dst_Maker","",kFALSE},
    
  {"Fglobal"     ,"","","","",""                               ,"WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Fprimary"    ,"","","","",""                               ,"WARNING *** Option is OBSOLETE ***",kFALSE},
    
  {"dst"         ,"dst","globalChain","St_svt,dstOut,SCL,tls,gen_t,sim_T,ctf_T,trg_T,l3_T,ftpcT"
                                                        ,"St_dst_Maker","St_global,St_dst_Maker","",kFALSE},

  // StEvent,St_global,St_dst_Maker,

  {"dEdx"        ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"svtdEdx"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
  {"Event",  "","","StEvent,tpcDB,detDb","StEventMaker","StEventMaker","<StEvent creation/filling>",kFALSE},
#if 0
  {"GenericHit","","","StEvent","StGenericHitMaker","StGenericHitMaker","test GenericHitCollection",kFALSE},
#endif
  {"pixFastSim","","","StMcEvent,StEvent",
                                   "StPixelFastSimMaker","StPixelFastSimMaker","FastPixelSimulator",kFALSE},
  {"ssdUtil",    ,"","","",                                         ,"","StSsdUtil","Ssd utilities",kFALSE},
  {"ssddat"      ,"","","ssd_daq"                             ,"","","SSD full chain for Real Data",kFALSE},
  {"ssd_daq","SpaStrip","","ssddb,St_svt,-sls,-spa,ssdUtil","StSsdDaqMaker","StSsdDaqMaker","... SSD Daq",kFALSE},
  {"ssd"         ,"","","sls,spa,spt"                        ,"","","SSD full chain for simulation",kFALSE},
  {"sls"         ,"","","tls,Simu,St_tpc,St_svt,SvtCL","St_sls_Maker","StSsdSimulationMaker",
                                                                           "... SSD slow simulator",kFALSE},
  {"spa"         ,"SpaStrip","","tls,Simu,St_tpc,St_svt,SvtCL,ssdUtil","St_spa_Maker","StSsdSimulationMaker",
                                                                     "... SSD Pedestal Annihilator",kFALSE},
  {"spt"        ,"","","St_svt,ssdUtil","StSsdPointMaker","StSsdPointMaker","... SSD Point Creator",kFALSE},
  {"emcDY2"   ,"emcRaw","emcY2",
     "daq,eemcDb,EEmcUtil,emc_T,EmcUtil,StEvent,PreEcl,Epc","StEmcRawMaker","StEmcRawMaker",
                                                                        "B/E EMC data common maker",kFALSE},
  {"emcAtoE"  ,"","" ,"db,emcDY2","StEmcADCtoEMaker","StEmcADCtoEMaker", "B-EMC ADC to E converter",kFALSE},
  {"eemcD"       ,"","","","","",                              "WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ZDCVtx"      ,"","","db"                              ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},

  {"emcY2"    ,"emcY2","","emc_T,tpc_T,db,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim"   ,"","emcY2","emc_T,EmcUtil,StMcEvent","StEmcSimulatorMaker","StEmcSimulatorMaker",
                                                                           "New simulator for BEMC",kFALSE},
  {"EEfs" ,"eefs","","db,EEmcUtil,MuDst",
                                     "StEEmcFastMaker","StEEmcSimulatorMaker","EEMC fast simulator",kFALSE},

  {"genvtx"   ,"","","EEmcUtil","StGenericVertexMaker","Minuit,Sti,StGenericVertexMaker"
                                                                           ,"Generic Vertex Finder",kFALSE},
  {"StiUtil"  ,"","","",                              "","StiUtilities","Load StiUtilities library",kFALSE},
  {"Sti"      ,"Sti","","SCL,StEvent,tables,TpcDb,SvtDb,ssdDb,StiUtil","StiMaker",
                               "StEventUtilities,Sti,StiMaker,StiTpc,StiSvt,StiSsd" ,"ITTF tracker",kFALSE},
  {"StiRnD" ,"","","Sti",                                 "","StiRnD", "Load StiRnD shared library",kFALSE},
  {"StiPulls" ,"","","Sti",                                      "","", "Request to make Sti Pulls",kFALSE},
  {"BeamBack" ,"","","StEvent","StBeamBackMaker","StBeamBackMaker"
                                                              ,"Beam background tracker in the TPC",kFALSE},
  {"dEdxY2"       ,"dEdxY2","","tpcDb,StEvent","StdEdxY2Maker","StdEdxY2Maker",
                                                                     "Bichsel method used for dEdx",kFALSE},

  // needs to be done after the tracker
  {"FindVtxSeed"   ,"FindVtxSeed"   ,"","","StVertexSeedMaker",  "St_global,St_dst_Maker,StPass0CalibMaker",
                                                                     "Performs vertex seed finding",kFALSE},
  {"FindEvtVtxSeed","FindEvtVtxSeed","","","StEvtVtxSeedMaker",
                                   "StPass0CalibMaker","Performs vertex seed finding using StEvent",kFALSE},


  {"Ftpc"      ,"ftpcChain"  ,"","ftpcT,fcl,fpt"                            ,"StMaker","StChain","",kFALSE},
  {"fss"       ,"ftpc_raw","ftpcChain","SCL,Simu",
                                    "StFtpcSlowSimMaker","StFtpcSlowSimMaker","FTPC Slow simulator",kFALSE},
  {"Fcl"       ,"ftpc_hits","ftpcChain","SCL","StFtpcClusterMaker",
                    "StDaqLib,StDAQMaker,StFtpcTrackMaker,StFtpcClusterMaker","FTPC cluster finder",kFALSE},
  {"fpt"      ,"ftpc_tracks","ftpcChain","SCL"
                                          ,"StFtpcTrackMaker","StFtpcTrackMaker","FTPC Track Maker",kFALSE},
  {"fdbg"     ,"","","fcl,fpt","","","StFtpcClusterMaker and StFtpcTrackMaker will write debugfile",kFALSE},
  {"flaser"    ,"","","fpt"                              ,"","","StFtpcTrackMaker in LASERTRACKING",kFALSE},


  {"pmdRaw"    ,"pmdRaw","","PmdUtil,pmdRead,pmdClust"         ,"StMaker","StChain","PMD Raw chain",kFALSE},
  {"pmd"       ,"pmd","","pmdSim,pmdClust,pmdDis","StMaker"      ,"StChain", "PMD Simulation chain",kFALSE},
  {"pmdRead"   ,"","","PmdUtil","StPmdReadMaker"            ,"StPmdReadMaker", "DAQ reader for PMD",kFALSE},
  {"pmdSim"    ,"","","PmdUtil","StPmdSimulatorMaker","StPmdSimulatorMaker","Hit Simulator for PMD",kFALSE},
  {"pmdClust"  ,"pmdClust","","","StPmdClusterMaker",    "StPmdClusterMaker","ClusterMaker for PMD",kFALSE},
  {"pmdDis"    ,"pmdDis","PmdClust","","StPmdDiscriminatorMaker",
                                                  "StPmdDiscriminatorMaker","Discriminator for PMD",kFALSE},

#ifdef __BFC2__
  //  Reminder: You are within the ITTF chain definitions
#endif /* __BFC2__ */
  {"Kink2"       ,"kink2","","db,MuDST,-kink","StKinkMaker","StSecondaryVertexMaker",
                                                                          "Find Kinks from StEvent",kFALSE},
  {"V02"         ,"v02","","db,MuDST,-V0","StV0FinderMaker","StSecondaryVertexMaker",
                                                                            "Find V0s from StEvent",kFALSE},
  {"Xi2"         ,"xi2","","db,MuDST,-V02,-Xi","StXiFinderMaker","StSecondaryVertexMaker",
                                                                         "Xis AND V0s from StEvent",kFALSE},
#ifndef __CLEANUP__ 
  {"V0svt"       ,"v0svt","","db,MuDST","StV0FinderMaker","StSecondaryVertexMaker",
                                                              "Special: use estGlobal from StEvent",kFALSE},
  {"Xisvt"       ,"xisvt","","db,MuDST","StXiFinderMaker","StSecondaryVertexMaker",
                                                              "Special: use estGlobal from StEvent",kFALSE},
#endif
  {"SCEbyE"      ,"scebye","","","StSpaceChargeEbyEMaker","StEvent,StPass0CalibMaker",
                                                         "Determine EbyE SpaceCharge using StEvent",kFALSE},
  {"SCScalerCal" ,"scscalercal","","","StSpaceChargeEbyEMaker","StEvent,StPass0CalibMaker",
                                                                    "Calibrate SpaceCharge scalers",kFALSE},
  {"PostEmc"     ,"PostChain","","emc_T,tpc_T,db,PreEcl,EmcUtil"            ,"StMaker","StChain","",kFALSE},
  {"PreEcl"      ,"preecl","PostChain","" ,"StPreEclMaker",  "StPreEclMaker","B-EMC Cluster finder",kFALSE},
  {"Epc"         ,"epc","PostChain","PreEcl,EmcUtil" ,"StEpcMaker","StEpcMaker","B-EMC point maker",kFALSE},
  {"fpd"         ,"fpd","","",                  "StFpdMaker","StFpdMaker","FPD/BBC Data base chain",kFALSE}, // yf
  {"rich"        ,"RichChain","","rch,RichPiD,RichSpectra",        "StMaker","StChain","RICH chain",kFALSE},
  {"Rrs"         ,"","RichChain","sim_T,Simu"                         ,"StRrsMaker","StRrsMaker","",kFALSE},
  {"rch"         ,"","RichChain","sim_T,globT"             ,"StRchMaker","StRrsMaker,StRchMaker","",kFALSE},
  {"RichPiD"     ,"","RichChain","Event"                      ,"StRichPIDMaker","StRichPIDMaker","",kFALSE},
    
  {"ToF"       ,"TofChain","","tofDat,tofrMatch,tofpMatch,tofCalib","StMaker","StChain","ToF Chain",kFALSE},
  {"tofDat"    ,"tof_raw","TofChain","db,Tofutil","StTofMaker","StEvent,StTofMaker",
                                                                              "TOF Data base chain",kFALSE},
  {"tofsim"    ,"","TofChain","TofUtil","StTofSimMaker","StEvent,StTofMaker,StTofSimMaker",
                                                                                    "TOF Simulator",kFALSE},
  {"tofrMatch" ,"","TofChain","db,TofUtil","StTofrMatchMaker","StTofrMatchMaker",
                                                                       "TPC to TOFr track matching",kFALSE},
  {"tofpMatch"   ,"","TofChain","db,TofUtil","StTofpMatchMaker","StTofpMatchMaker",
                                                                       "TPC to TOFp track matching",kFALSE},
  {"tofCalib"   ,"","TofChain","db,TofUtil","StTofCalibMaker","StTofCalibMaker",  "TOF calibration",kFALSE},
    
  {"l3"          ,"l3Chain","","l3cl,l3t"                                   ,"StMaker","StChain","",kFALSE},
  {"l3cl"        ,"","l3Chain","l3_T,l3util"        ,"St_l3Clufi_Maker","St_l3,St_l3Clufi_Maker","",kFALSE},
  {"l3t"         ,"","l3Chain","l3_T,l3util"                ,"St_l3t_Maker","St_l3,St_l3t_Maker","",kFALSE},
  {"l3onl"       ,"","",""                            ,"Stl3RawReaderMaker","Stl3RawReaderMaker","",kFALSE},
  {"l3count"     ,"","",""                              ,"Stl3CounterMaker","Stl3RawReaderMaker","",kFALSE},
    
  {"bbcSim"         ,"","","db","StBbcSimulationMaker"      ,"StBbcSimulationMaker","BBC Simulator",kFALSE},
    
  {"analysis"    ,"","","StEvent"        ,"StAnalysisMaker","StAnalysisMaker","Example of Analysis",kFALSE},
#ifdef __BFC2__
  {"compend"     ,"","","event","StEventCompendiumMaker","StEventCompendiumMaker",
                                                                 "Fill event summary in ITTF Chain",kFALSE},
#endif /* __BFC2__ */
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra" ,"","",""                            ,"StRichSpectraMaker","StRichSpectraMaker","",kFALSE},
    
  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
#ifndef __CLEANUP__  
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
#endif
  {"Flow"        ,"","TagsChain","StEvent"         ,"StFlowMaker","StEventUtilities,StFlowMaker","",kFALSE},
#ifndef __CLEANUP__ 
  {"FlowTag"     ,"","TagsChain","StEvent,Flow"               ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
#endif
  {"FlowAnalysis","","TagsChain","StEvent,Flow"     ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"StrangeTags" ,"","TagsChain","StEvent"            ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  {"SpectraTag"  ,"","TagsChain","StEvent"              ,"StSpectraTagMaker","StSpectraTagMaker","",kFALSE},
  {"HeavyTags"   ,"","TagsChain","StEVent"                  ,"StHeavyTagMaker","StHeavyTagMaker","",kFALSE},
  {"HighPtTags"  ,"","TagsChain","StEVent"              ,"StHighPtTagsMaker","StHighPtTagsMaker","",kFALSE},
#ifndef __CLEANUP__
//  {"EbyeScaTags" ,"","TagsChain","StEvent"            ,"StEbyeScaTagsMaker","StEbyeScaTagsMaker","",kFALSE},
#endif
  {"PCollTag"    ,"","TagsChain","StEvent"                  ,"StPCollTagMaker","StPCollTagMaker","",kFALSE},
  {"tags"        ,"","TagsChain",         "globT,Event,StrangeTags,SpectraTag,HeavyTags,PCollTag,HighPtTags"
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
    
  {"MuDSTChain","MuDSTChain","EMCmDST,CMuDST",""                            ,"StMaker","StChain","",kFALSE},
  {"StrngMuDST","","MuDSTDeps","",              "StStrangeMuDstMaker","","Creates Stangeness MuDST",kFALSE},
  {"EMCmDST"   ,"","MuDSTChain","MuDst",                "StEmcMicroDstMaker","","Creates EMC MuDST",kFALSE},
  {"CMuDST"    ,"","MuDSTChain","MuDst,StrngMuDST",         "StMuDstMaker","","Writes Common MuDST",kFALSE},
  {"St_geom"     ,""  ,"",""     ,                               "St_geom_Maker","St_geom_Maker","",kFALSE},
  {"Display"     ,"","","TbUtil,St_geom",
               "StEventDisplayMaker","StEvent,StEventUtilities,StEventDisplayMaker","Event Display",kFALSE},
  {"Mc"          ,"McChain","McEvent","sim_T,globT,McAss,McAna"             ,"StMaker","StChain","",kFALSE},
  {"McAss"       ,"","McChain","McEvent",              "StAssociationMaker","StAssociationMaker","",kFALSE},
  {"McAnaTpc"    ,"","","McAna"                                         "","","Mc Analysis for Tpc",kFALSE},
  {"McAnaSvt"    ,"","","McAna"                                         "","","Mc Analysis for Svt",kFALSE},
  {"McAnaSsd"    ,"","","McAna"                                         "","","Mc Analysis for Ssd",kFALSE},
  {"McAna"       ,"","McChain","McEvent,McAss",          "StMcAnalysisMaker","StMcAnalysisMaker","",kFALSE},
  {"McQa"        ,"","McChain","McEvent",  "StMcQaMaker","StMcQaMaker","QA histogramms for McEvent",kFALSE},
  {"McTpcAna"    ,"","McAnaChain","McEvent,McAss",
                                    "StTpcMcAnalysisMaker","StTrsMiniMaker,StTpcMcAnalysisMaker","",kFALSE},
  {"MiniMcEvent" ,"","","","",                   "StMiniMcEvent","Loads StMiniMcEvent library only",kFALSE},
  {"MiniMcMk"    ,"","","McAss,MiniMcEvent","StMiniMcMaker","StMiniMcMaker",
                                                                 "Creates tree in minimc.root file",kFALSE},
  {"SvtMatTree","","","","SvtMatchedTree",
                                  "StSvtPoolEventT,StSvtPoolSvtMatchedTree","Create SvtMatchedTree",kFALSE},
  {"LAna"        ,"","","in,RY1h,tpcDb","StLaserAnalysisMaker"
                                                      ,"StLaserAnalysisMaker","Laser data Analysis",kFALSE},
#ifndef __CLEANUP__
  {"SpinTag" ,"SpinTag","","","StSpinTagMaker","StppSpin","tag for analysis of polarized pp events",kFALSE},
  {"ppLPfind1"   ,"ppLPfind1"  ,"",""  ,"StppLPfindMaker","StppSpin","Find leading particle for pp",kFALSE},
  {"SpinSortA"   ,"SpinSortA"  ,"",""               ,"StSpinSortMaker","StppSpin","Spin sort event",kFALSE},
  {"ppLPprojectA","ppLPprojectA","",""
                      ,"StppLPprojectMaker","StppSpin","project LP to the spin dependent phi-histo",kFALSE},
  {"ppLPeval1"   ,"ppLPeval1"  ,"",""  ,"StppLPevalMaker","StppSpin","Evaluation of LP algo for pp",kFALSE},
  {"ppDAQfilter1","ppDAQfilter1"  ,"",""  ,"StDAQfilterMaker","StppSpin","DAQ filter (used for pp)",kFALSE},
#else
  {"SpinTag"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPfind1"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"SpinSortA"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPprojectA","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppDAQfilter1","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE}, 
  {"ppLPeval1"   ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"QA"     ,"QA","","QUtils,globT,SCL,global","St_QA_Maker","St_QA_Maker","Filling Y1/Y2 Qa histo",kFALSE},
  {"EventQA","EventQA","","QUtils,Event","StEventQAMaker"   ,"St_QA_Maker","Filling Y2/Y3 Qa histo",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"HitFilt"     ,"", "","",               "StHitFilterMaker","StHitFilterMaker","Hit filter Maker",kFALSE},
  {"KeepTpcHit"  ,"", "","",                          "","","Keep all TPC hits in StHitFilterMaker",kFALSE},
  {"KeepSvtHit"  ,"", "","",                          "","","Keep all SVT hits in StHitFilterMaker",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"logger"      ,""  ,"",""            ,"","","Use log4cxx package to manage the program messages",kFALSE},
#ifndef __BFC2__
  {"ITTF"        ,""  ,"","",                               "","","Just to keep option ITTF==false",kFALSE},
#endif /* ! __BFC2__ */
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE}
};
