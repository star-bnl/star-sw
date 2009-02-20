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
  {"RY2005d","","","db,detDb"                                             ,"","","y2005d + new SVT",kFALSE},
  {"RY2005e","","","db,detDb"                                             ,"","","y2005e + new SVT",kFALSE},
  {"RY2005f","","","db,detDb"                                             ,"","","y2005f + new SVT",kFALSE},
  {"RY2005g","","","db,detDb"                                             ,"","","y2005g + new SVT",kFALSE},

  {"RY2006","","" ,"db,detDb"                                            ,"","","y2006 for p+p run",kFALSE},
  {"RY2006g","","","db,detDb"                                           ,"","","y2006g for p+p run",kFALSE},

  {"RY2007","","" ,"db,detDb"                                            ,"","","y2007 for AuAu run",kFALSE},
  {"RY2007g","","","db,detDb"                                           ,"","","y2007g for AuAu run",kFALSE},

  {"RY2008","","","db,detDb,NosvtIT,NossdIT"                             ,"","","y2008 for dAu run",kFALSE},
  {"RY2009","","","db,detDb,NosvtIT,NossdIT"                             ,"","","y2009 for p+p run",kFALSE},

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
  {"Y2003c" ,"","","db,detDb","","","Year2003 geometry with corrected barrel EMC and SVT layer "
   "radii and extra material in the new SVT"                                                       ,kFALSE},
    
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
  {"Y2005e" ,"","","db,detDb","","",                                       "y2005d + new SSD code", kFALSE},
  {"Y2005f" ,"","","db,detDb","","",                            "y2005d + SSD code with dead area", kFALSE},

  {"ForceGeometry","","","","","",  "Force geometry to overwrite the geometry coming from fz-file", kFALSE},

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
  // ATTENTION
  // - Removal of trg for years < 2003 will not allow bacward compatibility
  // - Removal of trg for years >= 2003 (2003 TBC) will need trgd instead
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
  {"MDC4"        ,""  ,"","C2001,trs,tpc_daq,Simu,srs,fss,rrs,big,GeantOut","",""
                                                                          ,"Turn on chain for MDC4",kFALSE},
  {"MDC4New"     ,""  ,"","y2001n,C2default,trs,tpc_daq,Simu,srs,fss,rrs,big,GeantOut","","",
                                                     "Turn on chain for MDC4 (for after September)",kFALSE},
  {"PostMDC4"    ,""  ,"","C2001,trs,tpc_daq,Simu,sss,fss,rrs,big,GeantOut"     
                                                                   ,"","","Turn on Post MDC4 chain",kFALSE},
#ifndef __CLEANUP__  
  {"ppMDC4"      ,""  ,"","ppOpt,C2001,-PreVtx,mwc,trs,tpc_daq,Simu,srs,rrs,big,GeantOut",
                                                                    "","","Turn on chain for ppMDC",kFALSE},
#else
  {"ppMDC4"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"dAuMDC"      ,""  ,"","ppOpt,C2003,-PreVtx,trs,tpc_daq,Simu,srs,fss,big,GeantOut","",""    
                                                                                  ,"Chain for d+Au",kFALSE},
#ifndef __CLEANUP__
  {"dAuMDCa"     ,""  ,"","ppOpt,C2003,-PreVtx,trs,tpc_daq,Simu,srs,fss,big,GeantOut,est","",""
                                                                                  ,"Chain for d+Au",kFALSE},
#else
  {"dAuMDCa"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
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
  {"B2005b"      ,""  ,"","ry2005f,in,tpc_daq,tpc,svt_daq,SvtD,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                        ,"Base chain for 2005 Geo f (tpc+svt only)",kFALSE},
#else
  {"B2005a"      ,""  ,"","ry2005b,in,tpc_daq,tpc,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                                  ,"Base chain for 2005 (tpc only)",kFALSE},
  {"B2005b"      ,""  ,"","ry2005f,in,tpc_daq,tpc,svt_daq,SvtD,Physics,Cdst,Kalman,tags,Tree,evout","",""
                                                        ,"Base chain for 2005 Geo f (tpc+svt only)",kFALSE},
#endif
  {"P2005"       ,""     ,"","B2005,l3onl,fcf,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3","",""
                     ,"Production chain for winter 2004/2005 data (+ l3, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  // no "a" which is fine and less confusing
  {"P2005b"      ,""     ,"","B2005b,l3onl,fcf,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3","",""
                     ,"Production chain for winter 2004/2005 data (+ l3, bcc/fpd, ftpc, emc, trgd)",kFALSE},

  {"pp2005"     ,""   ,"",
     "B2005,fcf,ppOpt,VFppLMV5,-PreVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3",
                         "","","Production chain for 2005 pp data (+ l3, bcc/fpd, ftpc, emc, trgd)",kFALSE},
  {"pp2005a"      ,"" ,"",
   "B2005a,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},  
  {"pp2005b"      ,"" ,"",
   "B2005b,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
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
  {"B2005b"      ,""       ,"","ry2005f,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout","",""
                                                   ,"Base chain for 2005 ITTF Geo f (tpc+svt only)",kFALSE},
#else
  {"B2005a"      ,""       ,"","ry2005b,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2005 ITTF (tpc only)",kFALSE},
  {"B2005b"      ,""       ,"","ry2005f,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout","",""
                                                   ,"Base chain for 2005 ITTF Geo f (tpc+svt only)",kFALSE},
#endif
  {"P2005"       ,"" ,"",
     "B2005,IAna,fcf,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3",
              "","","Production chain for 2004/2005 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2005b"      ,"" ,"",
     "B2005b,IAna,fcf,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3",
              "","","Production chain for 2004/2005 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  {"pp2005"      ,"" ,"",
     "B2005,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,Corr3",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005a"      ,"" ,"",
   "B2005a,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005b"      ,"" ,"",
   "B2005b,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},


  // Year 6 chains - Geometry 2006 not yet ready, starting with y2005d
  {"B2006"       ,""       ,"","ry2005d,in,tpc_daq,tpcI,svt_daq,SvtD,Idst,tags,Tree,evout","",""
                                                              ,"Base chain for 2006 ITTF (tpc+svt)",kFALSE},
 
#ifndef __CLEANUP__
  {"B2006a"      ,""       ,"","ry2005d,in,tpc_daq,tpcI,Idst,-SvtDedx,tags,Tree,evout","",""
                                             ,"Base chain for 2006 with 2005d geom ITTF (tpc only)",kFALSE},
  {"B2006b"      ,""       ,"","ry2006,in,tpc_daq,tpcI,Idst,-SvtDedx,tags,Tree,evout","",""
                                                             ,"Base chain for 2006 ITTF (tpc only)",kFALSE},
  {"B2006g"      ,""       ,"","ry2006g,in,tpc_daq,tpcI,Idst,-SvtDedx,tags,Tree,evout","",""
                                                       ,"Base chain for 2006 ITTF geo g (tpc only)",kFALSE},

#else
  {"B2006a"      ,""       ,"","ry2005d,in,tpc_daq,tpcI,Idst,tags,Tree,evout","",""
                                             ,"Base chain for 2006 with 2005d geom ITTF (tpc only)",kFALSE},
  {"B2006b"      ,""       ,"","ry2006,in,tpc_daq,tpcI,Idst,l0,tags,Tree,evout","",""
                                                             ,"Base chain for 2006 ITTF (tpc only)",kFALSE},
  {"B2006g"      ,""       ,"","ry2006g,in,tpc_daq,tpcI,Idst,l0,tags,Tree,evout","",""
                                                       ,"Base chain for 2006 ITTF geo g (tpc only)",kFALSE},
#endif

  {"pp2006a"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006a,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr3",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006b"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006b,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,BeamBack",
                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  {"pp2006g"      ,"" ,"",     // added 2008 after geometry corrections
   "B2006g,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4,BeamBack",
          "","","Production chain for 2005 pp data geo g (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},



  // Year 7 chains - Geometry 2007 hopefully fine
  {"T2007","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,Tree,evout",
                                                                 "","","TPC only chain,  2007 ITTF",kFALSE},
  {"B2007","","","ry2007,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,spt,Idst,l0,tags,Tree,evout",
                                                     "","","Base chain for 2007 ITTF (tpc+svt+ssd)",kFALSE},
  {"B2007g","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,spt,Idst,l0,tags,Tree,evout",
                                               "","","Base chain for 2007 ITTF geo g (tpc+svt+ssd)",kFALSE},
  {"P2007"       ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5",
                   "","","Production chain for 2007 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  {"P2007g"      ,"" ,"",   // chain was set in 2008 to account for missing material
   "B2007g,IAna,KeepSvtHit,hitfilt,VFMinuit2,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5",
     "","","Production chain for 2007 data, revised 2008 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

   
  // startup for calib
  {"P2007a"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr3",
             "","","Production chain for 2007 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2007b"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr4",
             "","","Production chain for 2007 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},


  //  {"testing"      ,"" ,"",   // just a damned test
  //   "B2006b,sdt20061211,fcf,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,Corr4",
  //                "","","Production chain for 2005 pp data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  // 
  {"B2008" ,"","","ry2008,in,tpc_daq,tpcI,fcf,Idst,tags,Tree,evout","","",
                                                                   "Base chain for 2008 ITTF (tpc)",kFALSE},
  {"B2008a","","","ry2008,in,tpcX,ToFx,tpcDB,TpcHitMover,Idst,tags,Tree,evout","","",
                                                               "Base chain for 2008 ITTF (tpc+tof)",kFALSE},
  // startup for calib
  {"P2008a"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr3,analysis",
             "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008b"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
             "","","Production chain for 2008 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  // or VFPPVnoCTB
  {"pp2008a"      ,"" ,"",   
   "B2008,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
             "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
 
  {"P2008c"       ,"" ,"",   // ATTENTION: the below chain was used for preliminary results on low energy
   "B2008,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
                   "","","Production chain for 2008 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  
  {"pp2008c"      ,"" ,"",  // Note: this chain was not used and may be removed
   "B2008,IAna,hitfilt,ppOpt,Minuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
             "","","Production chain for 2008 data Corr4 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},

  // convergence chains
  {"pp2008"     ,"" ,"",   // VFPPV was chosen for p+p as final production chain
   "B2008a,IAna,hitfilt,ppOpt,VFPPV,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
             "","","Production chain for 2008 data Corr3 (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},   
  {"P2008"       ,"" ,"",  // this one is final and official production ready, June 2008
   "B2008a,IAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis",
                   "","","Production chain for 2008 data (+ l3, tof, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
   
  //
  // Chains for 2009 run p+p essentially
  // Note that we always need to start with VFMinuit as VFPPV is full of asserts
  //
  {"B2009.1","","","ry2009,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","","",
                                                                   "Base chain for 2009 ITTF (tpc)",kFALSE},
  {"B2009.2","","","ry2009,in,tpcX,ToFx,tpcDB,TpcHitMover,Idst,tags,Tree,evout","","",
                                                               "Base chain for 2009 ITTF (tpc+tof)",kFALSE},
  {"pp2009a"      ,"" ,"",   
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis",
              "","","Production chain for 2009 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2009b"      ,"" ,"",   
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,l3onl,emcDY2,fpd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis",
        "","","Production chain for 2009 data - no Corr (+ l3, bcc/fpd, ftpc, e/b-emc, no trigger)",kFALSE},




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
                    "","","Pass0 SpaceCharge evaluator with GridLeak, no geo or tracker dependence",kFALSE},

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
  {"dbSnapshot"  ,""  ,"",""                                         ,"","","Create?use dbSnapshot",kFALSE},
  {"NoEvent"     ,""  ,"","-event,-analysis"      ,"","","Switch Off StEvent and StAnalysis Makers",kFALSE},
  {"MakeDoc"     ,""  ,"",""                   ,"","","Make HTML documentation for the given Chain",kFALSE},
  {"Debug"       ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug1"      ,""  ,"",""                                            ,"","","Set debug flag = 1",kFALSE},
  {"Debug2"      ,""  ,"",""                                            ,"","","Set debug flag = 2",kFALSE},
  {"IdTruth"     ,""  ,"",""              ,"","","Enable IdTruth association in StAssociationMaker",kFALSE},
  {"useInTracker",""  ,"","","",""    ,"switch from EGR to Sti global tracks in StAssociationMaker",kFALSE},
  {"noRepeat"    ,""  ,"",""                                        ,"","","No repeat in Messenger",kFALSE},
  {"noHistos"    ,""  ,"",""                                    ,"","","Disables Attributes histos",kFALSE},
  {"Higz"        ,""  ,"",""                                               ,"","","Pop Higz window",kFALSE},
  {"big"         ,""  ,"",""                                         ,"","","Set NwGEANT =20Mwords",kFALSE},
  {"bigbig"      ,""  ,"",""                                         ,"","","Set NwGEANT =40Mwords",kFALSE},
#ifdef __BFC2__
  {"clearmem"    ,""  ,"",""                           				  ,"","","Obsolete",kFALSE},
#endif /* __BFC2__ */
  {"adcOnly"     ,""  ,"",""                          ,"","","DAQMaker selects only TPC ADC events",kFALSE},
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
  {"DbRichSca"   ,""  ,"","detdb","","",                    "Force reading of Rich scalers from DB",kFALSE},

    
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
  {"min2trkVtx"     ,""  ,"","",""                 ,"","...only 2 tracks needed for vertex finding",kFALSE},

  // WARNING: introduction of usePct4Vtx with default:false breaks backward compatibility.
  // See related code in StBFChain.cxx for details
  {"usePct4Vtx"     ,""  ,"","",""                ,"","Use Post-Crossing Tracks for vertex finding",kFALSE},

  {"svt1hit",""  ,"","",""                                     ,"","Use 1 SVT hit only combination",kFALSE},

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
  {"VFMinuit2"      ,""  ,"","genvtx","",  "","... Generic VF will use Minuit method w/rank mode 2",kFALSE},
  {"VFMinuit3"      ,""  ,"","genvtx","",  "","... Generic VF will use Minuit method w/rank mode 3",kFALSE},
  {"VFFV"           ,""  ,"","genvtx",""                            ,"","... Fixed dummy VF method",kFALSE},
  {"VFMCE"          ,""  ,"","genvtx",""                        ,"","... Fixed vertex from MCEvent",kFALSE},

  {"VFppLMV"        ,""  ,"","genvtx",""                 ,"","...VertexMaker will use ppLMV method",kFALSE},
  {"VFppLMV5"       ,""  ,"","genvtx",""         ,"","...VertexMaker will use ppLMV method (tuned)",kFALSE},
#endif /* __BFC2__ */


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
  {"Embedding"   ,"","","-Simu"                                              ,"","","Embedding run",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Geometry+Mag","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"pgf77"    ,"" ,"","",""                                                   ,"pgf77VMC","Fortran",kFALSE},
  {"minicern"    ,"" ,"","",""                                           ,"StarMiniCern","minicern",kFALSE},
  {"mysql"    ,"" ,"","",""                                                  ,"mysqlclient","MySQL",kFALSE},
  {"libPhysics"    ,"" ,"","",""                                            ,"libPhysics","TVector",kFALSE},
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
  {"rts"         ,""  ,"","",""                                                ,"RTS","load libRTS",kFALSE},
  {"daq"         ,""  ,"","rts",""                         ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                     ,"libGeom,StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TRGDef"      ,""  ,"","",""                          ,"StTriggerDataMaker","Load StTriggerData",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"BTofUtil"    ,""  ,"","",""                                     ,"StBTofUtil","Load StBTofUtil",kFALSE},
  {"StBichsel"   ,""  ,"","",""                         ,"StBichsel","Load Bichsel model for dE/dx",kFALSE},
  {"StEvent"     ,""  ,"","globT,SCL,TRGDef,StBichsel,EmcUtil",""         ,"StEvent","Load StEvent",kFALSE},
  {"SsdUtil"     ,""  ,"","StarMagField,StEvent",""            ,"libGeom,StSsdUtil","Load SSD Util",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T,geomT,StDbT",""                      ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"EEmcUtil"    ,""  ,"","",""                                     ,"StEEmcUtil","Load StEEmcUtil",kFALSE},
  {"l3Util"      ,""  ,"","",""                                         ,"Stl3Util","Load Stl3Util",kFALSE},
  {"PmdUtil"     ,""  ,"","","",                                       "StPmdUtil","Load StPmdUtil",kFALSE},
  {"QUtils"      ,""  ,"","PmdUtil,EmcUtil","",                      "","Load QA Libs dependencies",kFALSE},
  {"MuDSTDeps"   ,""  ,"","StEvent","","StEventUtilities,StStrangeMuDstMaker,Tree",
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
  {"db"          ,"db"   ,"","StDbT"             ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"detDb","detDb","","db","StDetectorDbMaker","StDetectorDbMaker","Load and run StDetectorDbMaker",kFALSE},
  {"magF"        ,"MagField","","StDbT,db","StMagFMaker","StarMagField,StMagF"
                                                         ,"Mag.field map with scale factor from Db",kFALSE},
  {"dbutil"      ,""     ,"","detDb,StDbT"                 ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"tpcDB"       ,"tpcDB","","tpc_T,dbutil,db"                         ,"StTpcDbMaker","StTpcDb","",kFALSE},
  {"svtDb"       ,"svtDb","","tpcDb,SvtCL", "StSvtDbMaker","StSvtDbMaker","Load and run SvtDbMaker",kFALSE},
  {"ssdDb"    ,"ssdDb","","tpcDb,SsdUtil","StSsdDbMaker","StSsdDbMaker","Load and run StSsdDbMaker",kFALSE},
  {"eemcDb"      ,"eeDb" ,"","db",                               "StEEmcDbMaker","StEEmcDbMaker","",kFALSE},
  {"trgd"        ,"trgd","","TRGDef"  ,"StTriggerDataMaker","StTriggerDataMaker","Get trigger data",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
//{"Event","","",",StEvent,tpcDB,detDb","StEventMaker",",StEventMaker","<StEvent creation/filling>",kFALSE},
//{"MakeEvent","","","Event",""                             ,"","WARNING *** Option is OBSOLETE ***"kFALSE},
// for simulation on fly Event time stamp is set outside of the simulation makers
  {"ntin"        ,"geant"  ,"","paw,-fzin,-geant,-gstar,Simu,geantL,paw","St_geant_Maker",
                                                       "gstar","read event generated Hbook nt-file",kFALSE},
  {"PrepEmbed","","","geantEmb","StPrepEmbedMaker","St_geant_Maker"
                                                                ,"Prepare kinematics for embedding",kFALSE},
  {"geant"       ,"geant","","geantL"                          ,"St_geant_Maker","","passive GEANT",kFALSE},
  {"geantEmb"    ,"geant","","geantL"                   ,"St_geant_Maker","","GEANT embedding mode",kFALSE},
  {"RootVMC","","" ,"-geant,-fzin,-ntin,StarMagField,-geantL,-geometry,-geomNoField,minicern","",
                                                          "Geom,VMC,EG,Pythia6,EGPythia6,geant3","",kFALSE},
  {"VMCAppl"  ,"","","geomT,gen_t,sim_T,RootVMC",""                  ,"StarVMCApplication","VMC G3",kFALSE},
  {"VMC"         ,"geant","","Simu,VMCAppl,-geant","StVMCMaker",           "StVMCMaker","VMC Maker",kFALSE},
  {"VMCPassive"  ,"geant","","VMCAppl"    ,"StVMCMaker",   "StVMCMaker","VMC Maker in Passive Mode",kFALSE},
  {"StMcEvent"   ,"","","gen_t,sim_T"                                            ,"","StMcEvent","",kFALSE},
  {"McEvent"     ,"","","StEvent,EEmcUtil,EmcUtil,StMcEvent","StMcEventMaker"  ,"StMcEventMaker","",kFALSE},
  {"MakeEvent","0Event","","StEvent,tpcDB,detDb","StEventMaker","StEventMaker",
                                                                         "<Early StEvent creation>",kFALSE},

#ifndef __CLEANUP__ 
  {"l0"          ,"l0Chain","","trg_T,globT,trg"                            ,"StMaker","StChain","",kFALSE},
#else
  {"l0"          ,"l0Chain","","globT,ctf,trg"                              ,"StMaker","StChain","",kFALSE},
#endif
  {"ctf"         ,"ctf","l0Chain","ctf_T,db" ,"St_ctf_Maker","St_ctf,St_ctf_Maker","ToF simulation",kFALSE},
#ifndef __CLEANUP__
  {"mwc"         ,"mwc","l0Chain","mwc_T,db,tpcDB"         ,"St_mwc_Maker","St_mwc,St_mwc_Maker","",kFALSE},
#else
  {"mwc"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
  {"trg"         ,"trg","l0Chain","trg_T,globT,db","St_trg_Maker","St_trg,St_trg_Maker"
                                                        ,"trigger analysis for Year 2001-2005 data",kFALSE},
#ifndef __CLEANUP__
  {"ppMCTrig"    ,"ppMC_trig1","l0Chain",""
                         ,"StppTrigMaker","StppSpin","Add emulation of pp Trigger based on CTB+MWC",kFALSE},
#else
  {"ppMCTrig"    ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},
#endif
    
  {"tpc"         ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,tcl,tpt,PreVtx"   ,"StMaker","StChain","",kFALSE},
  {"tpcI" ,"tpcChain","","tpc_T,globT,tls,db,tpcDB,TpcHitMover","StMaker","StChain","tpc with ITTF",kFALSE},
  {"tpcX" ,"tpcChain","","-tpcI,tpx,MakeEvent"            ,"StMaker","StChain","tpc+tpcx with ITTF",kFALSE},
  {"Trs"         ,"Trs","tpcChain","scl,tpcDB,TrsToF,StEvent"         ,"StTrsMaker","StTrsMaker","",kFALSE},
  {"TpcRS"    ,"","tpcChain","scl,tpcDB,-Trs,Simu","StTpcRSMaker","StTpcRSMaker"
                                                                      ,"New Tpc Response Simulator",kFALSE},
  {"Mixer"       ,"tpc_raw","","daq","StMixerMaker"                   ,"StTrsMaker,StMixerMaker","",kFALSE},
  {"St_tpc"      ,"","","tpc_T,tpcDb"                                               ,"","St_tpc","",kFALSE},
  {"St_svt"      ,"","","svt_T,tls,svtDb"                                           ,"","St_svt","",kFALSE},
  //  {"StGlobal"     ,"","","globT",                                                 ,"","St_global","",kFALSE},
  {"tpc_daq"  ,"tpc_raw","tpcChain","detDb,tpc_T","St_tpcdaq_Maker","StTrsMaker,St_tpcdaq_Maker","",kFALSE},
  {"tcl"         ,"tpc_hits","tpcChain","tls,St_tpc,StEvent,tpc_daq,-fcf","St_tcl_Maker","St_tcl_Maker", 
                                                                        "Cluster Finder (from raw)",kFALSE},
  {"fcf","","tpcChain","daq,-tcl,tpc_daq,StEvent","StRTSClientFCFMaker","StRTSClientFCF,StRTSClientFCFMaker",
                                                                       "Offline FCF Cluster finder",kFALSE},
  {"tfs"         ,"tpc_hits","tpcChain","MakeEvent,Simu,-trs,-tcl,-fcf,-tpc_daq,tls,St_tpc,StEvent" 
                                           ,"St_tfs_Maker","St_tcl_Maker","use tfs (no StTrsMaker)",kFALSE},
  {"tpx"         ,"tpc_hits","tpcChain","MakeEvent,-trs,-tcl,-fcf,-tpc_daq,-tfs,-St_tpc,StEvent,rts,detDb" 
                     ,"StTpcHitMaker","StTpcHitMaker","TPC hit reader for tpc + tpx via EVP_READER",kFALSE},
  {"TpxPulser","TpxPulser","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker","TPC+TPX pulser analysis"
                                                                                                  , kFALSE},
  {"TpxPadMonitor","TpxPadMonitor","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker",
                                                                             "TPC+TPX pad monitor", kFALSE},
  {"TpxDumpPxls2Nt","TpxDumpPxls2Nt","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
                                                                   ,"TPC+TPX pixel dump to NTuple", kFALSE},
  {"TpxRaw","TpxRaw","tpcChain","rts,detDb,StEvent"
   ,                                        "StTpcHitMaker","StTpcHitMaker","TPC+TPX Tpc Raw Data", kFALSE},
  {"TpcMixer","","tpcChain","StEvent,rts,-Mixer"              ,"StTpcMixerMaker","StTpcHitMaker","",kFALSE},
  {"TpxClu","tpc_hits","tpcChain","rts,detDb,-tpx","StTpcRTSHitMaker","StTpcHitMaker"
                                                                       ,"RTS(online) cluster maker",kFALSE},
  {"Velo"        ,"","tpcChain","tpc_T,tls"                         ,"StVeloMaker","StVeloMaker","",kFALSE},
#ifndef __CLEANUP__  
  {"TpcHitFilter","tpc_hit_filter","tpcChain",""    ,"StTpcHitFilterMaker","StTpcHitFilterMaker","",kFALSE},
#else
  {"TpcHitFilter","","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"TpcHitMover" ,"tpc_hit_mover","tpcChain","StEvent",
                      "StTpcHitMover","StTpcHitMoverMaker","TPC hits coord transform + corrections",kFALSE},
  {"tpt"   ,"tpc_tracks","tpcChain","tls,St_tpc,TpcHitMover",      "St_tpt_Maker","St_tpt_Maker","",kFALSE},
  {"tpt_old"     ,"tpc_tracks","tpcChain","St_tpc,tls",            "St_tpt_Maker","St_tpt_Maker","",kFALSE},
  {"TpcT0"  ,"TpcT0","","ctf_T,ftpcT,tls,St_tpc,St_svt,tpc_daq,kalman,StEvent,MuDSTDeps,","StTpcT0Maker",
               "St_tcl_Maker,St_tpt_Maker,St_global,St_dst_Maker,StMuDSTMaker,StPass0CalibMaker","",kFALSE},
#ifndef __CLEANUP__ 
  {"ChargeStep","","","tpc_T,globT,tls,db,tpcDB,tpc_daq","StChargeStepMaker","StChargeStepMaker","",kFALSE},
#else
  {"ChargeStep","","","",                                "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"laser"       ,"tpc_tracks","LaserTest,tpcChain","tpc_daq,tpt"
                                           ,"StLaserEventMaker","StLaserEvent,StLaserEventMaker","",kFALSE},
  {"PreVtx"      ,"","tpcChain","tpt,SCL,sim_T,St_tpc,St_svt,ftpcT,ctf_T",
                                                     "StPreVertexMaker","St_global,St_dst_Maker","",kFALSE},
#ifndef __BFC2__ 
  {"svt"         ,"svtChain","","svt_T,SvtCL,Est"                           ,"StMaker","StChain","",kFALSE},
#else
  {"svt"         ,"svtChain","","svt_T,SvtCL"                               ,"StMaker","StChain","",kFALSE},
#endif
  {"svt_daq"     ,"svt_raw","svtChain","daq,SvtCL"              ,"StSvtDaqMaker","StSvtDaqMaker","",kFALSE},
  {"sss"         ,"","","SvtSlowSim"                              ,"","","Short cut for SvtSlowSim",kFALSE},
  {"SvtSlowSim"  ,"","","SvtSSim,SvtOnlSeq"         ,"","","Short cut for SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtSSim","SvtSSimu","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                  ,"StSvtSimulationMaker","StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
    
  {"SvtEmbed"  ,"","","SvtSSim,SvtEm,SvtOnlSeq"      ,"","","Short cutfor SvtSlowSim and SvtOnlSeq",kFALSE},
  {"SvtEm","SvtEm","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit","StSvtEmbeddingMaker",
                                                          "StSvtSimulationMaker,StSvtCalibMaker","",kFALSE},
  {"SvtOnlSeq"   ,"SvtOnlSeq","svtChain","svtDb,SvtCL,Simu,SvtSeqAdj,SvtClu,SvtCluAnal,SvtHit"
                                             ,"StSvtOnlineSeqAdjSimMaker","StSvtSimulationMaker","",kFALSE},
    
  {"srs","svt_hits","svtChain","svtDb,tls,Simu,St_tpc,St_svt,SvtCL,-sss,-SvtSlowSim,StEvent,MakeEvent"
                                                ,"St_srs_Maker","StSvtClusterMaker,St_srs_Maker","",kFALSE},
  {"sfs","svt_hits","svtChain","svtDb,Simu,St_svt,SvtCL,-sss,-srs,-SvtSlowSim,StEvent,MakeEvent"
                                                                  ,"St_sfs_Maker","St_srs_Maker","",kFALSE},
  {"SvtSeqAdj"   ,"SvtSeqAdj","svtChain","SvtCL"          ,"StSvtSeqAdjMaker","StSvtSeqAdjMaker","",kFALSE},
  {"SvtClu"   ,"SvtClu","svtChain","svt_T,StEvent,SvtCL","StSvtClusterMaker","StSvtClusterMaker","",kFALSE},
  {"SvtCluAnal" ,"SvtCluAnal","svtChain","SvtCL","StSvtClusterAnalysisMaker","StSvtClusterMaker","",kFALSE},
  {"SvtHit"      ,"svt_hits","svtChain","SvtCL"             ,"StSvtHitMaker","StSvtClusterMaker","",kFALSE},
#ifndef __CLEANUP__  
  {"SvtVtx"      ,"SvtVtx","SvtChain",""           ,"StSvtVertexFinderMaker","StSvtClusterMaker","",kFALSE},

  {"stk"        ,"svt_tracks","svtChain","tls,St_tpc,St_svt,SvtCL","St_stk_Maker","St_stk_Maker","",kFALSE},
  {"Est"         ,"","svtChain","St_svt"                    ,"StEstMaker","St_global,StEstMaker","",kFALSE},
#else
  {"SvtVtx"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"stk"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Est"         ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
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
  {"ssddat"      ,"","","ssd_daq"                             ,"","","SSD full chain for Real Data",kFALSE},
  /*  {"ssd_daq","SpaStrip","","daq,ssddb,St_svt,-sls,-spa,ssdUtil","StSsdDaqMaker","StSsdDaqMaker"
      ,"... SSD Daq",                                                                                  kFALSE}, */
  {"ssd_daq","","","ssddb,St_svt,-sls,-spa,ssdUtil","StSsdDaqMaker" ,"StSsdDaqMaker", "... SSD Daq",kFALSE},
  {"ssdfast"     ,"","","ssdDb,StMcEvent,StEvent","StSsdFastSimMaker","StSsdFastSimMaker",
   "... SSD fast simulator"                                                                        ,kFALSE},
  {"ssd"         ,"","","sls,spa,spt"                        ,"","","SSD full chain for simulation",kFALSE},
  {"sls"         ,"","","McEvent,tls,Simu,St_tpc,St_svt,SvtCL","St_sls_Maker","StSsdSimulationMaker",
                                                                           "... SSD slow simulator",kFALSE},
  {"spa"         ,"SpaStrip","","tls,Simu,St_tpc,St_svt,SvtCL,ssdUtil","St_spa_Maker","StSsdSimulationMaker",
                                                                     "... SSD Pedestal Annihilator",kFALSE},
  {"SsdEmbed"   ,"","","","StSsdEmbeddingMaker","StSsdSimulationMaker","... SSD Mixing geom Maker" ,kFALSE},
  {"spt"        ,"","","St_svt,ssdUtil","StSsdPointMaker","StSsdPointMaker","... SSD Point Creator",kFALSE},
  {"ssdpre"      ,"","","ssdEmbed,spa"                    ,"","","SSD full chain for pre-embedding",kFALSE},/*this option works*/
  {"ssdAdd"     ,"","","ssd_daq","StSsdAddMaker","StSsdAddMaker",             "... SSD merge maker",kFALSE},
  {"ssdE"        ,"","","ssdpre,ssdAdd"                       ,"","","SSD full chain for embedding",kFALSE},
  //{"ssdE"        ,"","","ssdEmbed,spa,spt"                       ,"","","SSD full chain for embedding",kFALSE},

  {"emcDY2"   ,"emcRaw","emcY2",
     "daq,eemcDb,EEmcUtil,emc_T,EmcUtil,StEvent,PreEcl,Epc","StEmcRawMaker","StEmcRawMaker",
                                                                        "B/E EMC data common maker",kFALSE},
  {"emcAtoE"  ,"","" ,"db,emcDY2","StEmcADCtoEMaker","StEmcADCtoEMaker", "B-EMC ADC to E converter",kFALSE},
  {"eemcD"       ,"","","","","",                              "WARNING *** Option is OBSOLETE ***",kFALSE},
  {"ZDCVtx"      ,"","","db"                              ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},

  {"emcY2"    ,"emcY2","","emc_T,tpc_T,db,emcSim,PreEcl,epc"      ,"StMaker","StChain",
                            "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim"   ,"","emcY2","emc_T,EmcUtil,StMcEvent,MuDST","StEmcSimulatorMaker","StEmcSimulatorMaker",
                                                                           "New simulator for BEMC",kFALSE},
  {"EEfs" ,"eefs","","db,EEmcUtil,MuDst",
                                     "StEEmcFastMaker","StEEmcSimulatorMaker","EEMC fast simulator",kFALSE},
  {"StiLibs","","","StarMagField,StiTpcLib,StiSvtLib,StiSsdlib,StiRnDLib,StiUtil","",
                                                                       "","ITTF:load Sti libraries",kFALSE},
  {"StiTpcLib","","","tpcDB","",                          "Sti,StiTpc","Sti Tpc related libratries",kFALSE},
  {"StiSvtLib","","","svtDB","",        "Sti,StSvtClassLibrary,StiSvt","Sti Svt related libratries",kFALSE},
  {"StiSsdLib","","","ssdDB","",                "Sti,StSsdUtil,StiSsd","Sti Ssd related libratries",kFALSE},
  {"StiRnDLib","","","","",                               "Sti,StiRnD","Sti RnD related libratries",kFALSE},
  {"laserIT" ,"","","","",                               "TpcIT","use Sti for laser reconstruction",kFALSE},
  {"TpcIT"       ,""  ,"","TpcDb,StiLibs",""                       ,"","ITTF: track using TPC geom",kFALSE},
  {"NoSvtIT"     ,""  ,"","-SvtIT",""                    ,"","ITTF: track with switch off SVT geom",kFALSE},
  {"NoSsdIT"     ,""  ,"","-SsdIT",""                    ,"","ITTF: track with switch off SSD geom",kFALSE},
  {"SvtIT"       ,""  ,"","svtDb,StiLibs",""                       ,"","ITTF: track using SVT geom",kFALSE},
  {"SsdIT"       ,""  ,"","ssdDb,StiLibs",""                       ,"","ITTF: track using SSD geom",kFALSE},
  {"HpdIT"       ,""  ,"","StiLibs",""                             ,"","ITTF: track using Hpd geom",kFALSE},
  {"PixelIT"     ,""  ,"","StiLibs",""                           ,"","ITTF: track using Pixel geom",kFALSE},
  {"IstIT"       ,""  ,"","StiLibs",""                             ,"","ITTF: track using Ist geom",kFALSE},
  {"skip1row"    ,""  ,"","",""                           ,"","ITTF: skip the first pad row in TPC",kFALSE},
  {"genvtx"   ,"","","EEmcUtil,ctf","StGenericVertexMaker","Minuit,Sti,StGenericVertexMaker"
                                                                           ,"Generic Vertex Finder",kFALSE},
  {"StiUtil"  ,"","","",                              "","StiUtilities","Load StiUtilities library",kFALSE},
  {"StiRnD"   ,"","","Sti",                               "","StiRnD", "Load StiRnD shared library",kFALSE},
  {"Sti"      ,"Sti","","SCL,StEvent,StDbT,TpcIT,StiUtil","StiMaker",
                                                    "StEventUtilities,Sti,StiMaker" ,"ITTF tracker",kFALSE},
  {"StiPulls" ,"","","Sti",                                      "","", "Request to make Sti Pulls",kFALSE},
  {"BeamBack" ,"","","StEvent","StBeamBackMaker","StBeamBackMaker"
                                                              ,"Beam background tracker in the TPC",kFALSE},
  {"dEdxY2"       ,"dEdxY2","","tpcDb,StEvent","StdEdxY2Maker","libMinuit,StdEdxY2Maker",
                                                                     "Bichsel method used for dEdx",kFALSE},

  // needs to be done after the tracker
  {"FindVtxSeed"   ,"FindVtxSeed"   ,"","MuDSTDeps","StVertexSeedMaker",  
   "St_global,St_dst_Maker,StMuDSTMaker,StPass0CalibMaker",          "Performs vertex seed finding",kFALSE},
  {"FindEvtVtxSeed","FindEvtVtxSeed","","MuDSTDeps","StEvtVtxSeedMaker",
  "StStrangeMuDstMaker,StMuDSTMaker,StPass0CalibMaker","Performs vertex seed finding using StEvent",kFALSE},


  {"Ftpc"      ,"ftpcChain"  ,"","ftpcT,fcl,fpt"                            ,"StMaker","StChain","",kFALSE},
  {"fss"       ,"ftpc_raw","ftpcChain","SCL,Simu",
"StFtpcSlowSimMaker","StFtpcSlowSimMaker,StFtpcTrackMaker,StFtpcClusterMaker","FTPC Slow simulator",kFALSE},
  {"Fcl"       ,"ftpc_hits","ftpcChain","SCL,daq","StFtpcClusterMaker",
                                        "StFtpcTrackMaker,StFtpcClusterMaker","FTPC cluster finder",kFALSE},
  {"fpt"      ,"ftpc_tracks","ftpcChain","SCL"
                                          ,"StFtpcTrackMaker","StFtpcTrackMaker","FTPC Track Maker",kFALSE},
  {"fgain"     ,"","","fcl,fpt","","",
                        "StFtpcClusterMaker and StFtpcTrackMaker will produce gain scan histograms",kFALSE},
  {"fdbg"     ,"","","fcl,fpt","","","StFtpcClusterMaker and StFtpcTrackMaker will write debugfile",kFALSE},
  {"flaser"    ,"","","fpt"                              ,"","","StFtpcTrackMaker in LASERTRACKING",kFALSE},


  {"pmdReco"   ,"pmdReco","","PmdUtil,pmdRead,pmdClust"       ,"StMaker","StChain","PMD Reco chain",kFALSE},
  {"pmdRaw"    ,"pmdRaw","","pmdReco"                        "","","PMD Reco chain giving raw data",kFALSE},
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
#else
  {"V0svt"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
  {"Xisvt"       ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"SCEbyE"      ,"scebye","","MuDSTDeps","StSpaceChargeEbyEMaker","StMuDSTMaker,StPass0CalibMaker",
                                                         "Determine EbyE SpaceCharge using StEvent",kFALSE},
  {"SCScalerCal" ,"scscalercal","","MuDSTDeps","StSpaceChargeEbyEMaker","StMuDSTMaker,StPass0CalibMaker",
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
  {"ToFx"      ,"TofChain","","tofXDat,tofrMatch,tofCalib"        ,"StMaker","StChain","ToFx Chain",kFALSE},
  {"tofDat"    ,"tof_raw","TofChain","db,Tofutil","StTofMaker","StEvent,StTofMaker",
                                                                              "TOF Data base chain",kFALSE},
  {"tofXDat"   ,"tof_raw","TofChain","db,Tofutil","StTofHitMaker","StEvent,StTofMaker,StTofHitMaker",
                                                                                    "TOF hit maker",kFALSE},
  {"BtofDat"   ,"tof_raw","BTofChain","db,BTofutil","StBTofHitMaker","StEvent,StBTofHitMaker",
                                                                                   "BTOF hit maker",kFALSE},
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
  {"compend"     ,"","","event,detDb","StEventCompendiumMaker","StEventCompendiumMaker",
                                                                 "Fill event summary in ITTF Chain",kFALSE},
#endif /* __BFC2__ */
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra" ,"","",""                            ,"StRichSpectraMaker","StRichSpectraMaker","",kFALSE},
    
  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
#ifndef __CLEANUP__  
  {"TpcTag"      ,"","TagsChain",""                             ,"StTpcTagMaker","StTpcTagMaker","",kFALSE},
#else
  {"TpcTag"      ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
#endif
  {"Flow"        ,"","TagsChain","StEvent"         ,"StFlowMaker","StEventUtilities,StFlowMaker","",kFALSE},
#ifndef __CLEANUP__ 
  {"FlowTag"     ,"","TagsChain","StEvent,Flow"               ,"StFlowTagMaker","StFlowTagMaker","",kFALSE},
#else
  {"FlowTag"     ,"","","",                              "","","WARNING *** Option is OBSOLETE ***",kFALSE},   
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
  {"tags"        ,"","TagsChain",      "globT,Event,StrangeTags,SpectraTag,HeavyTags,PCollTag,HighPtTags"
                                           ,"StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"noTags"      ,"","","-tags,-StrangeTags,-SpectraTag,-HeavyTags,-PCollTag,-HighPtTags", 
                                                                              "","","Turn Off tags",kFALSE},
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
                                      "StTpcMcAnalysisMaker","StTpcRSMaker,StTpcMcAnalysisMaker","",kFALSE},
  {"MiniMcEvent" ,"","","","",                   "StMiniMcEvent","Loads StMiniMcEvent library only",kFALSE},
  {"MiniMcMk"    ,"","","McAss,MiniMcEvent","StMiniMcMaker","StMiniMcMaker",
                                                                 "Creates tree in minimc.root file",kFALSE},
  {"SvtMatTree","","","","SvtMatchedTree",
                                  "StSvtPoolEventT,StSvtPoolSvtMatchedTree","Create SvtMatchedTree",kFALSE},
  {"LAna"        ,"","","in,detDb,StEvent,tpcDb","StLaserAnalysisMaker"
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
  {"QAalltrigs"  ,"", "","",                                     "","","Analyze all triggers in QA",kFALSE},
  {"HitFilt"     ,"", "","",               "StHitFilterMaker","StHitFilterMaker","Hit filter Maker",kFALSE},
  {"KeepTpcHit"  ,"", "","",                          "","","Keep all TPC hits in StHitFilterMaker",kFALSE},
  {"KeepSvtHit"  ,"", "","",                          "","","Keep all SVT hits in StHitFilterMaker",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"logger"      ,""  ,"",""            ,"","","Use log4cxx package to manage the program messages",kFALSE},
#ifndef __BFC2__
  {"ITTF"        ,""  ,"","",                               "","","Just to keep option ITTF==false",kFALSE},
#endif /* ! __BFC2__ */
  {"NoSimuDb"    ,""  ,"",""                                 ,"","","Switch off Simu Option for DB",kFALSE},
  {"NoOutput"    ,""  ,"","-Tree,-EvOut,noTags"                 ,"","","Suppress Output root files",kFALSE},
  {"EvOutOnly"   ,""  ,"","EvOut,Tree,noTags"                        ,"","","Only event.root files",kFALSE},
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE},
  {"Notpc_daq"   ,""  ,"","-tpc_daq"                                            ,"","","No tpc_daq",kFALSE},

  {"Calibration chains","------------","-----------","-----------------------------------","","","",kFALSE},
  {"LanaDV"      ,"","","MakeEvent,trgd,in,tpc_daq,tpcI,fcf,LaserIT,VFMinuit,Lana,Analysis,Corr4",
   "",""                                                                                 ,"get LDV",kFALSE},
  {"LanaDVtpx"   ,"","","MakeEvent,trgd,in,tpx,TpcHitMover,LaserIT,VFMinuit,Lana,Analysis,Corr4",
   "",""                                                                        ,"get LDV with TPX",kFALSE},
  {"LaserDV.Chain","","","in,LaserCal,fcf,TpcHitMover,OGridLeak3D,OShortR,OSpaceZ2","","","get LDV",kFALSE},

  {"Production chain from Db","------------","-----------","-----------------------------","","","",kFALSE},
#ifndef __BFC2__  /* non ITTF chains */
  {"tfs1b.Chain" ,"" ,"","tfs_year_1b"	                                                  ,"","","",kFALSE},
  {"tfs1a.Chain" ,"" ,"","tfs_year_1a"	                                                  ,"","","",kFALSE},
  {"tss1b.Chain" ,"" ,"","tss_year_1b"	                                                  ,"","","",kFALSE},
  {"tss1a.Chain" ,"" ,"","tss_year_1a"	                                                  ,"","","",kFALSE},
  {"tfs2a.Chain" ,"" ,"","tfs_year_2a"	                                                  ,"","","",kFALSE},
  {"tss2a.Chain" ,"" ,"","tss_year_2a"	                                                  ,"","","",kFALSE},
  {"tfs2y.Chain" ,"" ,"","tfs_year_2y"	                                                  ,"","","",kFALSE},
  {"tfs2x.Chain" ,"" ,"","tfs_year_2x"	                                                  ,"","","",kFALSE},
  {"tfs2b.Chain" ,"" ,"","tfs_year_2b"	                                                  ,"","","",kFALSE},
  {"tss2b.Chain" ,"" ,"","tss_year_2b"	                                                  ,"","","",kFALSE},
  {"tfs2a.Chain" ,"" ,"","tfs_year_2a"	                                                  ,"","","",kFALSE},
  {"tss2a.Chain" ,"" ,"","tss_year_2a"	                                                  ,"","","",kFALSE},
  {"tfs1b.Chain" ,"" ,"","tfs_year_1b"	                                                  ,"","","",kFALSE},
  {"tss1b.Chain" ,"" ,"","tss_year_1b"	                                                  ,"","","",kFALSE},
  {"trs1b.Chain" ,"" ,"","trs_year_1b"	                                                  ,"","","",kFALSE},
  {"tfs1b.Chain" ,"" ,"","tfs_year_1b"	                                                  ,"","","",kFALSE},
  {"tfs6c.Chain" ,"" ,"","tfs_complete"	                                                  ,"","","",kFALSE},
  {"tfs2s.Chain" ,"" ,"","tfs,y1b,-emc,eval,fzin,xout"           	                  ,"","","",kFALSE},
  {"tfs4b.Chain" ,"" ,"","tfs,y1b,eval,fzin,xout"	                                  ,"","","",kFALSE},
  {"tfs5b.Chain" ,"" ,"","tfs,y1b,eval,allevent,fzin,xout"	                          ,"","","",kFALSE},
  {"tdaq1.Chain" ,"" ,"","off,tdaq,tpc,HalfField,global,dst,event,analysis,tree,xout"	  ,"","","",kFALSE},
  {"tdaq2.Chain" ,"" ,"","off,tdaq,tpc,FieldOff,global,dst,event,analysis,tree,xout"	  ,"","","",kFALSE},
  {"tdaq3.Chain" ,"" ,"","off,tdaq,tpc,FieldOn,global,dst,event,analysis,tree,xout"	  ,"","","",kFALSE},
  {"tfs6b.Chain" ,"" ,"","tfs,cy1b,eval,big,fzin,xout"	                                  ,"","","",kFALSE},
  {"tfs1h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs6a.Chain" ,"" ,"","tfs,cy2a,eval,big,fzin"	                                  ,"","","",kFALSE},
  {"tdaq4.Chain" ,"" ,"","off,tdaq,tpc,global,eval,QA,QAC,dst,event,analysis,tree,xout"	  ,"","","",kFALSE},
  {"tfs2h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs7a.Chain" ,"" ,"","tfs,cy2a,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs3h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs4h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"p00h1.Chain" ,"" ,"","off,tdaq,tpc,FieldOff,Cdst,tags,big,tree,evout"	          ,"","","",kFALSE},
  {"p00h2.Chain" ,"" ,"","p00h"	                                                          ,"","","",kFALSE},
  {"p00h3.Chain" ,"" ,"","p00h,trg"	                                                  ,"","","",kFALSE},
  {"tfs8a.Chain" ,"" ,"","tfs,cy2a,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs5h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"tfs6h.Chain" ,"" ,"","tfs,mdc3,ev03,big,fzin,evout"	                                  ,"","","",kFALSE},
  {"p00h4.Chain" ,"" ,"","p00h"	                                                          ,"","","",kFALSE},
  {"tfs7h.Chain" ,"" ,"","tfs,mdc3,big,fzin"	                                          ,"","","",kFALSE},
  {"p00h5.Chain" ,"" ,"","p00h"	                                                          ,"","","",kFALSE},
  {"p00h6.Chain" ,"" ,"","p00h"	                                                          ,"","","",kFALSE},
  {"p00h7.Chain" ,"" ,"","p00h"	                                                          ,"","","",kFALSE},
  {"p00h8.Chain" ,"" ,"","p00h,ExB"	                                                  ,"","","",kFALSE},
  {"p00h9.Chain" ,"" ,"","p00h,-Kalman"	                                                  ,"","","",kFALSE},
  {"trs1i.Chain" ,"" ,"","trs,Simu,mdc3,big,evout,fzin"	                                  ,"","","",kFALSE},
  {"p00h10.Chain" ,"" ,"","p00h,ExB"	                                                  ,"","","",kFALSE},
  {"p00h11.Chain" ,"" ,"","p00h,ExB"	                                                  ,"","","",kFALSE},
  {"tfs2s.Chain" ,"" ,"","tfs,y1b,-emc,eval,fzin,xout"	                                  ,"","","",kFALSE},
  {"tfs1a.Chain" ,"" ,"","tfs,year_1a"	                                                  ,"","","",kFALSE},
  {"tfs2a.Chain" ,"" ,"","tfs,year_2a"	                                                  ,"","","",kFALSE},
  {"tss1b.Chain" ,"" ,"","tss,year_1b"	                                                  ,"","","",kFALSE},
  {"tfsftpc1h.Chain" ,"" ,"","tfs,ftpc,mdc3,evout,big,fzin"	                          ,"","","",kFALSE},
  {"trs2y.Chain" ,"" ,"","trs,Simu,srs,fss,rrs,C2001,GeantOut,big,evout,fzin"	          ,"","","",kFALSE},
  {"trs2pp.Chain" ,"" ,"","ppMDC4"	                                                  ,"","","",kFALSE},
  {"trsY2.Chain" ,"" ,"","MDC4"	                                                          ,"","","",kFALSE},
  {"trsY2v.Chain" ,"" ,"","PostMDC4"	                                                  ,"","","",kFALSE},
  {"p2000.Chain" ,"" ,"","P2000"	                                                  ,"","","",kFALSE},
  {"p2001f.Chain" ,"" ,"","ry2001,in,tpc_daq,tpc,rich,Physics,Cdst,Kalman,tags,Tree,evout,ExB"	
   ,"","",""                                                                                       ,kFALSE},
  {"p2001g.Chain" ,"" ,"","ry2001,in,tpc_daq,tpc,rich,l3onl,Physics,Cdst,Kalman,tags,Tree,evout,ExB"
   ,"","",""                                                                                       ,kFALSE},
  {"p2000i.Chain" ,"" ,"","ry2000a,in,tpc_daq,tpc,rich,Physics,Kalman,AlignSectors,Cdst,tags,Tree"
   ",evout,ExB,OBmap,OClock,OPr13,NoHits"                                                 ,"","","",kFALSE},
  {"p2001k.Chain" ,"" ,"",
   "ry2001,DbV1107,in,tpc_daq,tpc,rich,l3onl,tofDat,Physics,Kalman,AlignSectors,Cdst,tags,Tree,evout,ExB,"
   "OBmap,OClock,OPr13,OTwist,NoHits"                                                     ,"","","",kFALSE},
  {"p2001c.Chain" ,"" ,"",
   "ry2001,in,tpc_daq,tpc,rich,l3onl,Physics,Kalman,AlignSectors,Cdst,tags,Tree,evout,ExB,OBmap,OClock,"
   "OPr13,OTwist,NoHits"           	                                                  ,"","","",kFALSE},
  {"trshi.Chain" ,"" ,"","trs,Simu,mdc3,big,evout,fzin"	                                  ,"","","",kFALSE},
  {"trsgk.Chain" ,"" ,"","y2001,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,big,GeantOut,fzin"
   ,"","",""                                                                                       ,kFALSE},
  {"p2001e.Chain" ,"" ,"","ry2001,DbV1107,in,tpc_daq,tpc,rich,l3onl,tofDat,Physics,Kalman,AlignSectors,"
   "Cdst,tags,Tree,evout,emcDY2,ExB,OBmap,OClock,OPr13,Otwist"                            ,"","","",kFALSE},
  {"trsgl.Chain" ,"","","y2001,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,emcY2,big,GeantOut,fzin"
                                 	                                                  ,"","","",kFALSE},
  {"p2001l.Chain" ,"" ,"","ry2001,in,tpc_daq,tpc,rich,l3onl,tofDat,Physics,Kalman,AlignSectors,Cdst,tags,"
   "Tree,evout,ExB,OBmap,OClock,OPr13,OTwist,NoHits"                                      ,"","","",kFALSE},
  {"p2001f.Chain" ,"" ,"","ry2001,DbV1211,in,tpc_daq,tpc,rich,l3onl,tofDat,Physics,Kalman,AlignSectors,"
   "Cdst,tags,Tree,evout,emcDY2,ExB,OBmap,OClock,OPr13,OTwist,NoHits"                     ,"","","",kFALSE},
  {"p2000l.Chain" ,"" ,"","ry2000a,DbV1007,in,tpc_daq,tpc,rich,Physics,Kalman,AlignSectors,Cdst,tags,Tree,"
   "evout,ExB,OBmap,OClock,OPr13,NoHits"                                                  ,"","","",kFALSE},
  {"test chains","-----------","-----------","-------------------------------------------","","","",kFALSE},
  {"mkMuDst.Chain",   "", "","in,StEvent,tree,CMuDst,analysis,nodefault","","","Create MuDst from StEvent"
   ,                                                                                                kFALSE},
  {"p2002test.Chain" ,"" ,"","ry2001,in,tpc_daq,tpc,rich,ftpc,l3onl,tofDat,Physics,Kalman,AlignSectors,"
   "Cdst,tags,Tree,evout,ExB,OBmap,OClock,OPr13,OTwist,NoHits"                            ,"","","",kFALSE},
  {"pp2001fpd.Chain" ,"" ,"","pp2001,fpd,beamLine,NoHits"	                          ,"","","",kFALSE},
  {"p2001gb.Chain" ,"" ,"","DbV20020226,P2001,ftpc,emcDY2,NoHits"	                  ,"","","",kFALSE},
  {"trsgb.Chain" ,"" ,"","y2001,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,emcY2,big,GeantOut,"
   "fzin"                           	                                                  ,"","","",kFALSE},
  {"p2001gc.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,ftpc,emcDY2,NoHits"	          ,"","","",kFALSE},
  {"p2001gcMu.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,ftpc,emcDY2,EmcMDST,NoHits"	  ,"","","",kFALSE},
  {"pp2001test.Chain" ,"" ,"","DbV20020226,pp2001,fpd,beamLine"	                          ,"","","",kFALSE},
  {"p2001gdMu.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,ftpc,emcDY2,EmcMDST,NoHits"	  ,"","","",kFALSE},
  {"p2001gc2Mu.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,emcDY2,EmcMDST,NoHits"	          ,"","","",kFALSE},
  {"pp2001test2.Chain" ,"" ,"","DbV20020226,pp2001,svt_daq,SvtD,ftpc,fpd,beamLine,EmcMDST,NoHits"	                         
   ,"","",""                                                                                       ,kFALSE},
  {"p2001gd2Mu.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,emcDY2,EmcMDST,NoHits"	          ,"","","",kFALSE},
  {"pp2001ge.Chain" ,"" ,"","DbV20020402,pp2001a,fpd,beamLine,EmcMDst,CMuDst,NoHits"	  ,"","","",kFALSE},
  {"p2001gdMu.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,emcDY2,EmcMDST,NoHits"	          ,"","","",kFALSE},
  {"p2001ge.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,emcDY2,EmcMDST,CMuDst,NoHits"	  ,"","","",kFALSE},
  {"p2001ge2.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,ftpc,emcDY2,EmcMDST,CMuDst,NoHits"  ,"","","",kFALSE},
  {"pp2001svt.Chain" ,"" ,"","DbV20020520,pp2001,fpd,beamLine,svt_daq,SvtD,est,svtdedx,EmcMDst,CMuDst"	  
   ,"","",""                                                                                       ,kFALSE},
  {"trsge.Chain" ,"" ,"","y2001,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,emcY2,big,GeantOut,"
   "fzin"                        	                                                  ,"","","",kFALSE},
  {"trsgg1.Chain" ,"" ,"","y2001n,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,emcY2,big,GeantOut,"
   "fzin,sdt20010805"                                                                     ,"","","",kFALSE},
  {"trsgg2.Chain" ,"" ,"","y2001n,trs,Simu,rrs,tpc,rich,l0,ctf,Cdst,Kalman,tags,Tree,EvOut,emcY2,big,GeantOut,"
   "fzin,sdt20011125"            	                                                  ,"","","",kFALSE},
  {"p2001gg1.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,ftpc,emcDY2,EmcMDST,CMuDst,NoHits"  ,"","","",kFALSE},
  {"dedxDcl.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,-tcl,daqclf,CMuDst"	          ,"","","",kFALSE},
  {"ppdedxDcl.Chain" ,"" ,"","DbV20020802,pp2001a,fpd,beamLine,-tcl,daqclf,CMuDst"	  ,"","","",kFALSE},
  {"ppdedxDcl2.Chain" ,"" ,"","DbV20020802,pp2001a,fpd,beamLine,CMuDst"	                  ,"","","",kFALSE},
  {"dedxDclf2.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,CMuDst"	                          ,"","","",kFALSE},
  {"trsgh.Chain" ,"" ,"","y2001n,trs,Simu,rrs,srs,tpc,rich,l0,ctf,svt,ftpc,Cdst,Kalman,tags,Tree,EvOut,emcY2,"
   "big,GeantOut,fzin,sdt20011125"	                                                  ,"","","",kFALSE},
  {"p2001ge3.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,CMuDST,NoHits"	                  ,"","","",kFALSE},
  {"trsgh2.Chain" ,"" ,"","y2001n,trs,Simu,rrs,srs,tpc,rich,l0,ctf,svt,ftpc,Cdst,Kalman,tags,Tree,EvOut,emcY2,"
   "big,GeantOut,fzin"                                                                    ,"","","",kFALSE},
  {"test_nodaq100.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,CMuDst"	                  ,"","","",kFALSE},
  {"test_daq100.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,-tcl,daqclf,CMuDst"	          ,"","","",kFALSE},
  {"trsgi.Chain","","","trs,Simu,srs,rrs,y2001n,C2default,GeantOut,big,evout,fzin"        ,"","","",kFALSE},
  {"trsii.Chain" ,"" ,"","dAuMDC,tofsim,beamLine,CMuDST,fzin,-noHits"	                  ,"","","",kFALSE},
  {"dau_test.Chain" ,"" ,"","pp2001a,fpd,beamLine,CMuDST"	                          ,"","","",kFALSE},
  {"dau_test2003.Chain" ,"" ,"","dau2003,alltrigger,CMuDst"	                          ,"","","",kFALSE},
  {"ppEst.Chain" ,"" ,"","DbV20020802,pp2001a,fpd,beamLine,est,CMuDst,NoHits"	          ,"","","",kFALSE},
  {"dauProd1.Chain" ,"" ,"","dau2003,beamLine,hitfilt,CMuDst"	                          ,"","","",kFALSE},
  {"dauProd2.Chain" ,"" ,"","DbV20030220,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"dauProd3.Chain" ,"" ,"","DbV20030301,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"dauProd4.Chain" ,"" ,"","DbV20030307,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"ftpcTest2.Chain" ,"" ,"","DbV20030304,P2001,ftpc,ZDCvtx,CMuDst"	                  ,"","","",kFALSE},
  {"trsib.Chain" ,"" ,"","dAuMDCa,tofsim,beamLine,CMuDST,fzin,-noHits"	                  ,"","","",kFALSE},
  {"dauProd5.Chain" ,"" ,"","DbV20030322,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"dauProd6.Chain" ,"" ,"","DbV20030326,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"spchargTest.Chain" ,"" ,"","DbV20020226,P2001,ZDCvtx,emcDY2,EmcMDST,CMuDst"	          ,"","","",kFALSE},
  {"dauProd7.Chain" ,"" ,"","DbV20030408,dau2003,est,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"trsib2.Chain" ,"" ,"","dAuMDCa,tofsim,Eefs,beamLine,CMuDST,fzin,-noHits"	          ,"","","",kFALSE},
  {"ftpcTest.Chain" ,"" ,"","DbV20030304,P2001,ftpc,ZDCvtx,CMuDst"	                  ,"","","",kFALSE},
  {"ppTest.Chain" ,"" ,"","pp2003,beamLine,CMuDst"	                                  ,"","","",kFALSE},
  {"pptrsic.Chain" ,"" ,"","dAuMDCa,tofsim,Eefs,beamLine,CMuDST,fzin,-noHits"	          ,"","","",kFALSE},
  {"trsid.Chain" ,"" ,"","dAuMDCa,tofsim,Eefs,beamLine,CMuDST,fzin,-noHits"	          ,"","","",kFALSE},
  {"dauFpdProd.Chain" ,"" ,"","DbV20030523,dau2003a,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"ftpcProd2.Chain" ,"" ,"","DbV20030527,P2001,ftpc,ZDCvtx,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"ppTrProd.Chain" ,"" ,"","DbV20030603,pp2003,beamLine,est,eemcD,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"upcTest.Chain" ,"" ,"","pp2003,est,hitfilt,CMuDst"	                                  ,"","","",kFALSE},
  {"ppProd03.Chain" ,"" ,"","DbV20030703,pp2003,beamLine,est,eemcD,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"ftpcTest3.Chain" ,"" ,"","DbV20030801,dau2003a,beamLine,hitfilt,CMuDst"	          ,"","","",kFALSE},
  {"trsie.Chain" ,"" ,"",   "trs,y2003x,tpc,l0,ctf,svt,Cdst,Kalman,tags,Tree,bbcsim,evout,emcY2,eefs,GeantOut,"
   "big,fzin"                                                                             ,"","","",kFALSE},
  {"ppProd04.Chain" ,"" ,"","DbV20030628,pp2003,beamLine,est,eemcD,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"ppProd05.Chain" ,"" ,"","DbV20030723,pp2003,beamLine,est,eemcD,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"trsieFlow.Chain" ,"" ,"","trs,Simu,mdc3,GeantOut,big,CMuDst,fzin"	                  ,"","","",kFALSE},
  {"ppsvt2002.Chain" ,"" ,"",   "DbV20030808,pp2001a,fpd,est,beamLine,-xi,-v0,SvtHitFilt,HitFilt,"
   "xi2,XiSvt,svtdEdx,SvtMatchVtx,CMuDst"                                                 ,"","","",kFALSE},
  {"trsieFlow2.Chain" ,"" ,"","y2001n,C2default,trs,Simu,fss,rrs,big,GeantOut,CMuDst,fzin","","","",kFALSE},
  {"pphipt.Chain" ,"" ,"","dAuMDCa,tofsim,Eefs,beamLine,CMuDST,fzin,-noHits"	          ,"","","",kFALSE},
  {"auauFtpc.Chain" ,"" ,"","y2001n,trs,Simu,fss,rrs,C2default,GeantOut,CMuDst,big,fzin"  ,"","","",kFALSE},
  {"ppFPDProd.Chain" ,"" ,"","DbV20030603,pp2003,beamLine,est,eemcD,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"dEdxProd.Chain" ,"" ,"","DbV20030923,B2003,Corr2,ppOpt,-PreVtx,tofDat,svt_daq,SvtD,beamLine,CMuDst"	
   ,"","",""                                                                                       ,kFALSE},
  {"ppTuneProd.Chain" ,"" ,"","DbV20030723,B2003,ppOpt,-PreVtx,fpd,svt_daq,SvtD,trgd"	  ,"","","",kFALSE},
  {"dauFpdProd2.Chain" ,"" ,"","DbV20031016,dau2003a,l3onl,beamLine,hitfilt,CMuDst"	  ,"","","",kFALSE},
  {"dEdxProd2.Chain" ,"" ,"","DbV20030923,B2003,Corr2,ppOpt,-PreVtx,tofDat,svt_daq,SvtD,beamLine"
   ,"","",""                                                                                       ,kFALSE},
  {"daq100Test2.Chain" ,"" ,"","DbV20020802,P2001,ZDCvtx,-tcl,fcf,CMuDst"	          ,"","","",kFALSE},
  {"auauFtpc2.Chain" ,"" ,"","y2001n,trs,Simu,fss,rrs,C2default,GeantOut,CMuDst,big,fzin" ,"","","",kFALSE},
  {"ppLongRmProd.Chain" ,"" ,"","DbV20031016,B2003,ppOpt,-PreVtx,fpd,ftpc,emcDY2,eemcD,trgd,CMuDst"
   ,"","",""                                                                                       ,kFALSE},
  {"auauvniProd.Chain" ,"" ,"","C2000,trs,Simu,GeantOut,big,CMuDst,fzin"	          ,"","","",kFALSE},
  {"dauProd2.Chain" ,"" ,"","DbV20031114,dau2003a,est,l3onl,beamLine,-xi,-v0,SvtHitFilt,HitFilt,"
   "xi2,XiSvt,svtdEdx,SvtMatchVtx,eemcD,CMuDst"                                           ,"","","",kFALSE},
  {"ppProd06.Chain" ,"" ,"","DbV20031114,pp2003,est,l3onl,beamLine,-xi,-v0,SvtHitFilt,HitFilt,"
   "xi2,XiSvt,svtdEdx,SvtMatchVtx,eemcD,CMuDst"                                           ,"","","",kFALSE},
  {"trsihJ/psi.Chain" ,"" ,"",   "trs,Simu,y2003x,tpc,l0,ctf,svt,Cdst,Kalman,tags,Tree,bbcsim,evout,emcY2,"
   "eefs,GeantOut,big,CMuDst,fzin"                                                        ,"","","",kFALSE},
  {"auau62prod.Chain" ,"" ,"","trs,Simu,srs,y2004,tpc,l0,ctf,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,evout,"
   "est,-xi,-v0,xi2,XiSvt,svtdEdx,SvtMatchVtx,emcY2,eefs,GeantOut,big,CMuDst,fzin"        ,"","","",kFALSE},
  {"trs/omega2.Chain" ,"" ,"","trs,Simu,srs,y2003x,tpc,l0,ctf,svt,Cdst,Kalman,tags,Tree,bbcsim,evout,est,"
   "-xi,-v0,xi2,XiSvt,svtdEdx,SvtMatchVtx,emcY2,eefs,GeantOut,big,CMuDst,fzin"            ,"","","",kFALSE},
  {"daq100Test3.Chain" ,"" ,"","DbV20020802,pp2001,-tcl,fcf,beamline,CMuDst"	          ,"","","",kFALSE},
  {"auau62prod.Chain" ,"" ,"","trs,Simu,srs,fss,y2004,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin"               ,"","","",kFALSE},
  {"auau62prod2.Chain" ,"" ,"","DbV20040409,P2004,svt_daq,SvtD,-xi,-v0,EST,OShortR,OSpaceZ2,Xi2,XiSvt,"
   "svtdEdx,Kink2,eemcD,pmdRaw,NoHits,CMuDst"                                             ,"","","",kFALSE},
  {"auau62prod3.Chain" ,"" ,"","P2004,DbV20040415,OShortR,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,"
   "Kink2,CMuDst,ZDCvtx,hitfilt"                                                          ,"","","",kFALSE},
  {"calibProd.Chain" ,"" ,"",
   "P2004,DbV20040415,OShortR,svt_daq,svtD,EST,svtdEdx,Xi2,xiSvt,Kink2,CMuDst,ZDCvtx"
   ,"","",""                                                                                       ,kFALSE},
  {"omegaProd.Chain" ,"" ,"",
   "trs,Simu,srs,y2003x,tpc,l0,ctf,svt,Cdst,Kalman,tags,Tree,bbcsim,evout,est,xi2,XiSvt,svtdEdx,SvtMatchVtx,"
   "emcY2,eefs,GeantOut,big,CMuDst,fzin"                                                  ,"","","",kFALSE},
  {"auau62Prod4.Chain" ,"" ,"","P2004,DbV20040415,OShortR,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,"
   "Kink2,CMuDst,ZDCvtx,hitfilt"	                                                  ,"","","",kFALSE},
  {"dauprod3.Chain" ,"" ,"","DbV20040520,dau2003a,-tcl,fcf,est,beamLine,xi2,Kink2,XiSvt,svtdEdx,eemcD,"
   "hitfilt,CMuDst"              	                                                  ,"","","",kFALSE},
  {"auau200hij.Chain" ,"" ,"","trs,Simu,srs,fss,y2004a,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin,sdt2001112"    ,"","","",kFALSE},
  {"auau62hij.Chain" ,"" ,"","trs,Simu,srs,fss,y2004a,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin"               ,"","","",kFALSE},
  {"pp200pyth.Chain" ,"" ,"","trs,Simu,srs,fss,y2004a,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin"               ,"","","",kFALSE},
  {"j/psi.prod1.Chain","","","trs,Simu,srs,fss,y2005x,tpc,l0,ctf,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,"
   "evout,EST,Xi2,svtdEdx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin"                    ,"","","",kFALSE},
  {"j/psi.prod2.Chain" ,"" ,"","trs,Simu,fss,y2005x,tpc,l0,ctf,ftpc,Cdst,Kalman,tags,Tree,bbcsim,tofsim,"
   "evout,Xi2,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin"                                ,"","","",kFALSE},
  {"cu22prod.Chain" ,"" ,"","P2005,DbV20060112,useCDV,tofDat,EST,svtdEdx,xiSvt,Xi2,V02,Kink2,pmdRaw,"
   "CMuDst,OShortR,OSpaceZ2,OGridLeak3D,hitfilt"                                          ,"","","",kFALSE},
  {"auau200.jpsi.hij.Chain" ,"" ,"","trs,Simu,fss,y2004y,tpc,l0,ctf,ftpc,Cdst,Kalman,tags,Tree,bbcsim,tofsim,"
   "evout,Xi2,Kink2,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin"                          ,"","","",kFALSE},
  {"auau200y2004.tpt.Chain" ,"" ,"","P2004,DbV20041213,SCEbyE,svt_daq,svtD,EST,svtdEdx,eemcD,Xi2,xiSvt,"
   "Kink2,pmdRaw,CMuDst,hitfilt"                                                          ,"","","",kFALSE},
  {"pprun2004zb.Chain" ,"" ,"","pp2004,DbV20041106,svt_daq,svtD,EST,svtdEdx,eemcD,Xi2,xiSvt,Kink2,"
   "beamLine,pmdRaw,CMuDst"      	                                                  ,"","","",kFALSE},
  {"pprun2004.Chain" ,"" ,"","pp2004,DbV20041213,EST,svtdEdx,Xi2,xiSvt,Kink2,beamLine,pmdRaw,CMuDst,hitfilt"
   ,"","",""                                                                                       ,kFALSE},
  {"auau200prod.tpt.Chain" ,"" ,"","P2004,DbV20041213,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt"
   ,"","",""                                                                                       ,kFALSE},
  {"auau200prod2.tpt.Chain" ,"" ,"",
   "P2004,DbV20050312,SCEbyE,OGridLeak,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt" ,"","","",kFALSE},
  {"auau200mb.tpt.Chain" ,"" ,"",
   "P2004,DbV20050215,SCEbyE,OGridLeak,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt" ,"","","",kFALSE},
  {"cucu200hij.Chain" ,"" ,"","trs,Simu,srs,fss,y2005x,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin"               ,"","","",kFALSE},
  {"cucu200hij2.Chain" ,"" ,"","trs,Simu,srs,fss,y2005x,tpc,l0,ctf,ftpc,svt,pmd,Cdst,Kalman,tags,Tree,bbcsim,"
   "tofsim,evout,est,xi2,XiSvt,svtdEdx,emcY2,eefs,GeantOut,big,CMuDst,fzin"               ,"","","",kFALSE},
  {"auau200.prod3.tpt.Chain" ,"" ,"",
   "P2004,DbV20050312,SCEbyE,OGridLeak,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt" ,"","","",kFALSE},
  {"cucu.prod1.Chain" ,"" ,"",   "P2005,DbV20050421,SCEbyE,OGridLeak,ToF,svt_daq,svtD,EST,pmdRaw,"
   "Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2"                                        ,"","","",kFALSE},
  {"cucu.prod2.Chain" ,"" ,"","P2005,DbV20050515,useCDV,SCEbyE,OGridLeak,tofDat,EST,svtdEdx,xiSvt,pmdRaw,"
   "Xi2,V02,Kink2,-dstout,CMuDst,OShortR,OSpaceZ2,hitfilt"                                ,"","","",kFALSE},
  {"pp.special.Chain" ,"" ,"","trs,Simu,fss,y2004c,tpc,l0,ctf,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,"
   "evout,Xi2,V02,Kink2,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin"                      ,"","","",kFALSE},
  {"pp.special2.Chain" ,"" ,"","trs,Simu,fss,y2004y,tpc,l0,ctf,ftpc,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,"
   "evout,Xi2,Kink2,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin,sdt20040427"              ,"","","",kFALSE},
#else /* ITTF chains */
  {"ppITTFsvt.Chain" ,"" ,"","DbV20020802,pp2001,fpd,beamLine,svt_daq,SvtD,est,svtdedx,CMuDst"
   ,"","",""                                                                                       ,kFALSE},
  {"ittfMiniMc.Chain" ,"" ,"","RY2005,in,event,svtDb,ITTF,Sti,SvtIT,evOut,InTree,ReadAll,Simu,MiniMc"
   ,"","",""                                                                                       ,kFALSE},
  {"dau.ittf.prod.Chain" ,"" ,"","DbV20040520,dau2003i,ITTF,SvtIT,hitfilt,in"	          ,"","","",kFALSE},
  {"ppITTFpyth.Chain" ,"" ,"","trs,Simu,srs,fss,y2004,Idst,tpcI,tcl,ftpc,l0,ctf,Tree,SvtCL,svtDb,ITTF,Sti,"
   "genvtx,dEdxY2,geant,tags,bbcSim,tofsim,emcY2,"                                        ,"","","",kFALSE},
  {"auau200run2004.ITTF.Chain" ,"" ,"","P2004,DbV20040917,SCEbyE,ITTF,pmdRaw,hitfilt"	  ,"","","",kFALSE},
  {"pp200run2004.ITTF.Chain" ,"" ,"","pp2004,DbV20040917,beamLine,ITTF,hitfilt"	          ,"","","",kFALSE},
  {"auau200y2004.ITTF.Chain" ,"" ,"","P2004,DbV20040804,SCEbyE,ITTF,pmdRaw,hitfilt"       ,"","","",kFALSE},
  {"pp.2005prod.ITTF.Chain","","","DbV20050816,pp2005a,ITTF,OSpaceZ2,OGridLeak3D,hitfilt" ,"","","",kFALSE},
  {"pp.pythia2.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2004y,Idst,l0,ctf,tpcI,fcf,ftpc,Tree,logger,ITTF,Sti,"
   "VFPPV,bbcSim,tofsim,tags,emcY2,EEfs,evout,geantout,big,fzin,MiniMcMk,eemcDb,beamLine,"
   "sdt20050727"                	                                                  ,"","","",kFALSE},
  {"cucu.ITTF.noSvt.Chain" ,"" ,"",   "P2005,DbV20060421,useCDV,ITTF,tofDat,-svtIT,"
   "SCEbyE,OGridLeak,OShortR,OSpaceZ2,hitfilt"                                            ,"","","",kFALSE},
  {"cucu62.D0.nosvt.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,l0,ctf,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,VFMCE,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,fzin,"
   "MiniMcMk"          	                                                  ,"","","",kFALSE},
  {"cucu.ITTF.Chain" ,"" ,"","P2005,DbV20060524,useCDV,MakeEvent,ITTF,tofDat,ssddat,spt,SsdIt,"
   "SCEbyE,OGridLeak3D,OShortR,OSpaceZ2,KeepSvtHit,hitfilt"                               ,"","","",kFALSE},
  {"cucu62.D0.prod.ITTF.Chain" ,"" ,"","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,l0,ctf,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"      	                                                  ,"","","",kFALSE},
  {"cucu200.D0.prod.ITTF.Chain" ,"" ,"","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,l0,ctf,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                  ,"","","",kFALSE},
  {"cucu200.D0.nosvt.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"      	                                                  ,"","","",kFALSE},
  {"cucu.ITTF.test2.Chain","" ,"","P2005,DbV20060620,useCDV,MakeEvent,ITTF,tofDat,ssddat,spt,SsdIt,"
   "SCEbyE,OGridLeak3D,OShortR,OSpaceZ2,KeepSvtHit,hitfilt"                               ,"","","",kFALSE},
  {"cucu200.D0.prod2.ITTF.Chain","","","trs,Simu,srs,ssd,fss,y2006,Idst,IAna,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,SvtIt,SsdIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                  ,"","","",kFALSE},
  {"cucu200.D0.nosvt.prod2.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk"     	                                                  ,"","","",kFALSE},
  {"ppJpsi.prod2006.ITTF.Chain","","","DbV20060729,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt",
   "","",""                                                                                        ,kFALSE},
  {"pp200.Upmix.prod.ITTF.Chain" ,"" ,"","trs,Simu,fss,y2006,Idst,IAna,tpcI,fcf,ftpc,Tree,logger,"
   "ITTF,Sti,genvtx,-SvtIt,geant,evout,IdTruth,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big,-dstout,"
   "fzin,MiniMcMk,beamLine"                                                      ,"","","",kFALSE},
  {"pp2006prod.ITTF.Chain" ,"" ,"","DbV20060915,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt","","","",kFALSE},
  {"pp2006prod2.ITTF.Chain","" ,"","DbV20061021,pp2006b,ITTF,OSpaceZ2,OGridLeak3D,hitfilt","","","",kFALSE},
  {"auauUpgr05.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr05,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,"
   "IdTruth,tags,bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"            ,"","","",kFALSE},
  {"auauUpgr01.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr01,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr07.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr07,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"pp2006prod3.ITTF.Chain" ,"" ,"","DbV20060915,pp2006b,ITTF,hitfilt"	                  ,"","","",kFALSE},
  {"auauUpgr08.prod.ITTF.Chain" ,"" ,"","trs,Simu,upgr08,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr06.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr06,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,HpdIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr09.prod.ITTF.Chain" ,"" ,"","trs,Simu,upgr09,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,HpdIT,IstIT,StiPulls,genvtx,NoSvtIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr10.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr10,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr11.prod.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr11,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,IstIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut"                         ,"","","",kFALSE},
  {"auauUpgr06.prod2.ITTF.Chain" ,"" ,"","trs,Simu,ssd,upgr06,pixFastSim,Idst,IAna,tpcI,fcf,Tree,logger,"
   "ITTF,Sti,StiRnd,PixelIT,HpdIT,StiPulls,genvtx,NoSvtIt,SsdIt,MakeEvent,McEvent,geant,evout,IdTruth,tags,"
   "bbcSim,emcY2,EEfs,big,-dstout,fzin,MiniMcMk,McEvOut" ,"" ,"",""                       ,kFALSE},
#endif
  {"nightly test (dev) chains","-----------","-----------","-----------------------------","","","",kFALSE},
#ifndef __BFC2__
  {"Test.year_1h_central_daq", "","", "p2000","","",
   "/star/rcf/test/daq/2000/09/st_physics_1248022_raw_0001.daq"                                    ,kFALSE},
  {"Test.year_1h_hc_standard_trs","","","trs,Simu,mdc3,v0,xi,big,logger,evout,-dstout,fzin","","",
   "/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/"
   "hc_standard.40_evts.fz"                                                                        ,kFALSE},
  {"Test.year_1h_minbias_daq", "","", "p2000","","",
   "/star/rcf/test/daq/2000/08/st_physics_1229021_raw_0003.daq"                                    ,kFALSE},
  {"Test.year_2001_central_daq", "","P2001a,v0,xi,ZDCvtx,-dstout,logger,CMuDst","","",
   "/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq"                                   ,kFALSE},
  {"Test.year_2001_hc_highdensity_trs", "", "", 
   "trs,Simu,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,logger,CMuDst,big,fzin","","",
   "/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/hadronic_on/Gstardata/"
   "hc_highdensity.16_evts.fz"                                                                     ,kFALSE},
  {"Test.year_2001_hc_lowdensity_trs","","", "trs,Simu,srs,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,"
   "logger,CMuDst,big,fzin","","",
   "/star/rcf/simu/cocktail/hadronic/default/lowdensity/year2001/hadronic_on/Gstardata/"
   "hc_lowdensity.400_evts.fz"                                                                     ,kFALSE},
  {"Test.year_2001_minbias_daq", "", "", "P2001a,v0,xi,ZDCvtx,-dstout,logger,CMuDst","","",
   "/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq"                                   ,kFALSE},
  {"Test.year_2001_ppMinBias_daq", "","","pp2001a,v0,xi,fpd,beamLine,est,-dstout,logger,CMuDst","","",
   "/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq"                                   ,kFALSE},
  {"Test.year_2001_pp_minbias_trs", "","",
   "trs,Simu,rrs,fss,y2001n,C2default,v0,xi,GeantOut,-dstout,logger,CMuDst,big,fzin","","",
   "/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/gstardata/pds0200_04_12812evts.fzd"
   ,                                                                                                kFALSE},
  {"Test.year_2003_dAuMinBias_daq","","","DbV20040520,dau2003,v0,xi,l3onl,est,beamLine,-dstout,logger,CMuDst"
   ,"",""                         ,"/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",kFALSE},
  {"Test.year_2003_dau_minbias_trs", "","","dAuMDCa,v0,xi,tofsim,Eefs,beamLine,-dstout,logger,CMuDst,fzin",
   "","","/star/rcf/simu/rcf1197_05_5940evts.fzd"                                                  ,kFALSE},
  {"Test.year_2003_ppMinBias_daq", "","","pp2003,v0,xi,l3onl,beamLine,est,eemcD,-dstout,logger,CMuDst",
   "","","/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq"                          ,kFALSE},
  {"Test.year_2004_auau_central_trs", "","","trs,Simu,srs,fss,y2004a,tpc,l0,ctf,svt,ftpc,Cdst,Kalman,tags,Tree,"
   "est,bbcsim,tofsim,evout,EventQA,xi2,Kink2,emcY2,eefs,GeantOut,big,-dstout,logger,CMuDst,fzin",
   "","","/star/rcf/simu/rcf1209_05_80evts.fzd"                                                    ,kFALSE},
  {"Test.year_2004_AuAuMinBias_daq","","","P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,"
   "logger,CMuDst","","",          "/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",kFALSE},
  {"Test.year_2004_auau_minbias_trs", "","",   "trs,Simu,srs,fss,y2004,tpc,l0,ctf,svt,ftpc,Cdst,Kalman,tags,"
   "Tree,est,bbcsim,tofsim,evout,EventQA,xi2,Kink2,emcY2,eefs,GeantOut,big,-dstout,logger,CMuDst,fzin",
   "","","/star/rcf/simu/rcf1207_01_225evts.fzd"                                                   ,kFALSE},
  {"Test.year_2004_AuAu_prodHigh_daq", "","",   "P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,"
   "pmdRaw,logger,CMuDst","","",   "/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",kFALSE},
  {"Test.year_2004_AuAu_prodLow_daq", "","","P2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,"
   "logger,CMuDst","","",          "/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",kFALSE},
  {"Test.year_2004_prodPP_daq", "","",   "pp2004,DbV20050312,EST,-dstout,svtdEdx,Xi2,xiSvt,Kink2,beamLine,"
   "pmdRaw,logger,CMuDst","","",   "/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",kFALSE},
  {"Test.year_2005_CuCu200_HighTower_daq", "","",
   "P2005,tofDat,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,logger,CMuDst,OShortR,OSpaceZ2","","",
   "/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq"                                ,kFALSE},
  {"Test.year_2005_CuCu200_MinBias_daq", "","",
   "P2005,tofDat,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,logger,CMuDst,OShortR,OSpaceZ2","","",
   "/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq"                                ,kFALSE},
  {"Test.year_2005_cucu200_minbias_trs", "","","trs,Simu,srs,fss,y2005x,tpc,l0,ctf,ftpc,svt,Cdst,Kalman,tags,"
   "Tree,bbcsim,tofsim,evout,est,xi2,Kink2,emcY2,eefs,GeantOut,big,logger,CMuDst,fzin",
   "","","/star/rcf/simu/rcf1216_05_200evts.fzd"                                                   ,kFALSE},
  {"Test.year_2005_CuCu22_MinBias_daq", "","",
   "P2005,tofDat,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,logger,CMuDst,OShortR,OSpaceZ2","","",
   "/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq"                                ,kFALSE},
  {"Test.year_2005_CuCu62_MinBias_daq", "","",
   "P2005,tofDat,svt_daq,svtD,EST,pmdRaw,Xi2,V02,Kink2,-dstout,logger,CMuDst,OShortR,OSpaceZ2","","",
   "/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq"                                ,kFALSE},
  {"Test.year_2005_cucu62_minbias_trs", "","","trs,Simu,srs,fss,y2005x,tpc,l0,ctf,ftpc,svt,Cdst,Kalman,tags,"
   "Tree,bbcsim,tofsim,evout,est,xi2,Kink2,emcY2,eefs,GeantOut,big,logger,CMuDst,fzin",
   "","","/star/rcf/simu/rcf1237_01_500evts.fzd"                                                   ,kFALSE},
#else
  {"Test.ITTF","","","svtIT,ssdIT,ITTF,genvtx,event,analysis,EventQA,tags,Tree,EvOut,StarMagField,FieldOn"
   ",IDST,CMuDst,Tree,analysis"                                                           ,"","","",kFALSE},
  {"Test.reco.ITTF","","","MakeEvent,tpcI,fcf,ftpc,SvtCL,Test.ITTF"                       ,"","","",kFALSE},
  {"Test.fast.ITTF","","","gstar,tfs,Simu,srs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,SvtCL,"
   "tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,ITTF,genvtx,Idst,event,analysis,EventQA,tags,Tree,EvOut,"
   "StarMagField,FieldOn,IAna,y2007,CMuDst"                                               ,"","","",kFALSE},
#if 0
  {"Test.default.ITTF","","","gstar,trs,Simu,sss,svt,ssd,fss,bbcSim,emcY2,McEvOut,GeantOut,IdTruth,MakeEvent,"
   "miniMcMk,McAna,Test.ITTF"                                                             ,"","","",kFALSE},
#else
  {"Test.default.ITTF","","","gstar,trs,Simu,sss,svt,ssd,fss,bbcSim,emcY2,GeantOut,IdTruth,MakeEvent,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
#endif
  {"Test.default.Fast.ITTF","","","gstar,tfs,sfs,ssdFast,IdTruth,MakeEvent,miniMcMk,McAna,Test.ITTF",
   ""                                                                                        ,"","",kFALSE},
  {"Test.srs.ITTF","","",   "gstar,trs,Simu,srs,svt,ssd,fss,bbcSim,emcY2,McEvOut,GeantOut,IdTruth,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"Test.year_2003_dAuMinBias_daq.ittf", "","","DbV20040520,dau2003i,logger,ITTF,evout,in","","",
   "/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq"                                ,kFALSE},
  {"Test.year_2004_auau_central_trs.ittf","","","trs,Simu,srs,fss,y2004a,Idst,IAna,l0,ctf,tpcI,fcf,ftpc,Tree,"
   "logger,ITTF,evout,Sti,genvtx,SvtIT,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big,fzin,"
   "MiniMcMk",                                "","","/star/rcf/simu/rcf1209_05_80evts.fzd",kFALSE},
  {"Test.year_2004_AuAuMinBias_daq.ittf", "","","P2004,DbV20050312,logger,ITTF,evout,-dstout,pmdRaw",
   "","","/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq"                          ,kFALSE},
  {"Test.year_2004_auau_minbias_trs.ittf", "", "",   "trs,Simu,srs,fss,y2004,Idst,IAna,l0,ctf,tpcI,fcf,ftpc,"
   "Tree,logger,ITTF,evout,Sti,genvtx,SvtIt,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big,fzin,"
   "MiniMcMk",                               "","","/star/rcf/simu/rcf1207_01_225evts.fzd",kFALSE},
  {"Test.year_2004_AuAu_prodHigh_daq.ittf","","","P2004,DbV20050312,logger,ITTF,evout,-dstout,"
   "pmdRaw","","",                 "/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",kFALSE},
  {"Test.year_2004_AuAu_prodLow_daq.ittf", "","",   "P2004,DbV20050312,logger,ITTF,evout,-dstout,"
   "pmdRaw","","",                 "/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",kFALSE},
  {"Test.year_2004_prodPP_daq.ittf", "","","pp2004,DbV20050312,beamLine,logger,ITTF,evout,-dstout",
   "",""                          ,"/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",kFALSE},
  {"Test.year_2005_CuCu200_HighTower_daq.ittf", "", "",
   "P2005,tofDat,logger,useCDV,MakeEvent,ITTF,evout,ssddat,spt,SsdIt,pmdRaw,OShortR,OSpaceZ2",
   "","","/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq"                          ,kFALSE},
  {"Test.year_2005_CuCu200_MinBias_daq.ittf", "","",
   "P2005,tofDat,logger,useCDV,MakeEvent,ITTF,evout,ssddat,spt,SsdIt,pmdRaw,OShortR,OSpaceZ2",
   "","","/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq"                          ,kFALSE},
  {"Test.year_2005_cucu200_minbias_trs.ittf", "","",   "trs,Simu,srs,fss,ssd,y2005x,Idst,IAna,l0,ctf,tpcI,"
   "fcf,ftpc,Tree,logger,ITTF,evout,Sti,SsdIt,genvtx,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,"
   "big,fzin,MiniMcMk,SvtIt",                "","","/star/rcf/simu/rcf1216_05_200evts.fzd",kFALSE},
  {"Test.year_2005_CuCu22_MinBias_daq.ittf", "","",
   "P2005,tofDat,logger,useCDV,MakeEvent,ITTF,evout,ssddat,spt,SsdIt,pmdRaw,OShortR,OSpaceZ2",
   "","","/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq"                          ,kFALSE},
  {"Test.year_2005_CuCu62_MinBias_daq.ittf", "","",
   "P2005,tofDat,logger,useCDV,MakeEvent,ITTF,evout,ssddat,spt,SsdIt,pmdRaw,OShortR,OSpaceZ2",
   "","","/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq"                          ,kFALSE},
  {"Test.year_2005_cucu62_minbias_trs.ittf", "", "","trs,Simu,srs,fss,ssd,y2005x,Idst,IAna,l0,ctf,tpcI,fcf,"
   "ftpc,Tree,logger,ITTF,evout,Sti,SsdIt,genvtx,SvtIT,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,"
   "big,fzin,MiniMcMk",                      "","","/star/rcf/simu/rcf1237_01_500evts.fzd",kFALSE},
  {"Test.year_2005_ppProduction_daq.ittf", "","","pp2005a,tofdat,ITTF,evout,OSpaceZ2,OGridLeak3D",
   "","","/star/rcf/test/daq/2005/171/st_physics_6171062_raw_2040010.daq"                          ,kFALSE},
  {"Test.year_2006_ppProdLong_daq.ittf", "","","pp2006b,ITTF,evout,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2006/155/7155010/st_physics_7155010_raw_1020003.daq"                        ,kFALSE},
  {"Test.year_2006_ppProdTrans_daq.ittf", "","","pp2006b,ITTF,evout,OSpaceZ2,OGridLeak3D","","",
   "/star/rcf/test/daq/2006/129/7129023/st_physics_7129023_raw_1020003.daq"                        ,kFALSE},
  {"Test.year_2007_auau200_central.ittf", "","","trs,srs,ssd,fss,y2007,Idst,IAna,l0,tpcI,fcf,ftpc,Tree,"
   "logger,ITTF,Sti,SvtIt,SsdIt,genvtx,MakeEvent,IdTruth,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,"
   "GeantOut,big,fzin,MiniMcMk,-dstout,clearmem", "","","/star/rcf/simu/rcf1296_02_100evts.fzd"    ,kFALSE},
  {"Test.year_2008_production_dAu2008.ITTF", "","","P2008b,ittf", "","",
   "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq"                                ,kFALSE},
  {"Test.year_2008_ppProduction2008.ITTF",  "","","pp2008a,ittf", "","",
   "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq"                                ,kFALSE},
#endif
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Aliased     ","time stamps","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE}
};
