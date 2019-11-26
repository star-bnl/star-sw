#ifndef __BigFullChain_h__
#define __BigFullChain_h__
#if 0
/*   -- from Jeff list 04/26/12 -- zdc always in
                                   fpd   (>=2003) is in TriggerMaker
				   l3raw (=>2004) is empty
year : active detector list                                                                | Data samples
______________________________________________________________________________________________________________
y2000: tpc ctb                                                                             | AuAu130, HF
y2001: tpc ctb rich     svt     	 ftpc						   | AuAu200,19; pp200
y2003: tpc ctb      bbc svt tof btow fpd ftpc pmd ssd -etow   			           | dAu200; pp200 1/3 of etow was installed, but db  is failed, fpdm00
y2004: tpc ctb      bbc	svt tof btow fpd ftpc pmd ssd etow       bsmd esmd	           | AuAu62,200; pp200	         fpdm01
y2005: tpc ctb      bbc	svt tof btow fpd ftpc pmd ssd etow       bsmd esmd	           | CuCu200,62,22; pp200        fpdm01
y2006: tpc ctb      bbc	svt tof btow fpd ftpc pmd ssd etow       bsmd esmd	       vpd | pp200,62		         fpdm01,fpdm02
y2007: tpc ctb      bbc	svt     btow fms ftpc pmd ssd etow       bsmd esmd             vpd | AuAu200,9		         fpdm03, fpd is part of fms
y2008: tpc ctb      bbc	    tof btow fms ftpc pmd     etow pp2pp bsmd esmd tpx	       vpd | pp200;AuAu9;dAu200	         -"-
y2009:     ctb      bbc	    tof btow fms ftpc         etow pp2pp bsmd esmd tpx	       vpd | pp500,200;pp2pp
y2010:              bbc	    tof btow fms ftpc pmd     etow       bsmd esmd tpx	       vpd | AuAu200,62,39,7.7,11.5
y2011:              bbc	    tof btow fms ftpc pmd     etow       bsmd esmd tpx mtd     vpd | AuAu19.6,27,200;pp500
y2012:              bbc	    tof btow fms              etow       bsmd esmd tpx mtd fgt vpd | pp500,200,UU193
y2013:              bbc     tof btow fms              etow       nsmd esmd tpx mtd fgt vpd gmt  | - fpd, pp500
y2014:              bbc     tof btow                  etow       nsmd esmd tpx mtd     vpd gmt  | no fgt and fms
y2015:              bbc     tof btow fms              etow       nsmd esmd tpx mtd     vpd gmt fps | +fms and fps                                        
______________________________________________________________________________________________________________
 */
#endif

#define STAR_CHAIN_OBSOLETE "WARNING *** Option is OBSOLETE ***"

//#define __NoStrangeMuDst__
#define __NoDisplay__
#define __AgMLonFly__
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

  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Valid Db    ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"NoDb"  ,""  ,"","-db,-tpcDb,-magF"                              ,"","","Take out Db from Chain",kFALSE},
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
  {"Geometry    ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#ifdef __AgMLonFly__
  {"ideal",       "",  "","",            "",           "",              "Ideal Alignment", kFALSE},
  {"misalign",    "",  "","",            "","-AgMLideal",           "Misaligned Geometry", kFALSE},
  {"AgMLutil",    "",  "","",            "","StarAgmlUtil",             "AgML support", kFALSE},
  {"AgMLlib",     "",  "","",            "","StarAgmlUtil,StarAgmlLib", "AgML support", kFALSE},
  {"AgML"        ,""  ,"","AgMLlib,-Agi,-VmcGeo","","Geometry,StarGeometry"
   ,                                                            "Alias VmcGeometry to AgiLGeometry",kFALSE},
#else /* __AgMLonFly__ */
  {"AgML"        ,""  ,"","-Agi,-VmcGeo","",""                      //StarAgmlLib,Geometry,StarGeometry
   ,                                                            "Alias VmcGeometry to AgiLGeometry",kFALSE},
#endif /* __AgMLonFly__ */
  {"Agi" ,"" ,"","-VmcGeo","",""      ,"Alias VmcGeometry to AgiGeometry (gstar original geometry)",kFALSE},
  {"VmcGeo"      ,""  ,"","-AgML,-Agi"                    ,"",""     ,"Alias VmcGeometry to VmcGeo",kFALSE},

  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Trigger Type","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Physics"     ,"","","",                                               "","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"LaserTest"   ,"","","",                                               "","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"PulserSvt"   ,"","","",                                               "","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"alltrigger"  ,"","","",                                               "","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"phys_off"    ,"","",""                                  ,"","","Turn off physics in simulation",kFALSE},
  {"hadr_off"    ,"","",""                    ,"","","Turn off hadronic interactions in simulation",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"C H A I N S ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Calibration chains","------------","-----------","-----------------------------------","","","",kFALSE},
  {"LanaDV",   "","","LanaDVtpx","",""                                                   ,"get LDV",kFALSE},
  {"LanaDVtpx","","","MakeEvent,trgd,in,tpx,TpcHitMover,LaserIT,VFMinuit,Lana,Analysis,Corr4,NosvtIT,NossdIT",
   "",""                                                                        ,"get LDV with TPX",kFALSE},
  {"LaserDV.Chain","","","in,LaserCal,fcf,TpcHitMover,OGridLeak3D,OShortR,OSpaceZ2","","","get LDV",kFALSE},
  {"Test.default.ITTF","","","TpcRS,Simu,sss,svt,ssd,fss,bbcSim,IdTruth,MakeEvent,genvtx,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"Test.default.y2005g.ITTF","","","Test.default.ITTF,sdt20050130,noSimuDb"              ,"","","",kFALSE},
  {"Test.default.y2007g.ITTF","","","Test.default.ITTF,sdt20070322,noSimuDb"              ,"","","",kFALSE},
  {"Test.fast.y2005g.ITTF","","","Test.fast.ITTF,sdt20050130,noSimuDb"                    ,"","","",kFALSE},
  {"Test.reco.StiVMC","","","MakeEvent,tpcI,tcl,ftpc,SvtCL,Test.StiVMC"                   ,"","","",kFALSE},
  {"Test.default.StiVMC","","","TpcRS,Simu,sss,svt,ssd,fss,bbcSim,IdTruth,MakeEvent,"
   "miniMcMk,McAna,Test.reco.StiVMC,CMuDst"                                               ,"","","",kFALSE},
  {"Test.StiVMC","","","TpcRS,StiVMC,event,analysis,tags,EvOut,StarMagField,FieldOn,Idst,CMuDst"
   ,                                                                                      "","","",kFALSE},
  {"Test.VeryFast.StiVMC","","","TpcFastSim,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,"
   "SvtCL,tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,StiVMC,Idst,event,analysis,EventQA,tags,"
   "EvOut,StarMagField,FieldOn,IAna,CMuDst"                                               ,"","","",kFALSE},
  {"Test.fast.StiVMC","","","tfs,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,SvtCL,"
   "tpc_T,globT,tls,db,tpcDB,svtDb,StiVMC,Idst,event,analysis,EventQA,tags,EvOut,"
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
  {"Test.default.Fast.ITTF","","","tfs,sfs,ssdFast,IdTruth,MakeEvent,miniMcMk,Test.ITTF",  "","","",kFALSE},
  {"Test.srs.ITTF","","",   "TpcRS,Simu,srs,svt,ssd,fss,bbcSim,emcY2,McEvOut,GeantOut,IdTruth,"
   "miniMcMk,McAna,Test.reco.ITTF,CMuDst"                                                 ,"","","",kFALSE},
  {"Test.ITTF","","","svtIT,ssdIT,ITTF,event,analysis,EventQA,tags,EvOut,StarMagField,FieldOn"
   ",IDST,CMuDst,analysis"                                                                ,"","","",kFALSE},
  {"Test.reco.ITTF","","","MakeEvent,tpcI,tcl,ftpc,SvtCL,Test.ITTF"                       ,"","","",kFALSE},
  {"Test.fast.ITTF","","","tfs,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,SvtCL,"
   "tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,ITTF,Idst,event,analysis,EventQA,tags,EvOut,"
   "StarMagField,FieldOn,IAna,CMuDst"                                                     ,"","","",kFALSE},
  {"Test.VeryFast.ITTF","","","TpcFastSim,Simu,sfs,ssdfast,McEvOut,GeantOut,IdTruth,miniMcMk,McAna,"
   "SvtCL,tpc_T,globT,tls,db,tpcDB,svtDb,svtIT,ssdIT,ITTF,Idst,event,analysis,EventQA,tags,"
   "EvOut,StarMagField,FieldOn,IAna,CMuDst"                                               ,"","","",kFALSE},
  {"nightly test (dev) chains","-----------","-----------","----------------------------","","","", kFALSE},
  {"MC----------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MC.y2000"        ,"","","trs,rrs,y1h,Idst,rich,IAna,l0,tpcI,tpxClu,NosvtIT,NossdIT,"
   "-Kink2,VFMinuit,geant,evout,IdTruth,tags,GeantOut,big"                                ,"","","",kFALSE},
  {"MC.y2001"        ,"","","trs,fss,rrs,y2001n,Idst,rich,IAna,l0,tpcI,tpxClu,ftpc,-Kink2,"
   "VFMinuit,geant,evout,IdTruth,tags,emcY2,GeantOut,big"                                 ,"","","",kFALSE},
  {"MC.pp.y2001"     ,"","","trs,rrs,fss,y2001n,Idst,rich,IAna,l0,tpcI,tpxClu,ftpc,-Kink2,"
   "VFMinuit,geant,evout,IdTruth,tags,emcY2,MiniMcMk,GeantOut,big"                        ,"","","",kFALSE},
  {"MC.y2003"        ,"","","trs,fss,y2003,Idst,IAna,l0,tpcI,tpxClu,ftpc,VFMinuit,bbcSim,tofsim,"
   "tags,emcY2,evout,IdTruth,geantout"                                                    ,"","","",kFALSE}, // EEfs,
  {"MC.y2004"        ,"","","trs,srs,fss,y2004,Idst,BAna,l0,tpcI,tpxClu,ftpc,VFMinuit,SvtIt,geant,evout,"
   "tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big"                                           ,"","","",kFALSE},
  {"MC.y2004a"       ,"","","trs,srs,fss,y2004a,Idst,BAna,l0,tpcI,tpxClu,ftpc,VFMinuit,SvtIT,geant"
   ",tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big"                                    ,"","","",kFALSE},
  {"MC.y2005"        ,"","","trs,srs,fss,ssd,y2005x,Idst,IAna,l0,tpcI,tpxClu,ftpc,SvtCL,svtDb,"
   "SsdIt,SvtIt,VFMinuit,geant,evout,tags,bbcSim,tofsim,emcY2,EEfs,GeantOut,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2006"        ,"","","trs,fss,y2006h,Idst,IAna,l0,tpcI,tpxClu,ftpc,VFMinuit,NoSsdIt,NoSvtIt"
   ",MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2007"        ,"","","trs,srs,ssd,fss,y2007,Idst,BAna,tpcI,tpxClu,ftpc,SvtIt,SsdIt,"
   "VFMinuit,MakeEvent,IdTruth,geant,tags,bbcSim,tofsim,emcY2,EEfs,evout,GeantOut,big"
   ""                                                                                     ,"","","",kFALSE},
  {"MC.y2008"        ,"","","trs,fss,y2008a,Idst,IAna,tpcI,tpxClu,ftpc,VFMinuit,NoSsdIt,NoSvtIt"
   ",MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.in.y2008"     ,"","","in,y2008e,FieldOn,ReverseField,Idst,BAna,ftpcT,fpt,NoSsdIt,"
   "NoSvtIt,VFMinuit,tpcDB,MakeEvent,IdTruth,tags,bbcsim,emcY2,EEfs,evout,big,McEvout,MiniMcMk,"
   "ReadAll"                                                                              ,"","","",kFALSE},
  {"MC.in.y2009"     ,"","","in,y2009c,FieldOn,ReverseField,Idst,BAna,ftpcT,fpt,NoSsdIt,"
   "NoSvtIt,VFMinuit,tpcDB,MakeEvent,IdTruth,tags,bbcsim,emcY2,EEfs,evout,big,McEvout,MiniMcMk,"
   "ReadAll"                                                                              ,"","","",kFALSE},
  {"MC.y2009"       ,"","","TpcRS,TpxClu,fss,y2009,Idst,IAna,tpcI,ftpc,VFMinuit,NoSsdIt,NoSvtIt"
   ",MakeEvent,bbcSim,tofsim,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2009a"       ,"","","TpcRS,TpxClu,fss,y2009a,Idst,IAna,tpcI,ftpc,VFMinuit,NoSsdIt,NoSvtIt,"
   "MakeEvent,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2010a"       ,"","","TpcRS,TpxClu,y2010a,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2010"        ,"","","TpcRS,TpxClu,y2010,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big",      "","","",kFALSE},
  {"MC.fast.pp.y2011","","","y2011,Test.default.Fast.ITTF,bbcSim,btofsim,btofMatch,emcY2,emcSim,EEfs,"
   "NosvtIT,NossdIT,-sfs,-ssdFast,VFPPVnoCTB,beamline"                                    ,"","","",kFALSE},
  {"MC.in.y2010"     ,"","","in,y2010c,FieldOn,ReverseField,Idst,BAna,ftpcT,fpt,NoSsdIt,"
   "NoSvtIt,VFMinuit,tpcDB,MakeEvent,IdTruth,tags,bbcsim,emcY2,EEfs,evout,big,McEvout,MiniMcMk,"
   "ReadAll"                                                                              ,"","","",kFALSE},
  {"MC.y2011"        ,"","","TpcRS,TpxClu,y2011,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB"
   ",TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.fast.y2011"   ,"","","y2011,Test.default.Fast.ITTF,NosvtIT,NossdIT,-sfs,-ssdFast,"
   "VFPPVnoCTB,beamline,emcy2"                                                           ,"","","",kFALSE},
  {"MC.in.y2011"     ,"","","in,y2011,FieldOn,ReverseField,Idst,BAna,ftpcT,fpt,NoSsdIt,NoSvtIt,"
   "VFMinuit,tpcDB,MakeEvent,IdTruth,tags,bbcsim,emcY2,EEfs,evout,big,McEvout,MiniMcMk,ReadAll"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2012"        ,"","","TpcRS,TpxClu,y2012,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2012a"       ,"","","TpcRS,TpxClu,y2012a,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2012.eval"      ,"","","in,y2012,FieldOn,ReverseField,Idst,BAna,ftpcT,fpt,NoSsdIt,NoSvtIt"
   ",VFMinuit,tpcDB,MakeEvent,IdTruth,tags,bbcsim,emcY2,EEfs,evout,big,McEvout,MiniMcMk,ReadAll"
   ,                                                                                       "","","",kFALSE},
  {"MC.y2013"        ,"","","TpcRS,TpxClu,y2013_1x,useXgeom,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big",      "","","",kFALSE},
  {"MC.y2014"        ,"","","TpcRS,TpxClu,y2014,useXgeom,MakeEvent,NoSsdIt,NoSvtIt,Idst,BAna,VFMinuit,tpcDB,"
   "TpcHitMover,bbcSim,btofsim,btofMatch,tags,emcY2,EEfs,evout,IdTruth,geantout,big",      "","","",kFALSE},
  {"RC----------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"RC.y2000"        ,"","","p2000,VFMinuit,CMuDst,NosvtIT,NossdIT"                       ,"","","",kFALSE},
  {"RC.y2001"        ,"","","P2001a,VFMinuit,ZDCvtx,CMuDst,NosvtIT,NossdIT"               ,"","","",kFALSE},
  {"RC.pp.y2001"     ,"","","pp2001a,VFMinuit,CMuDst,NossdIT"                             ,"","","",kFALSE},
  {"RC.pp.y2001.ppv" ,"","","pp2001a,VFPPV,beamLine,CMuDst,NossdIT"                       ,"","","",kFALSE},
  {"RC.y2003"        ,"","","DbV20040520,dau2003i,in,-SvtIT,NossdIT"                      ,"","","",kFALSE},
  {"RC.pp.y2003.VFPPV"    ,"","","pp2003,VFPPV,beamLine,CMuDst,-svtIT,NossdIT,Corr2,v0,xi","","","",kFALSE},
  {"RC.y2004"        ,"","","P2004,DbV20041213"                                           ,"","","",kFALSE},
  {"RC.y2004.NoSvt"  ,"","","P2004,DbV20041213,-SsdIt,-SvtIt,pmdRaw"                      ,"","","",kFALSE},
  {"RC.y2004.NoSvt.pmd"    ,"","","P2004,DbV20041213,pmdRaw,-SvtIT,-SsdIT"                ,"","","",kFALSE},
  {"RC.pp.y2004"     ,"","","pp2004,DbV20041213,beamLine"                                 ,"","","",kFALSE},
  {"RC.y2005"        ,"","","P2005,tofDat,MakeEvent,ssddat,sptd,SsdIt,SvtIt,pmdRaw,OShortR,OSpaceZ2"
   ,                                                                                       "","","",kFALSE},
  {"RC.pp.y2005"     ,"","","pp2005a,tofdat,OSpaceZ2,OGridLeak3D"                         ,"","","",kFALSE},
  {"RC.pp.y2006"     ,"","","pp2006b,OSpaceZ2,OGridLeak3D"                                ,"","","",kFALSE},
  {"RC.y2007"        ,"","","DbV20080418,B2007g,IAna,KeepSvtHit,hitfilt,VFMinuit3,emcDY2,ftpc,trgd,"
   "ZDCvtx,svtIT,ssdIT,Corr4,OSpaceZ2,OGridLeak3D"                                        ,"","","",kFALSE},
  {"RC.y2007.NoSvt"  ,"","","DbV20080418,B2007g,IAna,KeepSvtHit,hitfilt,VFMinuit3,emcDY2,ftpc,"
   "trgd,ZDCvtx,Corr4,OSpaceZ2,OGridLeak3D"                                               ,"","","",kFALSE},
  {"RC.y2008"        ,"","","DbV20080712,P2008,OSpaceZ2,OGridLeak3D,beamLine"             ,"","","",kFALSE},
  {"RC.y2008.notof"  ,"","","DbV20080712,P2008,-ToF,-tofDat,-tofrMatch,-tofpMatch,-tofCalib,OSpaceZ2,"
   "OGridLeak3D,beamLine"                                                                 ,"","","",kFALSE},
  {"RC.pp.y2008"     ,"","","DbV20080712,pp2008,OSpaceZ2,OGridLeak3D,beamLine"            ,"","","",kFALSE},
  {"RC.pp.y2008.Minuit","","","DbV20080712,pp2008,-VFPPV,VFMinuit,-ToF,-tofDat,-tofrMatch,-tofpMatch,"
   "-tofCalib,OSpaceZ2,OGridLeak3D,beamLine"                                              ,"","","",kFALSE},
  {"RC.pp.y2009"     ,"","","pp2009c,VFMinuit,beamLine,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D"
   ,                                                                                       "","","",kFALSE},
  {"RC.pp.y2009.notof","","","pp2009c,VFMinuit,beamLine,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"
   ,                                                                                       "","","",kFALSE},
  {"RC.pp.y2009.VFPP","","","pp2009c,VFPPVnoCTB,beamLine,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2010"        ,"","","P2010a,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,pmdReco",  "","","",kFALSE},
  {"RC.y2010.notof"  ,"","","P2010a,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"               ,"","","",kFALSE},
  {"RC.pp.y2011.VFPPV","","","pp2011a,VFPPVnoCTB,beamline,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,"
   "-hitfilt"                                                                             ,"","","",kFALSE},
  {"RC.pp.y2011","","","pp2011a,VFMinuit,beamline,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2011"        ,"","","P2011a,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt,pmdReco,mtdDat"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2011.notof"  ,"","","P2011a,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt,pmdReco,mtdDat"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2012"        ,"","","P2012a,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt,mtdDat,fmsDat"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2012.notof"  ,"","","P2012a,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt",      "","","",kFALSE},
  {"RC.pp.y2012"     ,"","","pp2012a,VFPPVnoCTB,beamline,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,"
   "-hitfilt,mtdDat,fmsDat",                                                               "","","",kFALSE},
  {"RC.pp.y2012.notof","","","pp2012a,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,"
   "-hitfilt",                                                                             "","","",kFALSE},
  {"RC.pp.y2012.notofMin","","","pp2012a,VFMinuit,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"
   ",-hitfilt",                                                                            "","","",kFALSE},
  {"RC.y2012b"        ,"","","P2012b,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt,mtdDat,fmsDat"
   ,                                                                                       "","","",kFALSE},
  {"RC.y2012b.notof"  ,"","","P2012b,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt",     "","","",kFALSE},
  {"RC.pp.y2012b"    ,"","","pp2012b,VFPPVnoCTB,beamline,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,"
   "-hitfilt,mtdDat,fmsDat",                                                               "","","",kFALSE},
  {"RC.pp.y2012b.notof","","","pp2012b,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"
   ",-hitfilt",                                                                            "","","",kFALSE},
  {"RC.pp.y2012b.notofMin","","","pp2012b,VFMinuit,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"
   ",-hitfilt",                                                                            "","","",kFALSE},
  {"RC.pp.y2012b.notofMin","","","pp2012b,VFMinuit,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D"
   ",-hitfilt",                                                                            "","","",kFALSE},
  {"RC.pp.y2013","","","pp2013a,mtd,btof,fmsDat,fgt,fgtPoint,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,"
   "OSpaceZ2,OGridLeak3D,-hitfilt",                                                        "","","",kFALSE},
  {"RC.y2014","","","P2014a,mtd,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt",     "","","",kFALSE},

  {"MC nightlies and Eval","--","-----------","------------------------------------------","","","",kFALSE},
  {"test_MC.stan.y2000","","","MC.y2000,Sti,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/"
   "hc_standard.40_evts.fz",                                                                        kFALSE},
  {"test_MC.pp.y2001","","","MC.pp.y2001,Sti,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/gstardata/pds0200_04_12812evts.fzd"
   ,                                                                                                kFALSE},
  {"test_MC.stan.y2001","","","MC.y2001,Sti,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/cocktail/hadronic/default/standard/year2001/hadronic_on/Gstardata/hc_standard.40_evts.fz"
   ,                                                                                                kFALSE},
  {"test_dau.MC.y2003"         ,"","","MC.y2003,Sti,fzin,MiniMcMk",""
   ,                                                    "","/star/rcf/simu/rcf1197_05_5940evts.fzd",kFALSE},
  {"test_auauCtr.MC.y2004"      ,"","","MC.y2004a,Sti,fzin,MiniMcMk","",""
   ,                                                         "/star/rcf/simu/rcf1209_05_80evts.fzd",kFALSE},
  {"test_auau.MC.y2004"         ,"","","MC.y2004,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1207_01_225evts.fzd",kFALSE},
  {"test_cucu200.MC.y2005"      ,"","","MC.y2005,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1216_05_200evts.fzd",kFALSE},
  {"test_cucu62.MC.y2005"       ,"","","MC.y2005,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1237_01_500evts.fzd",kFALSE},
  {"test_pp200.MC.y2006"       ,"","","MC.y2006,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9991_01_1000evts.fzd",kFALSE},
  {"test_auau200.MC.y2007"      ,"","","MC.y2007,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1296_02_100evts.fzd",kFALSE},
  {"test_dau200.MC.y2008","","","MC.y2008,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9066_20_1000evts.fzd",kFALSE},
  {"test_pp200.MC.y2008","","","MC.y2008,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9992_01_1000evts.fzd",kFALSE},
  {"test_pp200.MC.y2009","","","MC.y2009a,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9993_01_1000evts.fzd",kFALSE},
  {"test_pp500.MC.y2009","","","MC.y2009a,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9994_01_1000evts.fzd",kFALSE},
  {"test_auau11.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10031_1_100evts.fzd",kFALSE},
  {"test_auau200.MC.y2010","","","MC.y2010,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9068_305_100evts.fzd",kFALSE},
  {"test_auau39.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10032_1_100evts.fzd",kFALSE},
  {"test_auau62.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10033_1_100evts.fzd",kFALSE},
  {"test_auau7.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10030_1_100evts.fzd",kFALSE},
  {"test_auau200.MC.y2011","","","MC.y2011,Sti,fzin,MiniMcMk","",""
   ,                                                      "/star/rcf/simu/rcf11023_2060_25evts.fzd",kFALSE},
  {"test_pp500.MC.y2011","","","MC.fast.y2011,Sti,fzin,MiniMcMk","",""
   ,                             "/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd",kFALSE},
  {"test_pp500.pileup.MC.y2011","","","MC.fast.y2011,Sti,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_200evts_Wplus_enu.fzd\n"
   " gfile b /star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd\n"
   " mode TPCE back 4001400\n gback 400 400 0.1 106.6"                                             ,kFALSE},
  {"test_CuAu200.MC.AgML.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12003_1_100evts.fzd",kFALSE},
  {"test_CuAu200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12003_1_100evts.fzd",kFALSE},
  {"test_pp200.MC.AgML.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12000_1_1000evts.fzd",kFALSE},
  {"test_pp200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12000_1_1000evts.fzd",kFALSE},
  {"test_pp500.MC.AgML.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12001_1_1000evts.fzd",kFALSE},
  {"test_pp500.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12001_1_1000evts.fzd",kFALSE},
  {"test_UU200.MC.AgML.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12002_1_100evts.fzd",kFALSE},
  {"test_UU200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12002_1_100evts.fzd",kFALSE},
  {"test.RC.centr.y2000","","","RC.y2000,Sti","",""
   ,                                   "/star/rcf/test/daq/2000/09/st_physics_1248022_raw_0001.daq",kFALSE},
  {"test.RC.minb.y2000","","","RC.y2000,Sti","",""
   ,                                   "/star/rcf/test/daq/2000/08/st_physics_1229021_raw_0003.daq",kFALSE},
  {"test.RC.cent.y2001d","","","RC.y2001,v02,xi2,Sti","",""
   ,                                  "/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq",kFALSE},
  {"test.RC.cent.y2001","","","RC.y2001,v0,xi,Sti","",""
   ,                                  "/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq",kFALSE},
  {"test.RC.minb.y2001d","","","RC.y2001,v02,xi2,Sti","",""
   ,                                  "/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq",kFALSE},
  {"test.RC.minb.y2001","","","RC.y2001,v0,xi,Sti","",""
   ,                                  "/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq",kFALSE},
  {"test.RC.pp.y2001d","","","RC.pp.y2001,v02,xi2,Sti","",""
   ,                                  "/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq",kFALSE},
  {"test.RC.pp.y2001","","","RC.pp.y2001.ppv,v0,xi,Sti","",""
   ,                                  "/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq",kFALSE},
  {"test.RC.pp.y2001_vfppvd","","","RC.pp.y2001.ppv,v02,xi2,Sti","",""
   ,                                  "/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq",kFALSE},
  {"test.RC.dau.y2003","","","RC.y2003,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",kFALSE},
  {"test.RC.pp.y2003","","","RC.pp.y2003.VFPPV,Sti","",""
   ,                               "/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq",kFALSE},
  {"test.RC.auau.lo.y2004","","","RC.y2004,-SvtIT,-SsdIT,pmdRaw,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq",kFALSE},
  {"test.RC.auau.ph.y2004","","","RC.y2004,pmdRaw,-SvtIT,-SsdIT,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq",kFALSE},
  {"test.RC.auau.StiCA.y2004","","","RC.y2004,-SsdIt,-SvtIt,pmdRaw,StiCA","",""
   ,                               "/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",kFALSE},
  {"test.RC.auau.y2004","","","RC.y2004,-SsdIt,-SvtIt,pmdRaw,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",kFALSE},
  {"test.RC.pp.y2004","","","RC.pp.y2004,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2004/134/st_physics_5134013_raw_2010010.daq",kFALSE},
  {"test.RC.cucu200.ht.y2005","","","RC.y2005,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2005/054/st_physics_6054016_raw_1020005.daq",kFALSE},
  {"test.RC.cucu200.y2005","","","RC.y2005,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2005/048/st_physics_6048025_raw_1020002.daq",kFALSE},
  {"test.RC.cucu22.y2005","","","RC.y2005,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2005/083/st_physics_6083006_raw_1040002.daq",kFALSE},
  {"test.RC.cucu62.y2005","","","RC.y2005,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2005/080/st_physics_6080011_raw_1020004.daq",kFALSE},
  {"test.RC.pp200.y2005","","","RC.pp.y2005,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2005/171/st_physics_6171062_raw_2040010.daq",kFALSE},
  {"test.RC.pp200.Long.y2006","","","RC.pp.y2006,ITTF,Sti","",""
   ,                       "/star/rcf/test/daq/2006/155/7155010/st_physics_7155010_raw_1020003.daq",kFALSE},
  {"test.RC.pp200.Trans.y2006","","","RC.pp.y2006,ITTF,Sti","",""
   ,                       "/star/rcf/test/daq/2006/129/7129023/st_physics_7129023_raw_1020003.daq",kFALSE},
  {"test.RC.auau200.MB.y2007","","","RC.y2007,pmdReco,ITTF,Sti","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"test.RC.auau200.y2007","","","RC.y2007,pmdReco,ITTF,Sti","",""
   ,                       "/star/rcf/test/daq/2007/112/8112052/st_physics_8112052_raw_1020010.daq",kFALSE},
  {"test.RC.dau200.y2008","","","RC.y2008,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"test.RC.pp200.y2008","","","RC.pp.y2008,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",kFALSE},
  {"test.RC.pp200.y2009","","","RC.pp.y2009.VFPP,ITTF,Sti","",""
   ,                              "/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq",kFALSE},
  {"test.RC.pp500.y2009","","","RC.pp.y2009.VFPP,ITTF,Sti","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"test.RC.auau11.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",kFALSE},
  {"test.RC.auau200.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"test.RC.auau39.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"test.RC.auau62.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",kFALSE},
  {"test.RC.auau7.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/143/st_physics_11143043_raw_1020001.daq",kFALSE},
  {"test.RC.auau200.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"test.RC.auau20.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"test.RC.auau27.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/174/st_physics_12174106_raw_2040001.daq",kFALSE},
  {"test.RC.pp500.y2011","","","RC.pp.y2011.VFPPV,pmdReco,mtdDat,Sti","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"test.RC.cuAu200.AgML.y2012","","","RC.y2012b,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/143/st_physics_13143018_raw_3020009.daq",kFALSE},
  {"test.RC.cuAu200.y2012","","","RC.y2012b,Sti","",""
   ,                              "/star/rcf/test/daq/2012/143/st_physics_13143018_raw_3020009.daq",kFALSE},
  {"test.RC.pp200.AgML.y2012","","","RC.pp.y2012b,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"test.RC.pp200.y2012","","","RC.pp.y2012b,Sti","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"test.RC.pp500.AgML.y2012","","","RC.pp.y2012b,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"test.RC.pp500.y2012","","","RC.pp.y2012b,Sti","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"test.RC.UU193.AgML.y2012","","","RC.y2012b,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"test.RC.UU193.y2012","","","RC.y2012b,Sti","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"test.RC.pp500.y2013","","","RC.pp.y2013,Sti","",""
   ,                              "/star/rcf/test/daq/2013/079/st_physics_14079008_raw_1920004.daq",kFALSE},
  {"test.RC.AuAu15.y2014","","","RC.y2014,Sti","",""
   ,                              "/star/rcf/test/daq/2014/069/st_physics_15069008_raw_2500008.daq",kFALSE},
  {"test.RC.AuAu200.y2014","","","RC.y2014,Sti","",""
   ,                              "/star/rcf/test/daq/2014/086/st_physics_15086051_raw_2500017.daq",kFALSE},
  {"test.RC.AuAu200.mid.y2014","","","RC.y2014,Sti","",""
   ,                                  "/star/rcf/test/daq/2014/146/st_mtd_15146050_raw_1000030.daq",kFALSE},
  {"test.RC.He3Au200.y2014","","","RC.y2014,Sti","",""
   ,                              "/star/rcf/test/daq/2014/171/st_physics_15171039_raw_2000008.daq",kFALSE},
  //_________ eval _____________
  {"eval_Sti_auau200.MC.y2007"  ,"","","MC.y2007,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1296_02_100evts.fzd",kFALSE},
  {"eval_StiCA_auau200.MC.y2007","","","MC.y2007,StiCA,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf1296_02_100evts.fzd",kFALSE},
  {"eval_Sti.AgML_dau200.MC.y2008","","","MC.y2008,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9066_20_1000evts.fzd",kFALSE},
  {"eval_Sti.AgML_pp200.MC.y2008","","","MC.y2008,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9992_01_1000evts.fzd",kFALSE},
  {"eval_StiCA_dau200.MC.y2008","","","MC.y2008,StiCA,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9066_20_1000evts.fzd",kFALSE},
  {"eval_StiCA_pp200.MC.y2008","","","MC.y2008,StiCA,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9992_01_1000evts.fzd",kFALSE},
  {"eval_Sti_dau200.MC.y2008","","","MC.y2008,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9066_20_1000evts.fzd",kFALSE},
  {"eval_Sti_pp200.MC.y2008","","","MC.y2008,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9992_01_1000evts.fzd",kFALSE},
  {"eval_StvCA_dau200.MC.Stv.y2008","","","MC.in.y2008,StvPulls,StvCA,in,MiniMcMk","",""
   ,                                              "/star/rcf/simu/rcf11025_2040_100evts.event.root",kFALSE},
  {"eval_Stv_dau200.MC.y2008","","","MC.in.y2008,Stv,StvPulls,in,MiniMcMk","",""
   ,                                              "/star/rcf/simu/rcf11025_2040_100evts.event.root",kFALSE},
  {"eval_Sti.AgML_pp500.MC.y2009","","","MC.y2009a,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9994_01_1000evts.fzd",kFALSE},
  {"eval_StiCA_pp200.MC.y2009","","","MC.y2009a,StiCA,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9993_01_1000evts.fzd",kFALSE},
  {"eval_StiCA_pp500.MC.y2009","","","MC.y2009a,StiCA,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9994_01_1000evts.fzd",kFALSE},
  {"eval_Sti_pp200.MC.y2009","","","MC.y2009a,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9993_01_1000evts.fzd",kFALSE},
  {"eval_Sti_pp500.MC.y2009","","","MC.y2009a,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9994_01_1000evts.fzd",kFALSE},
  {"eval_StvCA_pp500.MC.y2009","","","MC.in.y2009,StvPulls,StvCA,in,MiniMcMk","",""
   ,                                        "/star/rcf/simu/rcf11026_1020_50evts_pileup.event.root",kFALSE},
  {"eval_Stv_pp500.MC.y2009","","","MC.in.y2009,Stv,StvPulls,in,MiniMcMk","",""
   ,                                        "/star/rcf/simu/rcf11026_1020_50evts_pileup.event.root",kFALSE},
  {"eval_Sti.AgML_auau11.MC.y2010","","","MC.y2010a,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10031_1_100evts.fzd",kFALSE},
  {"eval_Sti.AgML_auau200.MC.y2010","","","MC.y2010,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9068_305_100evts.fzd",kFALSE},
  {"eval_Sti_auau11.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10031_1_100evts.fzd",kFALSE},
  {"eval_Sti_auau200.MC.y2010","","","MC.y2010,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9068_305_100evts.fzd",kFALSE},
  {"eval_Sti_auau39.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10032_1_100evts.fzd",kFALSE},
  {"eval_Sti_auau62.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10033_1_100evts.fzd",kFALSE},
  {"eval_Sti_auau7.MC.y2010","","","MC.y2010a,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10030_1_100evts.fzd",kFALSE},
  {"eval_StiCA_auau11.MC.y2010","","","MC.y2010a,StiCA,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10031_1_100evts.fzd",kFALSE},
  {"eval_StiCA_auau200.MC.y2010","","","MC.y2010,StiCA,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf9068_305_100evts.fzd",kFALSE},
  {"eval_StiCA_auau39.MC.y2010","","","MC.y2010a,StiCA,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10032_1_100evts.fzd",kFALSE},
  {"eval_StiCA_auau62.MC.y2010","","","MC.y2010a,StiCA,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10033_1_100evts.fzd",kFALSE},
  {"eval_StiCA_auau7.MC.y2010","","","MC.y2010a,StiCA,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf10030_1_100evts.fzd",kFALSE},
  {"eval_Stv_auau200.MC.y2010","","","MC.in.y2010,Stv,StvPulls,in,MiniMcMk","",""
   ,                                               "/star/rcf/simu/rcf11022_2241_50evts.event.root",kFALSE},
  {"eval_StvCA_auau200.MC.y2010","","","MC.in.y2010,StvPulls,StvCA,in,MiniMcMk","",""
   ,                                               "/star/rcf/simu/rcf11022_2241_50evts.event.root",kFALSE},
  {"eval_StvCA_auau39.MC.y2010","","","MC.y2010a,StvCA","",""
   ,                                                        "/star/rcf/simu/rcf10032_1_100evts.fzd",kFALSE},
  {"eval_StvCA_auau62.MC.y2010","","","MC.y2010a,StvCA","",""
   ,                                                        "/star/rcf/simu/rcf10033_1_100evts.fzd",kFALSE},
  {"eval_StvCA_auau7.MC.y2010","","","MC.y2010a,StvCA","",""
   ,                                                        "/star/rcf/simu/rcf10030_1_100evts.fzd",kFALSE},
  {"eval_Sti.AgML_auau200.MC.y2011","","","MC.y2011,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                      "/star/rcf/simu/rcf11023_2060_25evts.fzd",kFALSE},
  {"eval_Sti.AgML_pp500.MC.y2011","","","MC.fast.y2011,AgML,Sti,fzin,MiniMcMk","",""
   ,                             "/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd",kFALSE},
  {"eval_Sti_auau200.MC.y2011","","","MC.y2011,Sti,fzin,MiniMcMk","",""
   ,                                                      "/star/rcf/simu/rcf11023_2060_25evts.fzd",kFALSE},
  {"eval_StiCA_pp500.MC.y2011","","","MC.fast.y2011,StiCA,fzin,MiniMcMk","",""
   ,                             "/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd",kFALSE},
  {"eval_StiCA_pp500.pileup.MC.y2011","","","MC.fast.y2011,StiCA,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_200evts_Wplus_enu.fzd\n "
   "gfile b /star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd\n"
   " mode TPCE back 4001400\n gback 400 400 0.1 106.6"                                             ,kFALSE},
  {"eval_Sti_pp500.MC.y2011","","","MC.fast.y2011,Sti,fzin,MiniMcMk","",""
   ,                             "/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd",kFALSE},
  {"eval_Sti_pp500.pileup.MC.y2011","","","MC.fast.y2011,Sti,fzin,MiniMcMk","",""
   ,"/star/rcf/simu/pp500/pythia/pileup/rcf10100_90_200evts_Wplus_enu.fzd\n"
   " gfile b /star/rcf/simu/pp500/pythia/pileup/rcf10100_90_4000evts_minb.fzd\n"
   " mode TPCE back 4001400\n gback 400 400 0.1 106.6"                                             ,kFALSE},
  {"eval_Stv_auau200.MC.y2011","","","MC.in.y2011,Stv,StvPulls,in,MiniMcMk","",""
   ,                                               "/star/rcf/simu/rcf11023_2060_25evts.event.root",kFALSE},
  {"eval_StvCA_auau200.MC.y2011","","","MC.in.y2011,StvPulls,StvCA,in,MiniMcMk","",""
   ,                                               "/star/rcf/simu/rcf11023_2060_25evts.event.root",kFALSE},
  {"eval_Sti.AgML_CuAu200.MC.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12003_1_100evts.fzd",kFALSE},
  {"eval_Sti.AgML_pp200.MC.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12000_1_1000evts.fzd",kFALSE},
  {"eval_Sti.AgML_pp500.MC.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12001_1_1000evts.fzd",kFALSE},
  {"eval_Sti.AgML_UU200.MC.y2012","","","MC.y2012,Sti,AgML,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12002_1_100evts.fzd",kFALSE},
  {"eval_Sti_CuAu200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12003_1_100evts.fzd",kFALSE},
  {"eval_Sti_pp200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12000_1_1000evts.fzd",kFALSE},
  {"eval_Sti_pp500.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                       "/star/rcf/simu/rcf12001_1_1000evts.fzd",kFALSE},
  {"eval_Sti_UU200.MC.y2012","","","MC.y2012,Sti,fzin,MiniMcMk","",""
   ,                                                        "/star/rcf/simu/rcf12002_1_100evts.fzd",kFALSE},
  {"eval_Sti.AgML.RC.auau200.MB.y2007","","","RC.y2007,pmdReco,ITTF,AgML,Sti","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_StiCA.RC.auau200.MB.y2007","","","RC.y2007,pmdReco,StiCA","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_StiCA.RC.auau200.y2007","","","RC.y2007,pmdReco,StiCA","",""
   ,                       "/star/rcf/test/daq/2007/112/8112052/st_physics_8112052_raw_1020010.daq",kFALSE},
  {"eval_Sti.RC.auau200.MB.y2007","","","RC.y2007,pmdReco,ITTF,Sti","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_Stv.AgML.RC.auau200.MB.y2007","","","RC.y2007,Stv,AgML","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_StvCA.RC.auau200.MB.y2007","","","RC.y2007.NoSvt,StvCA","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_Stv.RC.auau200.MB.y2007","","","RC.y2007.NoSvt,Stv","",""
   ,                       "/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",kFALSE},
  {"eval_Sti.AgML.RC.dau200.y2008","","","RC.y2008,ITTF,AgML,Sti","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_StiCA.RC.dau200.y2008","","","RC.y2008,StiCA","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_StiCA.RC.pp200.y2008","","","RC.pp.y2008,StiCA","",""
   ,                               "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",kFALSE},
  {"eval_Sti.RC.dau200.y2008","","","RC.y2008,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_Sti.RC.pp200.y2008","","","RC.pp.y2008,ITTF,Sti","",""
   ,                               "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",kFALSE},
  {"eval_Stv.AgML.RC.dau200.y2008","","","RC.y2008.notof,Stv,AgML","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_StvCA.RC.dau200.y2008","","","RC.y2008.notof,StvCA","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_StvCA.RC.pp200.y2008","","","RC.pp.y2008.Minuit,StvCA","",""
   ,                               "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",kFALSE},
  {"eval_Stv.RC.dau200.y2008","","","RC.y2008.notof,Stv","",""
   ,                               "/star/rcf/test/daq/2007/352/st_physics_8352025_raw_1030011.daq",kFALSE},
  {"eval_Stv.RC.pp200.y2008","","","RC.pp.y2008.Minuit,Stv","",""
   ,                               "/star/rcf/test/daq/2008/043/st_physics_9043046_raw_2030002.daq",kFALSE},
  {"eval_Sti.AgML.RC.pp500.y2009","","","RC.pp.y2009.VFPP,ITTF,Sti","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_StiCA.RC.pp200.y2009","","","RC.pp.y2009.VFPP,StiCA","",""
   ,                              "/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq",kFALSE},
  {"eval_StiCA.RC.pp500.y2009","","","RC.pp.y2009.VFPP,StiCA","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_Sti.RC.pp200.y2009","","","RC.pp.y2009.VFPP,ITTF,Sti","",""
   ,                              "/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq",kFALSE},
  {"eval_Sti.RC.pp500.y2009","","","RC.pp.y2009.VFPP,ITTF,Sti","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_Stv.AgML.RC.pp500.y2009","","","RC.pp.y2009,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_StvCA.RC.pp200.y2009","","","RC.pp.y2009,StvCA","",""
   ,                              "/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq",kFALSE},
  {"eval_StvCA.RC.pp500.y2009","","","RC.pp.y2009,StvCA","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_Stv.RC.pp200.y2009","","","RC.pp.y2009,Stv","",""
   ,                              "/star/rcf/test/daq/2009/115/st_physics_10115020_raw_5020001.daq",kFALSE},
  {"eval_Stv.RC.pp500.y2009","","","RC.pp.y2009,Stv","",""
   ,                              "/star/rcf/test/daq/2009/085/st_physics_10085024_raw_2020001.daq",kFALSE},
  {"eval_Sti.AgML.RC.auau200.y2010","","","RC.y2010.notof,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_Sti.AgML.RC.auau39.y2010","","","RC.y2010.notof,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_StiCA.RC.auau11.y2010","","","RC.y2010,StiCA","",""
   ,                              "/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",kFALSE},
  {"eval_StiCA.RC.auau200.y2010","","","RC.y2010,StiCA","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_StiCA.RC.auau39.y2010","","","RC.y2010,StiCA","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_StiCA.RC.auau62.y2010","","","RC.y2010,StiCA","",""
   ,                              "/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",kFALSE},
#if 0
  {"eval_StiCA.RC.auau7.y2010","","","RC.y2010,StiCA","",""
   ,                              "/star/rcf/test/daq/2010/138/st_physics_11138001_raw_2020001.daq",kFALSE},
#endif
  {"eval_Sti.RC.auau11.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",kFALSE},
  {"eval_Sti.RC.auau200.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_Sti.RC.auau39.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_Sti.RC.auau62.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",kFALSE},
#if 0
  {"eval_Sti.RC.auau7.y2010","","","RC.y2010,Sti","",""
   ,                              "/star/rcf/test/daq/2010/138/st_physics_11138001_raw_2020001.daq",kFALSE},
#endif
  {"eval_Stv.AgML.RC.auau200.y2010","","","RC.y2010.notof,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_Stv.AgML.RC.auau39.y2010","","","RC.y2010.notof,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_StvCA.RC.auau11.y2010","","","RC.y2010.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",kFALSE},
  {"eval_StvCA.RC.auau200.y2010","","","RC.y2010.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_StvCA.RC.auau39.y2010","","","RC.y2010.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_StvCA.RC.auau62.y2010","","","RC.y2010.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",kFALSE},
#if 0
  {"eval_StvCA.RC.auau7.y2010","","","RC.y2010.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2010/138/st_physics_11138001_raw_2020001.daq",kFALSE},
#endif
  {"eval_Stv.RC.auau11.y2010","","","RC.y2010.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2010/157/st_physics_11157020_raw_2030001.daq",kFALSE},
  {"eval_Stv.RC.auau200.y2010","","","RC.y2010.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2010/029/st_physics_11029020_raw_1030002.daq",kFALSE},
  {"eval_Stv.RC.auau39.y2010","","","RC.y2010.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2010/100/st_physics_11100070_raw_1030001.daq",kFALSE},
  {"eval_Stv.RC.auau62.y2010","","","RC.y2010.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2010/098/st_physics_11098050_raw_3020001.daq",kFALSE},
#if 0
  {"eval_Stv.RC.auau7.y2010","","","RC.y2010.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2010/138/st_physics_11138001_raw_2020001.daq",kFALSE},
#endif
  {"eval_Sti.AgML.RC.auau200.y2011","","","RC.y2011,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_Sti.AgML.RC.auau20.y2011","","","RC.y2011,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_Sti.AgML.RC.pp500.y2011","","","RC.pp.y2011.VFPPV,pmdReco,mtdDat,Sti","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"eval_StiCA.RC.auau200.y2011","","","RC.y2011,StiCA","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_StiCA.RC.auau20.y2011","","","RC.y2011,StiCA","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_StiCA.RC.auau27.y2011","","","RC.y2011,StiCA","",""
   ,                              "/star/rcf/test/daq/2011/174/st_physics_12174106_raw_2040001.daq",kFALSE},
  {"eval_StiCA.RC.pp500.y2011","","","RC.pp.y2011.VFPPV,pmdReco,mtdDat,StiCA","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"eval_Sti.RC.auau200.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_Sti.RC.auau20.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_Sti.RC.auau27.y2011","","","RC.y2011,Sti","",""
   ,                              "/star/rcf/test/daq/2011/174/st_physics_12174106_raw_2040001.daq",kFALSE},
  {"eval_Sti.RC.pp500.y2011","","","RC.pp.y2011.VFPPV,pmdReco,mtdDat,Sti","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"eval_Stv.AgML.RC.auau200.y2011","","","RC.y2011,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_Stv.AgML.RC.auau20.y2011","","","RC.y2011,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_StvCA.RC.auau200.y2011","","","RC.y2011,StvCA","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_StvCA.RC.auau20.y2011","","","RC.y2011,StvCA","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_StvCA.RC.auau27.y2011","","","RC.y2011,StvCA","",""
   ,                              "/star/rcf/test/daq/2011/174/st_physics_12174106_raw_2040001.daq",kFALSE},
  {"eval_StvCA.RC.pp500.y2011","","","RC.pp.y2011,pmdReco,mtdDat,StvCA","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"eval_Stv.RC.auau200.y2011","","","RC.y2011,Stv","",""
   ,                              "/star/rcf/test/daq/2011/130/st_physics_12130084_raw_5020002.daq",kFALSE},
  {"eval_Stv.RC.auau20.y2011","","","RC.y2011,Stv","",""
   ,                              "/star/rcf/test/daq/2011/122/st_physics_12122018_raw_2010002.daq",kFALSE},
  {"eval_Stv.RC.auau27.y2011","","","RC.y2011,Stv","",""
   ,                              "/star/rcf/test/daq/2011/174/st_physics_12174106_raw_2040001.daq",kFALSE},
  {"eval_Stv.RC.pp500.y2011","","","RC.pp.y2011,pmdReco,mtdDat,Stv","",""
   ,                              "/star/rcf/test/daq/2011/059/st_physics_12059038_raw_2030002.daq",kFALSE},
  {"eval_Sti.AgML.RC.pp200.y2012","","","RC.pp.y2012,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_Sti.AgML.RC.pp500.y2012","","","RC.pp.y2012,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"eval_Sti.AgML.RC.UU193.y2012","","","RC.y2012,AgML,Sti","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"eval_StiCA.RC.pp200.y2012","","","RC.pp.y2012,StiCA","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_Sti.RC.pp200.y2012","","","RC.pp.y2012,Sti","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_Sti.RC.pp500.y2012","","","RC.pp.y2012,Sti","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"eval_Sti.RC.UU193.y2012","","","RC.y2012,Sti","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"eval_Stv.AgML.RC.cuAu200.y2012","","","RC.y2012b.notof,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2012/143/st_physics_13143018_raw_3020009.daq",kFALSE},
  {"eval_Stv.AgML.RC.pp200.y2012","","","RC.pp.y2012.notofMin,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_Stv.AgML.RC.pp500.y2012","","","RC.pp.y2012.notofMin,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"eval_Stv.AgML.RC.UU193.y2012","","","RC.y2012b.notof,Stv,AgML","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"eval_StvCA.RC.cuAu200.y2012","","","RC.y2012b.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2012/143/st_physics_13143018_raw_3020009.daq",kFALSE},
  {"eval_StvCA.RC.pp200.y2012","","","RC.pp.y2012.notofMin,StvCA","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_StvCA.RC.pp500.y2012","","","RC.pp.y2012.notofMin,StvCA","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"eval_StvCA.RC.UU193.y2012","","","RC.y2012b.notof,StvCA","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
  {"eval_Stv.RC.cuAu200.y2012","","","RC.y2012b.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2012/143/st_physics_13143018_raw_3020009.daq",kFALSE},
  {"eval_Stv.RC.pp200.y2012","","","RC.pp.y2012.notofMin,Stv","",""
   ,                              "/star/rcf/test/daq/2012/044/st_physics_13044030_raw_2010001.daq",kFALSE},
  {"eval_Stv.RC.pp500.y2012","","","RC.pp.y2012.notofMin,Stv","",""
   ,                              "/star/rcf/test/daq/2012/082/st_physics_13082004_raw_1020004.daq",kFALSE},
  {"eval_Stv.RC.UU193.y2012","","","RC.y2012b.notof,Stv","",""
   ,                              "/star/rcf/test/daq/2012/115/st_physics_13115004_raw_2010002.daq",kFALSE},
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
   ,"Base chain for 2001 (tpc+rhic)"                                                               ,kFALSE},
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
  {"dau2003"     ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,svt_daq,SvtD,ftpc","",""
   ,                       "Production chain for winter 2003 data (+ tof, svt (no est), ftpc, emc)",kFALSE},
  {"dau2003a"    ,""  ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,svt_daq,SvtD,ftpc,trgd","",""
   ,                 "Production chain for winter 2003 data (+ tof, svt (no est), ftpc, emc, trgd)",kFALSE},
  {"pp2003"      , "" ,"","B2003,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,svt_daq,SvtD,ftpc,trgd","",""
   ,                 "Production chain for Spring 2003 data (+ tof, svt (no est), ftpc, emc, trgd)",kFALSE},

  {"Idst"        ,""  ,"",                  "event,compend,EventQA"   ,"","","Turn on DST for ITTF",kFALSE},
  {"IAna"    ,""  ,"","dEdxY2,Kink2,xi2,CMuDst,analysis","",""  ,"Turn on Xi, Kink, dEdx and MuDst",kFALSE},
  {"BAna"    ,""  ,"","dEdxY2,CMuDst,analysis"          ,"",""            ,"Turn on dEdx and MuDst",kFALSE},
  {"B2003I"      ,"","","ry2003,in,TpxRaw,TpxClu,Idst,l0,tags,Tree,evout,svtDb"
   ,                                                               "","","Base chain for 2003 ITTF",kFALSE},

  {"dau2003i"    ,"","","B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,l3onl,tofDat,emcDY2,svt_daq,SvtD,ftpc,trgd"
   ,                               "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
  {"pp2003i","","","B2003I,IAna,CtbMatchVtx,Corr2,ppOpt,-PreVtx,l3onl,ToF,emcDY2,svt_daq,SvtD,ftpc,trgd"
   ,                               "","","Production chain for winter 2003 data dau2003a with ITTF",kFALSE},
  {"B2004"       ,"","","ry2004,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                           "Base chain for 2004 ITTF (tpc+svt)",kFALSE},
  // Notes:
  //  fcf was not added by default to allow switching if needed
  //  there is no PreVtx in tpcI so no need to do -PreVtx for pp chain
  //  SVT is added as base default, svtIT in chains
  {"P2004","" ,"","B2004,IAna,fcf,VFMinuit,ToF,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2"
   ,                   "","","Production chain for 2003/2004 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2004"      ,"" ,"",
   "B2004,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,ToF,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr4,OSpaceZ2"
   ,                   "","","Production chain for 2003/2004 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 5 Chains
  {"B2005"       ,"","","ry2005b,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                           "Base chain for 2005 ITTF (tpc+svt)",kFALSE},
  {"B2005a"      ,""       ,"","ry2005b,in,tpc_daq,tpcI,Physics,Idst,l0,tags,Tree,evout,svtDb,ssdDb","",""
   ,                                                          "Base chain for 2005 ITTF (tpc only)",kFALSE},
  {"B2005b"      ,"" ,"","ry2005f,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                                "Base chain for 2005 ITTF Geo f (tpc+svt only)",kFALSE},
  {"B2005c"      ,"" ,"","ry2005g,in,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,tags,Tree,evout,ssdDb","",""
   ,                                "Base chain for 2005 ITTF Geo g (tpc+svt only + dead material)",kFALSE},

  {"P2005"       ,"" ,"",   "B2005,IAna,fcf,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,                   "","","Production chain for 2004/2005 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2005b"      ,"" ,"",   "B2005b,IAna,fcf,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,                   "","","Production chain for 2004/2005 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2005c"      ,"" ,"",   "B2005c,IAna,fcf,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,                   "","","Production chain for 2004/2005 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},

  {"pp2005","" ,"","B2005,IAna,fcf,ppOpt,VFppLMV5,CtbMatchVtx,emcDY2,ftpc,trgd,ZDCvtx,svtIT,Corr3"
   ,                     "","","Production chain for 2005 pp data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005a","","","B2005a,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,emcDY2,ftpc,trgd,ZDCvtx,Corr4"
   ,                     "","","Production chain for 2005 pp data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2005b","","",   "B2005b,IAna,fcf,ppOpt,VFPPV,beamline,CtbMatchVtx,emcDY2,ftpc,trgd,ZDCvtx,Corr4"
   ,                     "","","Production chain for 2005 pp data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
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
   "B2006a,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,emcDY2,ftpc,trgd,ZDCvtx,Corr3"
   ,                     "","","Production chain for 2005 pp data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006b"      ,"" ,"",   // We cannot start with VFPPV as there are many asserts. ppLMV5 is safe until adjustment
   "B2006b,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,emcDY2,ftpc,trgd,ZDCvtx,Corr4,BeamBack"
   ,                     "","","Production chain for 2005 pp data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2006g"      ,"" ,"",     // added 2008 after geometry corrections
   "B2006g,IAna,fcf,ppOpt,VFPPVnoCTB,beamline,emcDY2,ftpc,trgd,ZDCvtx,Corr4,BeamBack"
   ,               "","","Production chain for 2005 pp data geo g (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  // Year 7 chains - Geometry 2007 hopefully fine
  {"T2007","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,Tree,evout"
   ,                                                             "","","TPC only chain,  2007 ITTF",kFALSE},
  {"B2007","","","ry2007,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,sptd,Idst,tags,Tree,evout"
   ,                                                 "","","Base chain for 2007 ITTF (tpc+svt+ssd)",kFALSE},
  {"B2007g","","","ry2007g,MakeEvent,in,tpc_daq,tpcI,fcf,svt_daq,SvtD,ssddat,sptd,Idst,tags,Tree,evout"
   ,                                           "","","Base chain for 2007 ITTF geo g (tpc+svt+ssd)",kFALSE},
  {"P2007"       ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5"
   ,                        "","","Production chain for 2007 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},

  {"P2007g"      ,"" ,"",   // chain was set in 2008 to account for missing material
   "B2007g,IAna,KeepSvtHit,hitfilt,VFMinuit2,emcDY2,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr5"
   ,          "","","Production chain for 2007 data, revised 2008 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  // startup for calib
  {"P2007a"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr3"
   ,                  "","","Production chain for 2007 data Corr3 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2007b"      ,"" ,"",
   "B2007,IAna,KeepSvtHit,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,svtIT,ssdIT,Corr4"
   ,                  "","","Production chain for 2007 data Corr4 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"B2008" ,"","","ry2008,in,tpc_daq,tpcI,fcf,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2008 ITTF (tpc)",kFALSE},
  {"B2008a","","","ry2008,in,tpcX,ToFx,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                           "Base chain for 2008 ITTF (tpc+tof)",kFALSE},
  // startup for calib
  {"P2008a"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr3,analysis"
   ,                  "","","Production chain for 2008 data Corr3 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008b"       ,"" ,"",
   "B2008,IAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                  "","","Production chain for 2008 data Corr4 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  // or VFPPVnoCTB
  {"pp2008a"      ,"" ,"",
   "B2008,IAna,hitfilt,ppOpt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                  "","","Production chain for 2008 data Corr3 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008c"       ,"" ,"",   // ATTENTION: the below chain was used for preliminary results on low energy
   "B2008,IAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                        "","","Production chain for 2008 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2008c"      ,"" ,"",  // Note: this chain was not used and may be removed
   "B2008,IAna,hitfilt,ppOpt,Minuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                  "","","Production chain for 2008 data Corr4 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  // convergence chains
  {"pp2008"     ,"" ,"",   // VFPPV was chosen for p+p as final production chain
   "B2008a,IAna,hitfilt,ppOpt,VFPPV,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                  "","","Production chain for 2008 data Corr3 (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},
  {"P2008"       ,"" ,"",  // this one is final and official production ready, June 2008
   "B2008a,IAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,Corr4,analysis"
   ,                        "","","Production chain for 2008 data (+ l3, tof, ftpc, e/b-emc, trgd)",kFALSE},

  //
  // Chains for 2009 run p+p essentially
  // Note that we always need to start with VFMinuit as VFPPV is full of asserts
  //
  {"B2009.1","","","ry2009,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2009 ITTF (tpc)",kFALSE},
  {"B2009.2","","","ry2009a,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2009 ITTF (tpc)",kFALSE},
  {"B2009.3","","","ry2009d,in,tpcX,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2009 ITTF (tpc)",kFALSE},

  {"pp2009a"      ,"" ,"",
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                   "","","Production chain for 2009 data - no Corr (+ l3, ftpc, e/b-emc, trgd)",kFALSE},
  {"pp2009b"      ,"" ,"",
   "B2009.1,IAna,hitfilt,ppOpt,VFMinuit,emcDY2,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,             "","","Production chain for 2009 data - no Corr (+ l3, ftpc, e/b-emc, no trigger)",kFALSE},
  {"pp2009c"      ,"" ,"",
   "B2009.2,BAna,hitfilt,ppOpt,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis","",""
   ,                   "Production chain for 2009 data - no Corr, no VF (+l3, ftpc, e/b-emc, trig)",kFALSE},
  {"pp2009d"      ,"" ,"",
   "B2009.3,BAna,hitfilt,ppOpt,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis","",""
   ,                   "Production chain for 2009 data - no Corr, no VF (+l3, ftpc, e/b-emc, trig)",kFALSE},


  // chains for year 10
  {"B2010","","","ry2010,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2010 ITTF (tpc)",kFALSE},
  {"B2010c","","","ry2010c,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2010 ITTF (tpc)",kFALSE},

  {"P2010a","" ,"",  // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt
   "B2010,BAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                         "","","Production chain for 2010 data - no Corr (+ l3, ftpc, e/b-emc)",kFALSE},
  {"pp2010a","" ,"", // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt,VFPPVnoCTB
   "B2010,BAna,hitfilt,ppOpt,emcDY2,trgd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                  "","","Production chain for 2010 data - no Corr (+ l3, ftpc, e/b-emc, no VF)",kFALSE},
  {"P2010c","" ,"",  // use of y2010c geometry
   "B2010c,BAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                         "","","Production chain for 2010 data - no Corr (+ l3, ftpc, e/b-emc)",kFALSE},
  {"pp2010c","" ,"", // use of y2010c geometry
   "B2010c,BAna,hitfilt,ppOpt,emcDY2,trgd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                  "","","Production chain for 2010 data - no Corr (+ l3, ftpc, e/b-emc, no VF)",kFALSE},


  // chains for year 11
  {"B2011","","","ry2011,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2011 ITTF (tpc)",kFALSE},

  {"P2011a","" ,"",  // initial chain - Add some to all of BEmcChkStat,QAalltrigs,trgd,btof,Corr3,-hitfilt
   "B2011,BAna,hitfilt,VFMinuit,emcDY2,ftpc,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                         "","","Production chain for 2011 data - no Corr (+ l3, ftpc, e/b-emc)",kFALSE},
  {"pp2011a","" ,"", // initial chain - Add some to all of BEmcChkStat,QAalltrigs,btof,Corr3,-hitfilt,VFPPVnoCTB
   "B2011,BAna,hitfilt,ppOpt,emcDY2,trgd,ftpc,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                  "","","Production chain for 2011 data - no Corr (+ l3, ftpc, e/b-emc, no VF)",kFALSE},


  // chains for year 12
  {"B2012","","","ry2012,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2012 ITTF (tpc)",kFALSE},
  {"pp2012a","" ,"","B2012,BAna,hitfilt,ppOpt,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,        "","",                "Production chain for 2012 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},

  {"P2012a","" ,"",
   "B2012,BAna,hitfilt,VFMinuit,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                               "","","Production chain for 2011 data - no Corr (+ l3, e/b-emc)",kFALSE},
  {"B2012b","","","ry2012a,in,tpcX,ITTF,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2012 ITTF (tpc)",kFALSE},
  {"pp2012b","" ,"",
   "B2012b,BAna,hitfilt,ppOpt,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,        "","",                "Production chain for 2012 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},

  {"P2012b","" ,"",
   "B2012b,BAna,hitfilt,VFMinuit,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,                               "","","Production chain for 2011 data - no Corr (+ l3, e/b-emc)",kFALSE},
  // Year 13 chains
  {"B2013","","","in,tpcX,UseXgeom,ITTF,NosvtIT,NossdIT,tpcDB,TpcHitMover,Idst,tags,Tree,evout,"
   "btof,mtd,BAna,emcDY2,trgd,ZDCvtx","","",                       "Base chain for 2013 ITTF (tpc)",kFALSE},
  {"B2013x","","","ry2013_1x,B2013","","",                         "Base chain for 2013 ITTF (tpc)",kFALSE},
  {"pp2013x","","","B2013x,BAna,emcDY2,trgd,ZDCvtx" //hitfilt,
   ,        "","",                "Production chain for 2013 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},
  {"B2013"   ,"","","B2013_c2","","",                             "Alias for B2013 Configuration 2",kFALSE},
  {"B2013_c2","","","ry2013_2c,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2013 ITTF (tpc)",kFALSE},
  {"B2013_c1","","","ry2013_1c,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2013 ITTF (tpc)",kFALSE},

  {"pp2013a","" ,"",
   "B2013_c2,ITTF,UseXgeom,BAna,hitfilt,ppOpt,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis",
   "","",                "Production chain for 2013 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},
  {"pp2013b","" ,"",
   "B2013_c1,ITTF,UseXgeom,BAna,hitfilt,ppOpt,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,        "","",                "Production chain for 2013 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},

  // option is bare, no tracker and no Geom
  {"pp2013","" ,"",
   "BAna,hitfilt,ppOpt,emcDY2,trgd,ZDCvtx,NosvtIT,NossdIT,analysis"
   ,        "","",                "Production chain for 2013 data - no Corr (+ l3, e/b-emc, no VF)",kFALSE},
  // Year 14 chains
  {"B2014"  ,"","","ry2014,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2014 ITTF (tpc)",kFALSE},
  {"B2014a","","","ry2014a,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                              "Base chain for 2014a ITTF (tpc)",kFALSE},

  {"P2014","" ,"",
   "B2014,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,analysis"
   ,                      "","","Production chain for 2014 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  // Target Chain options for BES -> P2014a,DbV20140410,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D
  {"P2014a","" ,"",
   "B2014a,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis,"
   ,                      "","","Production chain for 2014 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  // Year 15 chains
  {"B2015"  ,"","","ry2015,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2015 ITTF (tpc)",kFALSE},

  {"B2015a"  ,"","","ry2015a,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                                "Base chain for run 2015a data",kFALSE},

  {"B2015b"  ,"","","ry2015b,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                  "Base chain for run 2015 with y2015b geometry",kFALSE},

  {"B2015c"  ,"","","ry2015c,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                  "Base chain for run 2015 with y2015c geometry",kFALSE},

  {"pp2015","" ,"",
   "B2015,ITTF,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                      "","","Production chain for 2015 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"pp2015a","" ,"",
   "B2015a,ITTF,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015a data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"pp2015b","" ,"",
   "B2015b,ITTF,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015b data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"pp2015c","" ,"",
   "B2015c,ITTF,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015c data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},


  {"P2015","" ,"",
   "B2015,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                      "","","Production chain for 2015 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"P2015a","" ,"",
   "B2015a,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015a data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"P2015b","" ,"",
   "B2015b,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015b data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"P2015c","" ,"",
   "B2015c,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                "","","Production chain for year 2015c data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},


  // Year 16 initial chains - no data to really test yet but placeholder
  // 2016 standard + one base with HftC (Complete)
  {"B2016"  ,"","","ry2016,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                               "Base chain for 2016 ITTF (tpc)",kFALSE},
  {"P2016","" ,"",
   "B2016,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                      "","","Production chain for 2016 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"B2016a"  ,"","","ry2016a,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                         "Production chain for 2016 data (tpc)",kFALSE},
  {"P2016a","" ,"",
   "B2016a,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,StiHftC,analysis"
   ,                      "","","Production chain for 2016 data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},
  
  // 
  // Year 17, p+p and BES - first chains are FastOffline related as usual
  //
  {"B2017"  ,"","","ry2017,in,tpcX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,evout","",""
   ,                                                           "Base chain for run 2017 data (tpc)",kFALSE},

  {"pp2017","" ,"",
   "B2017,Sti,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline,l3onl,emcDY2,fpd,trgd,ZDCvtx,analysis"
   ,                    "","","Base chain for year 2017 pp data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},
  {"P2017","" ,"",
   "B2017,Sti,UseXgeom,BAna,hitfilt,VFMinuit,l3onl,emcDY2,fpd,trgd,ZDCvtx,analysis"
   ,                    "","","Base chain for year 2017 AA data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},


  {"B2017a"  ,"","","ry2017a,in,tpcX,CorrX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,picoWrite","",""
   ,                                                     "Production chain for run 2017 data (tpc)",kFALSE},

  {"pp2017a","" ,"",
   "B2017a,ITTF,UseXgeom,BAna,hitfilt,ppOpt,VFPPVnoCTB,beamline3D,l3onl,emcDY2,fpd,trgd,ZDCvtx,analysis"
   ,              "","","Production chain for year 2017 pp data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  {"P2017a","" ,"",
   "B2017a,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,beamline3D,l3onl,emcDY2,fpd,trgd,ZDCvtx,analysis"
   ,              "","","Production chain for year 2017 AA data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},



  {"B2018a"  ,"","","ry2018a,in,tpcX,CorrX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,picoWrite","",""
   ,                                                           "Base chain for run 2018 data (tpc)",kFALSE},

  {"P2018a","" ,"",
   "B2018a,ITTF,UseXgeom,BAna,hitfilt,VFMinuit,beamline3D,l3onl,emcDY2,epdHit,fpd,trgd,ZDCvtx,analysis"
   ,                    "","","Base chain for year 2018 AA data - no Corr (+ l3, bcc/fpd, e/b-emc)",kFALSE},

  // 2019 chains, BES
  {"B2019a" ,"","","ry2019a,in,tpcX,UseXgeom,iTpcIT,CorrX,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,picoWrite",
   "","",                                                      "Base chain for run 2019 data (tpc)",kFALSE},

  {"P2019a","" ,"",
   "B2019a,ITTF,BAna,iTpcIT,hitfilt,VFMinuit,beamline3D,etofa,btof,mtd,l3onl,emcDY2,epdHit,trgd,ZDCvtx,analysis"
   ,    "","",        "Base chain for year 2019 AA data - no Corr (+ l3, epd, mtd, b/etof, b-emc)",kFALSE},


  // 2020 initial chains
  {"B2020a" ,"","",
   "ry2020a,in,tpcX,UseXgeom,iTpcIT,CorrY,AgML,tpcDB,TpcHitMover,Idst,tags,Tree,picoWrite,picoVtxDefault,picoCovMtxWrite",
   "","",                                                      "Base chain for run 2020 data (tpc)",kFALSE},

  {"P2020a","" ,"",
   "B2020a,ITTF,BAna,iTpcIT,hitfilt,VFMinuit,etofa,btof,mtd,l3onl,emcDY2,epdHit,trgd,ZDCvtx,analysis"
   ,    "","",        "Base chain for year 2020 AA data - no Corr (+ l3, epd, mtd, b/etof, b-emc)",kFALSE},


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
  {"NoHits"      ,""  ,"",""                           ,"","","Don\'t write hits into Event.Branch",kFALSE},
  {"Kalman"      ,""  ,"",""                            ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
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
  {"huge"        ,""  ,"",""                                         ,"","","Set NwGEANT =80Mwords",kFALSE},
  {"clearmem"    ,""  ,"",""                           				  ,"","","Obsolete",kFALSE},
  {"adcOnly"     ,""  ,"",""                          ,"","","DAQMaker selects only TPC ADC events",kFALSE},
  {"InTree"      ,""  ,"","in",""                                     ,"","bfcTree Input Tree name",kFALSE},
  {"OutTree"     ,""  ,"","Tree",""                                  ,"","bfcTree Output Tree name",kFALSE},
  {"DstOut"      ,"","","",                                               "","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"McEvOut"     ,""  ,"","StMcEvent,Tree"                       ,"","","Write StMcEvent to StTree",kFALSE},
  {"EvOut"       ,""  ,"","Tree"                                   ,"","","Write StEvent to StTree",kFALSE},
  {"GeantOut"    ,""  ,"","Tree"                                ,"","","Write g2t tables to StTree",kFALSE},
  {"Simu"        ,""  ,"","" ,"","","Simulated Data, force to use Db time stamp from used geometry",kFALSE},
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
  {"CorrX"       ,"" ,"","ExB,OShortR,OBmap,OPr13,OIFC,OSectorAlign,NewTpcAlignment"
   ",-AlignSectors,-OBmap2D,-OClock,-OTwist"                             ,"","","New Tpc Alignment",kFALSE},
  {"CorrY"       ,"" ,"","ExB,OShortR,OBmap,OPr40,OIFC,OSectorAlign,NewTpcAlignment,OSpaceZ2,OGridLeakFull"
   ",-AlignSectors,-OBmap2D,-OClock,-OTwist"                             ,"","","New Tpc Alignment",kFALSE},
  {"ExB"         ,""  ,"","",""                                       ,"","Activate ExB correction",kFALSE},
  {"EB1"         ,""  ,"","",""                                     ,"","Force ExB configuration 1",kFALSE},
  {"EB2"         ,""  ,"","",""                                     ,"","Force ExB configuration 2",kFALSE},
  {"OBmap"       ,""  ,"","",""                                          ,"","ExB shape correction",kFALSE},
  {"OBmap2D"     ,""  ,"","",""                                      ,"","ExB 2 D shape correction",kFALSE},
  {"OTwist"      ,""  ,"","",""                                          ,"","ExB twist correction",kFALSE},
  {"OClock"      ,""  ,"","",""                                     ,"","Clock/tpc rot. correction",kFALSE},
  {"OPr13"       ,""  ,"","",""                                          ,"","PadRow 13 distortion",kFALSE},
  {"OPr40"       ,""  ,"","",""                                          ,"","PadRow 40 distortion",kFALSE},
  {"OCentm"      ,""  ,"","",""                                   ,"","Central membrane correction",kFALSE},
  {"OECap"       ,""  ,"","",""                                    ,"","EndCap (curved) correction",kFALSE},
  {"OIFC"        ,""  ,"","",""                                         ,"","Field Cage correction",kFALSE},
  {"OSpaceZ"     ,""  ,"","",""                                      ,"","Space Charge corrections",kFALSE},
  {"OSpaceZ2"    ,""  ,"","",""                                   ,"","Space Charge corrections R2",kFALSE},
  {"OShortR"     ,""  ,"","",""                                       ,"","Shorted Ring correction",kFALSE},
  {"OGridLeak"   ,""  ,"","",""                                          ,"","Grid Leak correction",kFALSE},
  {"OGridLeak3D" ,""  ,"","",""                                       ,"","3D Grid Leak correction",kFALSE},
  {"OGridLeakFull","" ,"","",""                                     ,"","Full Grid Leak correction",kFALSE},
  {"OGGVoltErr"  ,""  ,"","",""                                   ,"","GG voltage error correction",kFALSE},
  {"OSectorAlign",""  ,"","",""                        ,"","Sector alignment distortion correction",kFALSE},
  {"ODistoSmear" ,""  ,"","",""    ,"","Distortion smearing accounting for calibration resolutions",kFALSE},
  {"OAbortGap"   ,""  ,"","",""                ,"","Abort gap cleaning event distortion correction",kFALSE},
  {"AlignSectors",""  ,"","",""                                             ,"",STAR_CHAIN_OBSOLETE,kFALSE},
  {"DbRichSca"   ,""  ,"","detdb","","",                    "Force reading of Rich scalers from DB",kFALSE},
  {"EastOff"     ,""  ,"","",""                                  ,"","Disactivate East part of tpc",kFALSE},
  {"WestOff"     ,""  ,"","",""                                  ,"","Disactivate West part of tpc",kFALSE},
  {"AllOn"       ,""  ,"","",""                      ,"","Activate both East and West parts of tpc",kFALSE},
  {"ReadAll"     ,""  ,"","",""                                 ,"","Activate all branches to read",kFALSE},
  {"pp"          ,""  ,"",""                                             ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppOpt"       ,""  ,"","TrsPileUp","","",             "pp option without enabling special cases",kFALSE},
  {"TrsPileUp"   ,""  ,"","","","",                                              "Trs pile up mode",kFALSE},
  {"TrsToF"      ,""  ,"","","","",                       "Trs account for particle time of flight",kFALSE},
  {"SvtMatchVtx" ,""  ,"","",""                                             ,"",STAR_CHAIN_OBSOLETE,kFALSE},
  {"VtxOffSet"   ,""  ,"","",""                 ,"","Account Primary Vertex offset from y2000 data",kFALSE},
  {"Calibration" ,""  ,"","",""                                              ,"","Calibration mode",kFALSE},
  {"beamLine"    ,""  ,"","",""                                       ,"","LMV Beam line constrain",kFALSE},
  {"beamLine3D"  ,""  ,"","",""                                 ,"","Use beamline in 3D vertex fit",kFALSE},
  {"min2trkVtx"  ,""  ,"","",""                    ,"","...only 2 tracks needed for vertex finding",kFALSE},
  {"hitreuseon"  ,""  ,"","",""                     ,"","...do re-use hits for other tracks in Sti",kFALSE},
  // WARNING: introduction of usePct4Vtx with default:false breaks backward compatibility.
  // See related code in StBFChain.cxx for details
  {"usePct4Vtx"     ,""  ,"","",""                ,"","Use Post-Crossing Tracks for vertex finding",kFALSE},
  {"useBTOF4Vtx"    ,""  ,"","",""                 ,"","Use BTOF track matching for vertex ranking",kFALSE},
  {"useBTOFmatchOnly","","","useBTOF4Vtx",""                     ,"","Use BTOF track matching only",kFALSE},

  {"svt1hit"        ,""  ,"","",""                             ,"","Use 1 SVT hit only combination",kFALSE},
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
  {"tables"      ,"","","StDbT,ebyeT,emc_T,ftpcT,gen_T,geomT,globT,l3_T,sim_T,svt_T,tpc_T","","","",kFALSE},
  {"StDbT"       ,""  ,"","",""                                   ,"StDb_Tables","Load StDb_Tables",kFALSE},
  {"ctf_T"       ,""  ,"","",""                                     ,"ctf_Tables","Load ctf_Tables",kFALSE},
  {"ebyeT"       ,""  ,"","",""                                   ,"ebye_Tables","Load ebye_Tables",kFALSE},
  {"emc_T"       ,""  ,"","",""                                     ,"emc_Tables","Load emc_Tables",kFALSE},
  {"ftpcT"       ,""  ,"","",""                                   ,"ftpc_Tables","Load ftpc_Tables",kFALSE},
  {"gen_T"       ,""  ,"","",""                                     ,"gen_Tables","Load gen_Tables",kFALSE},
  {"geomT"       ,""  ,"","",""                           ,"geometry_Tables","Load geometry_Tables",kFALSE},
  {"globT"       ,""  ,"","",""                               ,"global_Tables","Load global_Tables",kFALSE},
  {"l3_T"        ,"",  "","",""                                       ,"l3_Tables","Load l3_Tables",kFALSE},
  {"mwc_T"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"sim_T"       ,""  ,"","",""                                     ,"sim_Tables","Load sim_Tables",kFALSE},
  {"svt_T"       ,""  ,"","",""                                     ,"svt_Tables","Load svt_Tables",kFALSE},
  {"tpc_T"       ,""  ,"",""                                            ,"" ,"",STAR_CHAIN_OBSOLETE,kFALSE},
  {"trg_T"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"vpd_T"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Embedding"   ,"","","-Simu"                                              ,"","","Embedding run",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Utilities   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Geometry+Mag","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"pgf77"       ,"" ,"","",""                                                ,"pgf77VMC","Fortran",kFALSE},
  {"rootcern"    ,"" ,"","geant3",""                                   ,"","ROOT minicern OBSOLETE",kFALSE},
  {"StarMiniCern","" ,"","geant3",""                       ,"","STAR addition to minicern OBSOLETE",kFALSE},
  {"mysql"       ,"" ,"","",""                                            ,"libmysqlclient","MySQL",kFALSE},
  {"libPhysics"  ,"" ,"","",""                                              ,"libPhysics","TVector",kFALSE},
  {"geant3vmc"   ,"" ,"","-usexgeom,-xgeometry","",        "libGeom,libVMC,libgeant3", "VMC geant3",kFALSE},
  {"geant3"      ,"" ,"","geant3vmc",""   ,"EG,Pythia6,EGPythia6","VMC geant3 plus ROOT EG,pythia6",kFALSE},
  {"geometry"    ,"" ,"","",""                                    ,"geometry","geometry+Mag.Field",kFALSE},
  {"StarMagField","", "","magF"                          ,"","VMC,StarMagField","Load StarMagField",kFALSE},
  {"geomNoField" ,"" ,"","-geometry,StarMagField"        ,"","geometryNoField","geometry-Mag.Field",kFALSE},
  {"xgeometry"   ,"" ,"","-geometry,-geomNoField"         ,"","xgeometry","AgML geometry-Mag.Field",kFALSE},
  {"UseProjectedVertex" ,"" ,"",""                ,"","","Run StBTofCalibMaker w/wo Primary Vertex",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"vpd"         ,""  ,"",""                                             ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"tls"         ,""  ,"",""                                             ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"rts"         ,""  ,"","",""                                                ,"RTS","load libRTS",kFALSE},
  {"daq"         ,""  ,"","rts",""                         ,"StDaqLib,StDAQMaker","Load StDAQMaker",kFALSE},
  {"SCL"         ,""  ,"","",""                         ,"StarClassLibrary","Load StarClassLibrary",kFALSE},
  {"SvtCL"       ,""  ,"","",""                                        ,"Geom,StSvtClassLibrary","",kFALSE},
  {"TbUtil"      ,""  ,"","sim_T,tpc_t,globT,SCL",""    ,"StTableUtilities","Load StTableUtilities",kFALSE},
  {"TofUtil"     ,""  ,"","",""                                       ,"StTofUtil","Load StTofUtil",kFALSE},
  {"BTofUtil"    ,""  ,"","",""                                     ,"StBTofUtil","Load StBTofUtil",kFALSE},
  {"ETofUtil"    ,""  ,"","",""                                     ,"StETofUtil","Load StETofUtil",kFALSE},
  {"MtdUtil"    ,""  ,"","",""                                        ,"StMtdUtil","Load StMtdUtil",kFALSE},
  {"StBichsel"   ,""  ,"","",""                         ,"StBichsel","Load Bichsel model for dE/dx",kFALSE},
  {"StEvent"   ,"","","globT,SCL,TRGDef,StBichsel,EmcUtil,TbUtil,detDb","","StEvent","Load StEvent",kFALSE},
  {"PxlUtil"     ,""  ,"","","",                                       "StPxlUtil","Load StPxlUtil",kFALSE},
  {"IstUtil"     ,""  ,"","","",                                       "StIstUtil","Load StIstUtil",kFALSE},
  {"SsdUtil"     ,""  ,"","StarMagField,StEvent",""               ,"Geom,StSsdUtil","Load SSD Util",kFALSE},
  {"SstUtil"     ,""  ,"","StarMagField,StEvent",""               ,"Geom,StSstUtil","Load SST Util",kFALSE},
  {"EmcUtil"     ,""  ,"","emc_T,geomT,StDbT",""                      ,"StEmcUtil","Load StEmcUtil",kFALSE},
  {"EEmcUtil"    ,""  ,"","",""                                     ,"StEEmcUtil","Load StEEmcUtil",kFALSE},
  {"FmsUtil"     ,""  ,"","",""                             ,"StFmsUtil,libMinuit","Load StFmsUtil",kFALSE},
  {"FgtUtil"     ,""  ,"","",""                                       ,"StFgtUtil","Load StFgtUtil",kFALSE},
  {"GmtUtil"     ,""  ,"","",""                                       ,"StGmtUtil","Load StGmtUtil",kFALSE},
  {"l3Util"      ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"PmdUtil"     ,""  ,"","","",                                       "StPmdUtil","Load StPmdUtil",kFALSE},

  {"QUtils"      ,""  ,"","PmdUtil,EmcUtil","",                      "","Load QA Libs dependencies",kFALSE},
  {"Stu"         ,""  ,"","","",                         "StEventUtilities","Load StEventUtilities",kFALSE},

#ifndef  __NoStrangeMuDst__
  {"MuDSTDeps"   ,"","","StEvent,Stu,StBichsel",""
   ,                                     "StStrangeMuDstMaker,Tree","Load MuDST misc. dependencies",kFALSE},
  {"MuDST"       ,"" ,"","MuDSTDeps,EmcUtil,TofUtil,BTofUtil,PmdUtil",""
   ,                                                            "StMuDSTMaker","Load MuDST library",kFALSE},
#else /* __NoStrangeMuDst__  
  // If NoStrangeMuDst, StMuDSTMaker has to be c with -D__NO_STRANGE_MUDST__*/
  {"MuDSTDeps"   ,"" ,"","StEvent,Stu,StBichsel",   "","Tree","Load MuDST misc. dependencies (all)",kFALSE},
  {"MuDST"       ,"" ,"","MuDSTDeps,EmcUtil,TofUtil,BTofUtil,PmdUtil",""
   ,                                                   "StMuDSTMakerNoStrange","Load MuDST library",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"picoEvt"    ,"","","StEvent,Stu","",            "StPicoEvent","Load picoEvent and dependencies",kFALSE},
  {"picoDst"    ,"","","picoEvt,EmcUtil,TofUtil,BTofUtil,PmdUtil",""
   ,                                                        "StPicoDstMaker","Load PicoDST library",kFALSE},


  {"geantL","","","geomT,gen_T,sim_T,StarMagField","","geometry,Geom,St_db_Maker,St_g2t,St_geant_Maker"
   ,                                                                               "Load GeantLibs",kFALSE},
  {"gstarLib","","",""                                                 ,"","gstar","Load gstar lib",kFALSE},
  {"flux"        ,"","","simu"                                           ,"","flux","Load flux lib",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"I/O Makers  ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"gstar"       ,"geant"  ,"","-fzin,-ntin,-geant,Simu,geantL","St_geant_Maker"
   ,                                        "","gstar for 80 muon tracks with pT = 1GeV in |eta|<4",kFALSE},
  {"pythia"      ,"geant" ,"","-gstar,-fzin,-ntin,-geant,geantL","St_geant_Maker"
   ,                                "Pythia6_4_26,bpythia","Load Pythia in starsim, set pp 510 GeV",kFALSE},
  {"Wenu"        ,"" ,"","pythia","", ""                 ,"set pp 510 GeV -> W+/- -> e+/- nu/nubar",kFALSE},
  {"hijing"      ,"geant" ,"","-gstar,-fzin,-ntin,-geant,geantL","St_geant_Maker"
   ,"StarGeneratorUtil,StarGeneratorEvent,StarGeneratorBase,Hijing1_383"
   ,                                                      "Load Hijing in starsim, set AuAu200 GeV",kFALSE},
  {"tdaq"        ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"miniDAQ"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"fzin"        ,"geant","","Simu,-gstar,-ntin,-geant,geantL","St_geant_Maker",""
   ,                                                                           "read gstar fz-file",kFALSE},
  {"UseXgeom","","","-geometry,-geomNoField,xgeometry","","","suppress mortran version of geometry",kFALSE},
  {"in"         ,""  ,"",""              ,     "StIOMaker","StIOMaker","Read [DAQ|ROOT] input file",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Db makers   ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"db"          ,"db"   ,"","StDbT"             ,"St_db_Maker","StDbLib,StDbBroker,St_db_Maker","",kFALSE},
  {"magF"        ,"MagField","","StDbT,db,detDb","StMagFMaker","StarMagField,StMagF"
   ,                                                      "Mag.field map with scale factor from Db",kFALSE},
  {"detDb","detDb","","db","StDetectorDbMaker","StDetectorDbMaker","Load StDetectorDbMaker library",kFALSE},
  {"mtin"        ,"geant"  ,"","-fzin,-geant,-gstar,geantL,gstarLib,-magF","St_geant_Maker"
   ,                                                                "gstar","read event from MuDst",kFALSE},
  {"tpcDB"   ,"tpcDB","","tpc_T,dbutil,detDb,StarMagField,magF,StEvent","StTpcDbMaker","StTpcDb","",kFALSE},
  {"dbutil"      ,""     ,"","detDb,StDbT"                 ,"","StDbUtilities","Load StDbUtilities",kFALSE},
  {"svtDb"       ,"svtDb","","tpcDb,SvtCL", "StSvtDbMaker","StSvtDbMaker","Load and run SvtDbMaker",kFALSE},
  // both sst and ssd are in the same SsdX libraries -----------V
  {"ssdDb"      ,"ssdDb","","tpcDb,SsdUtil","StSsdDbMaker","StSsdDbMaker","Load and run SsdDbMaker",kFALSE},
  {"sstDb"      ,"sstDb","","tpcDb,SstUtil","StSstDbMaker","StSsdDbMaker","Load and run SstDbMaker",kFALSE},

  {"svtCalDb"    ,""     ,"","svtDb"         ,"","","Declare Calibrations/svt as while list member",kFALSE},
  {"ssdCalDb"    ,""     ,"","ssdDb"         ,"","","Declare Calibrations/ssd as while list member",kFALSE},
  {"sstCalDb"    ,""     ,"","sstDb"         ,"","","Declare Calibrations/sst as while list member",kFALSE},

  {"eemcDb"      ,"eeDb" ,"","db,EEmcUtil",      "StEEmcDbMaker","StEEmcDbMaker","Load EEmcDbMaker",kFALSE},
  {"fmsDb"       ,"fmsDb","","db,fmsutil",          "StFmsDbMaker","StFmsDbMaker","Load FmsDbMaker",kFALSE},
  {"fgtDb"       ,"fgtDb","","db,fgtutil",          "StFgtDbMaker","StFgtDbMaker","Load FgtDbMaker",kFALSE},
  {"pxlDb"       ,"pxlDb","","tpcDb PxlUtil",       "StPxlDbMaker","StPxlDbMaker","Load PxlDbMaker",kFALSE},
  {"istDb"       ,"istDb","","tpcDb",               "StIstDbMaker","StIstDbMaker","Load IstDbMaker",kFALSE},

  {"epdDb"       ,"epdDb","","tpcDb",               "StEpdDbMaker","StEpdDbMaker","Load EpdDbMaker",kFALSE},

  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"MAKERS      ","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  // for simulation on fly Event time stamp is set outside of the simulation makers
  {"ntin"   ,"geant","","paw,-fzin,-geant,-gstar,Simu,geantL,gstarLib","St_geant_Maker"
   ,                                                        "","read event generated Hbook nt-file",kFALSE},
  {"PrepEmbed","","","geantEmb","StPrepEmbedMaker","St_geant_Maker"
   ,                                                             "Prepare kinematics for embedding",kFALSE},
  {"PythiaEmbed","","","pythia,-Simu","","",                     "Prepare kinematics for embedding",kFALSE},
  {"geant"       ,"geant","","geantL"                          ,"St_geant_Maker","","passive GEANT",kFALSE},
  {"geantEmb"    ,"geant","","geantL"                   ,"St_geant_Maker","","GEANT embedding mode",kFALSE},
  {"RootVMC","","","-geant,-fzin,-ntin,StarMagField,-geantL,-geometry,-geomNoField,geant3","","","",kFALSE},
  {"VMCAppl"     ,"","","geomT,gen_t,sim_T,tpcDB,RootVMC,","","StSvtDbMaker,StSsdDbMaker,StarVMCApplication"
   ,                                                                                       "VMC G3",kFALSE},
  {"VMC"         ,"geant","","Simu,VMCAppl,-geant","StVMCMaker",           "StVMCMaker","VMC Maker",kFALSE},
  {"VMCPassive"  ,"geant","","VMCAppl",       "StVMCMaker","StVMCMaker","VMC Maker in Passive Mode",kFALSE},
  {"trg"         ,"trg","l0Chain","trg_T,globT,db","St_trg_Maker","St_trg,St_trg_Maker"
   ,                                                     "trigger analysis for Year 2001-2005 data",kFALSE},
  {"TRGDef"      ,""  ,"","",""                          ,"StTriggerDataMaker","Load StTriggerData",kFALSE},
  {"trgd"        ,"trgd","","TRGDef"  ,"StTriggerDataMaker","StTriggerDataMaker","Get trigger data",kFALSE},
  //  {"MakeEvent","0Event","","StEvent,tpcDB,detDb","StEventMaker","StEventMaker"
  //   ,                                                                     "<Early StEvent creation>",kFALSE},
  {"MakeEvent","0Event","","StEvent,detDb","StEventMaker","StTpcDb,StEventMaker"
     ,                                                                     "<Early StEvent creation>",kFALSE},
  {"LaserAvEv"          ,"","",""             ,"StLaserAvEventMaker","StLaserAvEventMaker","Hejrad",kFALSE},
  {"LaserAvCl"          ,"","",""               ,"StLaserAvClusterMaker","StLaserAvClusterMaker","",kFALSE},
  {"l0"          ,"l0Chain","","globT,ctf,trg"                              ,"StMaker","StChain","",kFALSE},
  {"ctf"     ,"ctf","l0Chain","ctf_T,db" ,"St_ctf_Maker","ctf,St_ctf,St_ctf_Maker","ToF simulation",kFALSE},
  {"mwc"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppMCTrig"    ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"pp2pp"       ,"","","",                              "St_pp2pp_Maker","St_pp2pp_Maker","pp->pp",kFALSE},

  // Note: MTD related chains - because of the TrkMask, need to happen before TpcHitMaker
  {"mtdFilt"  ,"","","", "StMtdEvtFilterApplyMaker","StDataFilterMaker","MTD-based event filtering",kFALSE},
  {"mtd"      ,"MtdChain","","mtdDat,mtdMatch","StMaker",                     "StChain","MTD Chain",kFALSE},
  {"mtdDat"   ,"mtd_raw","MtdChain","db,MuDst","StMtdHitMaker","StEvent,StMtdHitMaker"
   ,                                                                                "MTD hit maker",kFALSE},
  {"mtdTrkMask","","","db","StMtdTrackingMaskMaker"      ,"StMtdEvtFilterMaker","MTD track masking",kFALSE},  

  // EPD
  {"epdHit",    "", "", "epdDb,event",            "StEpdHitMaker", "StEpdHitMaker","EPD hit maker", kFALSE},

  {"tpc" ,"","","TpxRaw,TpxClu,tpcI" ,"","","WARNING *** Option is OBSOLETE *** use TpxClu instead",kFALSE},
  {"tpcI" ,"tpcChain","","db,tpcDB,TpcHitMover",                "StMaker","StChain","tpc with ITTF",kFALSE},
  {"tpcX" ,"tpcChain","","-tpcI,tpx,MakeEvent"            ,"StMaker","StChain","tpc+tpcx with ITTF",kFALSE},
  {"Trs","Trs","tpcChain","scl,tpcDB,TrsToF,StEvent,EmbeddingShortCut","StTrsMaker","StTrsMaker","",kFALSE},
  {"TpcRS","","tpcChain","scl,tpcDB,-Trs,-EmbeddingShortCut","StTpcRSMaker"
   ,"libMathMore,StdEdxY2Maker,StTpcRSMaker",                          "New Tpc Response Simulator",kFALSE},
  {"tfs"     ,"","","TpcFastSim" ,"","","WARNING *** Option is OBSOLETE *** use TpcFastSim instead",kFALSE},
  {"TpcFastSim"  ,"tpc_hits","tpcChain","MakeEvent,Simu,-trs,-TpcRS,-tcl,-tpxclu,-fcf,-tpc_daq,StEvent,"
   "-TpcHitMover,EmbeddingShortCut" ,"StTpcFastSimMaker","St_tcl_Maker","use tfs (no Trs or TpcRS)",kFALSE},
  {"EmbeddingShortCut","","","",              "","","Short Cut for StdEdxY2Maker and StTpcHitMover",kFALSE},
  {"StMcEvent"   ,"","","gen_t,sim_T"                                            ,"","StMcEvent","",kFALSE},
  {"McEvent" ,"","","StEvent,tpcDb,EEmcUtil,EmcUtil,StMcEvent","StMcEventMaker","StMcEventMaker","",kFALSE},
  {"Mixer"       ,"tpc_raw","","daq","StMixerMaker"                   ,"StTrsMaker,StMixerMaker","",kFALSE},
  {"St_tpc"      ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"St_svt"      ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},

  {"tpc_daq"  ,"","","TpxRaw",        "","","WARNING *** Option is OBSOLETE *** use TpxRaw instead",kFALSE},
  {"tcl","","","TpxRaw,TpxClu,MakeEvent","",""
  ,                 "WARNING *** Option is OBSOLETE *** use option TpxRaw,TpxClu,MakeEvent instead",kFALSE},
  {"fcf"      ,"","","-tcl,tpcX",       "","","WARNING *** Option is OBSOLETE *** use tpcX instead",kFALSE},
  {"tpx"         ,"tpc_hits","tpcChain","MakeEvent,tpc_T,StEvent,rts,detDb"
   ,                  "StTpcHitMaker","StTpcHitMaker","TPC hit reader for tpc + tpx via EVP_READER",kFALSE},


  // GMT
  {"gmt"        ,"GmtChain","","gmtDat,gmtClu"                ,"StMaker","StChain","Gmt data Chain",kFALSE},
  {"gmtDat"     ,"","GmtChain", "event","StGmtRawMaker","StGmtRawMaker",          "GMT Data reader",kFALSE},
  {"gmtClu"    ,"","GmtChain","gmtutil","StGmtClusterMaker","StGmtClusterMaker","GMT cluster maker",kFALSE},

  {"TpxPulser","TpxPulser","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker","TPC+TPX pulser analysis"
   ,                                                                                                kFALSE},
  {"TpxPadMonitor","","",""                                             ,"","",STAR_CHAIN_OBSOLETE, kFALSE},
  {"TpxAvLaser","TpxAvLaser","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
   ,                        "TPC+TPX averaging laser events into Sparse histogram on pixels level", kFALSE},
  {"TpxDumpPxls2Nt","TpxDumpPxls2Nt","tpcChain","rts,detDb","StTpcHitMaker","StTpcHitMaker"
   ,                                                                "TPC+TPX pixel dump to NTuple", kFALSE},
  {"TpxRaw","TpxRaw","tpcChain","rts,detDb,StEvent","StTpcHitMaker","StTpcHitMaker"
   ,              "TPC+TPX convert DAQ Tpc Raw Data to TpcRawEvent used by TpcMixer and/or TpxClu", kFALSE},
  {"TpcMixer","","tpcChain","StEvent,rts,-Mixer,-tpx,TpxClu"  ,"StTpcMixerMaker","StTpcHitMaker","",kFALSE},
  {"TpxClu","tpc_hits","tpcChain","rts,tpcDb,detDb,-tpx,-tpc_daq,-fcf","StTpcRTSHitMaker"
   ,"StTpcHitMaker",                                                    "RTS(online) cluster maker",kFALSE},
  {"TpxClu2D"    ,"","","TpxClu","","",                              "RTS(online) 2D-cluster maker",kFALSE},
  {"TpcAvCluster","TpcAvCluster","tpcChain","rts,detDb","StTpcAvClusterMaker","StTpcHitMaker"
   ,                       "TPC+TPX averaging laser events into Sparse histogram on cluster level", kFALSE},
  {"Velo"        ,"","tpcChain","tpc_T"                             ,"StVeloMaker","StVeloMaker","",kFALSE},
  {"TpcHitMover" ,"tpc_hit_mover","tpcChain","tpcDb,StEvent"
   ,                  "StTpcHitMover","StTpcHitMoverMaker","TPC hits coord transform + corrections",kFALSE},
  {"tpt","","ITTF","",                          "","","WARNING *** Option is OBSOLETE *** use ITTF",kFALSE},
  {"tpt_old","","ITTF","",                      "","","WARNING *** Option is OBSOLETE *** use ITTF",kFALSE},
  {"TpcT0"  ,"","",""                                                    ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ChargeStep","","",""                                                 ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"laser"  ,"","",""                                                    ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"PreVtx"  ,"","",""                                                   ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
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
  {"SvtVtx"      ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"stk"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Est"         ,"","","SvtIT",       "","","WARNING *** Option is OBSOLETE *** use SvtIT instead",kFALSE},
  {"global"      ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},
  {"Match"       ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},
  {"Vertex"      ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},
  {"Primary"     ,"","","ITTF",          "","","WARNING *** Option is OBSOLETE use ITTF instead***",kFALSE},
  {"V0"          ,"","","V02",            "","","WARNING *** Option is OBSOLETE use V02 instead***",kFALSE},
  {"Xi"          ,"","","Xi2",            "","","WARNING *** Option is OBSOLETE use Xi2 instead***",kFALSE},
  {"Kink"        ,"","","Kink2",        "","","WARNING *** Option is OBSOLETE use Kink2 instead***",kFALSE},
  {"dst"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Fglobal"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Fprimary"    ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"dEdx"        ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"svtdEdx"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Event"       ,  "","","MakeEvent",                          "","","Request to initialize event",kFALSE},

  {"pxlRaw"      ,"","","pxlDb",    "StPxlRawHitMaker", "StPxlRawHitMaker",     "PXL raw hit maker",kFALSE},
  {"pxlFastSim"  ,"","","pxlRaw,StMcEvent,StEvent"
   ,                                           "StPxlSimMaker","StPxlSimMaker","PXL Fast Simulator",kFALSE},
  {"pxlSlowSim"  ,"","","pxlRaw,StMcEvent,StEvent"
   ,                                           "StPxlSimMaker","StPxlSimMaker","PXL Slow Simulator",kFALSE},
   
  {"pxlCluster"  ,"","","pxlRaw", "StPxlClusterMaker", "StPxlClusterMaker",     "PXL cluster maker",kFALSE},
  {"pxlHit"      ,"","","event pxlCluster",   "StPxlHitMaker", "StPxlHitMaker",     "PXL hit maker",kFALSE},
  //{"pxlMon"    ,"","","StEvent"              ,"StPxlMonMaker","StPxlMonMaker","Example of Pxl QA",kFALSE},

  {"istSlowSim", "", "","StMcEvent,StEvent,IstUtil", "StIstSlowSimMaker", 
                                                             "StIstSimMaker", "IST Slow simulator", kFALSE},
  {"istFastSim","","","StMcEvent,StEvent","StIstFastSimMaker","StIstSimMaker","IST Fast simulator", kFALSE},
  {"istRawHit",  "", "", "istUtil,istDb","StIstRawHitMaker", "StIstRawHitMaker","IST RAWhit maker", kFALSE},
  {"istCluster", "", "", "istRawHit","StIstClusterMaker",  "StIstClusterMaker","IST Cluster maker", kFALSE},
  {"istHit",     "", "", "event,istCluster", "StIstHitMaker",       "StIstHitMaker","IST Hit Maker",kFALSE},

  {"ssddat"      ,"","","ssd_daq"                             ,"","","SSD full chain for Real Data",kFALSE},
  {"sstdat"      ,"","","sst_daq"                             ,"","","SST full chain for Real Data",kFALSE},
  {"ssd_daq","","","ssdCalDb,svt_T,-sls,-spa,ssdUtil","StSsdDaqMaker","StSsdDaqMaker","... SSD Daq",kFALSE},
  {"sst_daq","","","sstCalDb,svt_T,-sls,-spa,sstUtil","StSstDaqMaker","StSstDaqMaker","... SSDT Daq",kFALSE},

  {"ssdfast"     ,"","","ssdDb,StMcEvent,StEvent","StSsdFastSimMaker","StSsdFastSimMaker",
   "... SSD fast simulator"                                                                        ,kFALSE},
  {"ssd"         ,"","","ssdCalDb,sls,spa,sptd"              ,"","","SSD full chain for simulation",kFALSE},
  {"sls","","","McEvent,Simu,svt_T,SvtCL"
   ,                                "St_sls_Maker","StSsdSimulationMaker", "... SSD slow simulator",kFALSE},
  {"spa"         ,"SpaStrip","","Simu,svt_T,SvtCL,ssdUtil","St_spa_Maker","StSsdSimulationMaker"
   ,                                                                 "... SSD Pedestal Annihilator",kFALSE},
  {"SsdEmbed"   ,"","","","StSsdEmbeddingMaker","StSsdSimulationMaker","... SSD Mixing geom Maker" ,kFALSE},

  {"spt"        ,"","","sptd", "",""                    ,"Alias for sptd - please replace in chain",kFALSE},

  {"sptd"       ,"","","ssdUtil,svt_T", "StSsdPointMaker","StSsdPointMaker","... SSD Point Creator",kFALSE},
  {"sptt"       ,"","","sstUtil", "StSstPointMaker","StSstPointMaker",      "... SST Point Creator",kFALSE},

  {"sstHit",     "", "", "sst_daq,sptt", "",                          "","SST reconstruction chain",kFALSE},

  {"ssdpre"      ,"","","ssdEmbed,spa"                    ,"","","SSD full chain for pre-embedding",kFALSE},
  {"ssdAdd"     ,"","","ssd_daq","StSsdAddMaker","StSsdAddMaker",             "... SSD merge maker",kFALSE},
  {"ssdE"        ,"","","ssdpre,ssdAdd"                       ,"","","SSD full chain for embedding",kFALSE},





  {"ZDCVtx"      ,"","","db"                              ,"StZdcVertexMaker","StZdcVertexMaker","",kFALSE},
  {"emcDY2"   ,"emcRaw","","daq,eemcDb,EEmcUtil,emc_T,EmcUtil,StEvent,PreEcl,Epc"
   ,"StEmcRawMaker","StEmcRawMaker",                                    "B/E EMC data common maker",kFALSE},
  {"eemcD"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"emcY2"    ,"","","emc_T,tpc_T,db,emcSim,PreEcl,epc","","" //no  emcAtoE in simulation
   ,                        "EMC Chain for Y2A (must be before makers which include in this chain)",kFALSE},
  {"emcSim"   ,"","","emc_T,EmcUtil,McEvent,MuDST","StEmcSimulatorMaker","StEmcSimulatorMaker"
   ,                                                                       "New simulator for BEMC",kFALSE},
  {"EEfs"     ,"eefs","","eemcDb,EEmcUtil,MuDst","StEEmcFastMaker","StEEmcSimulatorMaker"
   ,                                                                          "EEMC fast simulator",kFALSE},
  {"EEss"     ,"eess","","-eefs,eemcDb,EEmcUtil,MuDst","StEEmcSlowMaker","StEEmcSimulatorMaker"
   ,                                                                          "EEMC slow simulator",kFALSE},
  {"BEmcMixer", "","","",                          "StEmcMixerMaker","StEmcMixerMaker","BEMC mixer",kFALSE},
  {"emcAtoE"  ,"bemcA2E","" ,"db","StEmcADCtoEMaker","StEmcADCtoEMaker"
   ,                                "B-EMC ADC to E converter  OBSOLETE for data in Run 9 or later",kFALSE},
  {"PreEcl"   ,"preecl","","" ,"StPreEclMaker",              "StPreEclMaker","B-EMC Cluster finder",kFALSE},
  {"Epc"      ,"epc","","PreEcl,EmcUtil"             ,"StEpcMaker","StEpcMaker","B-EMC point maker",kFALSE},
  {"EEmcMixer", "","","",                    "StEEmcMixerMaker","StEEmcSimulatorMaker","EEMC mixer",kFALSE},
  {"eemcA2E","eemcA2E","" ,"db","StEEmcA2EMaker",       "StEEmcA2EMaker","E-EMC ADC to E converter",kFALSE},
  {"eemCl"    ,"eemCl","","db","StEEmcClusterMaker"        ,"StEEmcClusterMaker","E-EMC clustering",kFALSE},

  // BTOF related chains
  {"btof"       ,"BTofChain","","btofDat,vpdCalib,btofMatch,btofCalib","StMaker"
   ,                                                                         "StChain","BTOF Chain",kFALSE},
  {"BtofDat"   ,"tof_raw","BTofChain","db,BTofutil","StBTofHitMaker","StEvent,StBTofHitMaker"
   ,                                                                               "BTOF hit maker",kFALSE},
  {"vpdSim"    ,"","VpdChain","BTofUtil","StVpdSimMaker","StEvent,StMcEvent,StBTofHitMaker,StVpdSimMaker"
   ,                                                                                "Vpd Simulator",kFALSE},
  {"vpdCalib","","BTofChain","db,BTofUtil","StVpdCalibMaker"   ,"StVpdCalibMaker","VPD calibration",kFALSE},
  {"btofSim" ,"","BTofChain","BTofUtil"  ,"StBTofSimMaker",
                       "StEvent,StMcEvent,StTofUtil,StBTofHitMaker,StBTofSimMaker","BTOF Simulator",kFALSE},
  {"btofMixer"    ,"","BTofChain","BTofUtil","StBTofMixerMaker","StEvent,StBTofHitMaker,StBTofMixerMaker"
   ,                                                                                   "BTof Mixer",kFALSE},

  // ETOF chains - do they have to be before the VPD / vpdsim?
  {"ETofDat",   "etof_raw","ETofChain", "db, ETofUtil", "StETofDigiMaker",  "StEvent,StETofDigiMaker",
                                                                                  "ETOF digi maker",kFALSE},
  {"ETofCalib", "",  "ETofChain", "db, ETofUtil, muDst", "StETofCalibMaker", "StETofCalibMaker",
                                                                                 "ETOF calibration",kFALSE},
  {"ETofSim" ,  "",        "ETofChain", "",                    "StETofSimMaker",  "StETofSimMaker",
                                                                                   "ETOF simulator",kFALSE},



  // left MTD chain for sumulation alone here
  {"mtdSim"    ,"","MtdChain","","StMtdSimMaker",           "StEvent,StMtdSimMaker","MTD Simulator",kFALSE},


  // Time Of Flight related options
  {"ToF"       ,"TofChain","","tofDat,tofrMatch,tofpMatch,tofCalib,geant","StMaker","StChain","ToF Chain",kFALSE},
  {"ToFx"      ,"TofChain","","tofXDat,tofrMatch,tofCalib,geant"        ,"StMaker","StChain","ToFx Chain",kFALSE},
  {"tofDat"    ,"tof_raw","TofChain","db,Tofutil","StTofMaker","StEvent,StTofMaker",
   "TOF Data base chain",                                                                           kFALSE},
  {"tofXDat"   ,"tof_raw","TofChain","db,Tofutil","StTofHitMaker","StEvent,StTofMaker,StTofHitMaker",
   "TOF hit maker",                                                                                 kFALSE},
  {"tofsim"    ,"","TofChain","TofUtil","StTofSimMaker","StEvent,StTofMaker,StTofSimMaker",
   "TOF Simulator, Tof version before y2009",                                                       kFALSE},
  {"tofrMatch" ,"","TofChain","db,TofUtil","StTofrMatchMaker","StTofrMatchMaker",
   "TPC to TOFr track matching",                                                                    kFALSE},
  {"tofpMatch"   ,"","TofChain","db,TofUtil","StTofpMatchMaker","StTofpMatchMaker",
   "TPC to TOFp track matching",                                                                    kFALSE},
  {"tofCalib"   ,"","TofChain","db,TofUtil","StTofCalibMaker","StTofCalibMaker",  "TOF calibration",kFALSE},
  // Filtering - all filters will have the pattern "FiltXXX"
  {"FiltGamma" ,"","","StEvent,StMcEvent,EmcUtil",
   "StGammaFilterMaker","StFilterMaker",  "BEmc Gamma filtering",                                   kFALSE},
  {"FiltEemcGamma" ,"","","StEvent,StMcEvent,EmcUtil",
   "StEemcGammaFilterMaker","StFilterMaker",  "EEmc Gamma filtering",                               kFALSE},
  {"rich"        ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Rrs"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"rch"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"RichPiD"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"l3"          ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"l3cl"        ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"l3t"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#if 1 /* keep for y2001 and y2003 data */
  {"l3onl"       ,"","",""                            ,"Stl3RawReaderMaker","Stl3RawReaderMaker","",kFALSE},
  {"l3count"     ,"","",""                              ,"Stl3CounterMaker","Stl3RawReaderMaker","",kFALSE},
#else
  {"l3onl"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"l3count"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#endif
  {"bbcSim"         ,"","","db","StBbcSimulationMaker"      ,"StBbcSimulationMaker","BBC Simulator",kFALSE},
  // FMS
  {"fmsSim",""    ,"","StEvent,fmsDb",
   "StFmsFastSimulatorMaker","StFmsFastSimulatorMaker","FMS Fast Simulator",                        kFALSE},
  {"fmsDat"     ,"","", "StEvent,fmsDb",
   "StFmsHitMaker","StFmsHitMaker","Fill FMS struct and zero TRG",                                  kFALSE},
  {"fpsDat"     ,"","", "StEvent,fmsdb",
   "StFpsRawHitMaker","StFpsRawHitMaker","Fill FPS struct",                                         kFALSE},
  {"fmsPoint"   ,"","", "event,fmsDb",
   "StFmsPointMaker","StFmsPointMaker","Fill FMS clusters and points",                              kFALSE},
  {"fmsfps"   ,"","", "event,fmsDb",
   "StFmsFpsMaker","StFmsFpsMaker","Fill FPS association in FMS points",                            kFALSE},

#if 0
  {"fpd"         ,"fpd","","",                  "StFpdMaker","StFpdMaker","FPD/BBC Data base chain",kFALSE},
#else
  {"fpd"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#endif
  // FGT
  {"fgt"        ,"FgtChain","","fgtDat,fgtClu,fgtAtoC"        ,"StMaker","StChain","Fgt data Chain",kFALSE},
  {"fgtDat"     ,"","", "event,fgtdb","StFgtRawMaker","StFgtRawMaker",            "FGT Data reader",kFALSE},
  {"fgtAtoC"    ,"","", "fgtdb",      "StFgtA2CMaker","StFgtA2CMaker",    "FGT ADC to Charge maker",kFALSE},
  {"fgtClu"     ,"","", "fgtutil",    "StFgtClusterMaker","StFgtClusterMaker",  "FGT cluster maker",kFALSE},
  {"fgtAtoC"    ,"","", "fgtdb",      "StFgtA2CMaker","StFgtA2CMaker",    "FGT ADC to Charge maker",kFALSE},
  {"fgtPoint"   ,"","", "event",      "StFgtPointMaker",    "StFgtPointMaker", "Creates FGT points",kFALSE},

  // Some global Sti stuff including vertexing
  {"genvtx"    ,""  ,"","EEmcUtil","StGenericVertexMaker"
   ,                        "Minuit,StGenericVertexMakerNoSti"
   ,                                "Generic Vertex Finder library (default is MinuitVertexFinder)",kFALSE},
  {"VFMinuit"  ,""  ,""  ,"genvtx"                   ,"","","... Generic VF will use Minuit method",kFALSE},
  {"CtbMatchVtx"    ,""  ,"","VFMinuit",""              ,"","... CTB Matching ON in Vertex Finding",kFALSE},
  {"VFMinuit2"      ,""  ,"","VFMinuit","","","... Generic VF will use Minuit method w/rank mode 2",kFALSE},
  {"VFMinuit3"      ,""  ,"","VFMinuit","","","... Generic VF will use Minuit method w/rank mode 3",kFALSE},
  {"VFFV"           ,""  ,"","genvtx"                            ,"","","... Fixed dummy VF method",kFALSE},
  {"VFMCE"          ,""  ,"","genvtx"                        ,"","","... Fixed vertex from MCEvent",kFALSE},
  {"VFppLMV"        ,""  ,"","genvtx"                ,"","","... VertexMaker will use ppLMV method",kFALSE},
  {"VFppLMV5"       ,""  ,"","VFppLMV"       ,"","","... VertexMaker will use ppLMV method (tuned)",kFALSE},
  // VFStore.value would work as well
  {"VFStoreX"       ,""  ,"",""        ,"","","... VertexMaker will use save at least 100 vertices",kFALSE},

  // Sti options
  {"StiPulls" ,"","",""                                         ,"","", "Request to make Sti Pulls",kFALSE},
  {"StvPulls" ,"","",""                                         ,"","", "Request to make Stv Pulls",kFALSE},
  {"StiLib"   ,"","",""                                           ,"","Sti,StiUtilities","Load Sti",kFALSE},
  {"StiCALib" ,"","",""                                      ,"","TPCCATracker,StiCA","Load Sti+CA",kFALSE},
  {"StiTpc"   ,"","","TpcDb,ITTF,tpc_T,dbutil,detDb,StarMagField,magF"   ,"","StiTpc","Load StiTpc",kFALSE},
  {"StiSvt"   ,"",""," "                  ,"","StSvtClassLibrary,StSvtDbMaker,StiSvt","Load StiSvt",kFALSE},
  {"StiSsd"   ,"","",""                           ,"","StSsdUtil,StSsdDbMaker,StiSsd","Load StiSsd",kFALSE},
  {"StiSst"   ,"","",""                        ,"","StSsdUtil,StSsdDbMaker,StiSsd","Load StiSsd(t)",kFALSE},
  {"StiPxl"   ,"","",""                           ,"","StPxlUtil,StPxlDbMaker,StiPxl","Load StiPxl",kFALSE},
  {"StiIst"   ,"","",""                           ,"","StIstUtil,StIstDbMaker,StiIst","Load StiIst",kFALSE},

  // left for historical purpose
  {"StiLibs"    ,"","","StiTpc,StiSvt,StiSsd,BTofUtil"
                                                       ,"","StEEmcDbMaker","Load Sti Detector libs",kFALSE},
  // switch to the HFT based chain
  {"StiLibsHft" ,"","","StiLibs,StiSst,StiPxl,StiIst"       ,"","","Load Sti Detector libs for HFT",kFALSE},
  {"StiHftP", "","","StiLibsHft,-SvtIT,-SsdIT,PxlIT,pxlDb",""      ,"","Enables partial  Hft (PXL)",kFALSE},
  {"StiHftC", "","","StiLibsHft,-SvtIT,-SsdIT,PxlIT,pxlDb,IstIT,istDb,SstIT,sstDb",""
   ,                                                        "","Enables complete Hft (PXL+IST+SST)",kFALSE},

  // depends on Sti symbols
  {"VFPPV"    ,""  ,"","      eemcDb,StiLib,-VFMinuit,-VFMinuit2,-VFMinuit3,","StGenericVertexMaker"
   ,                                "Minuit,StGenericVertexMaker","... Pile-up proof vertex finder",kFALSE},
  {"VFPPVnoCTB"     ,""  ,"","VFPPV",""                ,"","... Pile-up proof vertex finder, noCTB",kFALSE},
  {"VFPPVEv"    ,""     ,""	,"eemcDb"	,"StGenericVertexMaker"
   ,                        "Minui,StGenericVertexMaker"          ,"... StEvent based VFPPV, noCTB",kFALSE},
  {"VFPPVEvNoBtof"     	,""  ,""	,"VFPPVEv"	,""	     ,"","...VFPPVEv with no Btof ",kFALSE},


  // Sti/Stv chains
  {"Sti"      ,"Sti","","StiLib,StiLibs,SCL,StEvent,StDbT,TpcIT,compend,sim_T,tbutil","StiMaker"
   ,                      "StEventUtilities,StiUtilities,StiMaker","Sti tracker",kFALSE},
  {"StiCA"      ,"Sti","","Sti","StiMaker","TPCCATracker,StiCA"                    ,"StiCA tracker",kFALSE},
  {"StiCAPerf","","","","",                             "TPCCATrackerPerformance", "CA Performance",kFALSE},
  {"StiCA"    ,"Sti","","Sti,StiLib,StiCALib,StiLibs,SCL,StEvent,StDbT,TpcIT,compend,tbutil","StiMaker"
   ,                                "StEventUtilities,libEG,StiUtilities,StiMaker","Sti+CA tracker",kFALSE},
  {"HLTCA"    ,""  ,"Sti","",                  "StHLTCAMaker","StHLTCAMaker",  "HLT reconstruction",kFALSE},
  {"KFVertex" ,""  ,"Sti","-genvtx,-VFMinuit,-VFFV,-VFMCE,-VFppLMV,-VFPPVnoCTB,-VFPPV,-Kink2,-V02,-Xi2"
   ,"StKFVertexMaker",      "MathMore,Spectrum",  "...KFParticle based multi vertex reconstruction",kFALSE},
  {"Stv"     ,"Stv","","-xgeometry,-Sti,-StiTpc,-StiSsd,-StiSvt,-StiPxl,-StiSsd,-StiSst,-StiIst,","StvMaker"
  ,"libHist,libHistPainter,libVMC,StarMiniCern,geant3,GeoTestMaker,StvUtil,Stv,StvSeed,StvMaker,StEventUtilities,-StiLibs,-StiLibsHft","Stv",kFALSE},
  {"StvCA"    ,"StvCA","","Stv","",""                                                      ,"StvCA",kFALSE},
  {"StiVMC"   ,"StiVMC","","-Sti,SCL,StEvent,StDbT,TpcDb,compend","StiVMCMaker"
   ,                                      "StEventUtilities,StiVMC,StiVMCMaker" ,"ITTF VMC tracker",kFALSE},
  {"StiVMCLibs","","","detDb,StarMagField","",                      "","ITTF:load StiVMC libraries",kFALSE},
  {"laserIT"  ,"","","","",                              "TpcIT","use Sti for laser reconstruction",kFALSE},
  {"TpcIT"    ,"","","ITTF"                                              ,"","","Sti tracking: TPC",kFALSE},
  {"SvtIT"    ,"","","ITTF"                                              ,"","","Sti tracking: SVT",kFALSE},
  {"SsdIT"    ,"","","ITTF"                                              ,"","","Sti tracking: SSD",kFALSE},

  {"HpdIT"  ,""  ,"","ITTF",""                               ,"Sti,StiRnD","Sti tracking: Hpd geom",kFALSE},
  {"PixelIT",""  ,"","PxlIT",""                                               ,"","Alias for PxlIT",kFALSE},

  {"PxlIT"  ,""  ,"","ITTF",""                               ,"","Sti tracking: Pixel geom",kFALSE},
  {"IstIT"  ,""  ,"","ITTF",""                               ,"","Sti tracking: Ist geom",kFALSE},
  {"SstIT"  ,""  ,"","ITTF",""                               ,"","Sti tracking: Sst geom",kFALSE},
  {"iTpcIT" ,""  ,"","ITTF",""                               ,"","Sti tracking: iTpc geom + hits",kFALSE},

  {"BTofIT"  ,""  ,"","ITTF",""                            ,"","Sti tracking: BTof geom",kFALSE},
  {"NoSvtIT"     ,""  ,"","-SvtIT",""                    ,"","ITTF: track with switch off SVT geom",kFALSE},
  {"NoSsdIT"     ,""  ,"","-SsdIT",""                    ,"","ITTF: track with switch off SSD geom",kFALSE},
  {"NoSstIT"     ,""  ,"","-SstIT",""                    ,"","ITTF: track with switch off SST geom",kFALSE},
  {"NoPxlIT"     ,""  ,"","-PxlIT",""                    ,"","ITTF: track with switch off PXL geom",kFALSE},
  {"NoIstIT"     ,""  ,"","-IstIT",""                    ,"","ITTF: track with switch off IST geom",kFALSE},
  {"skip1row"    ,""  ,"","",""                           ,"","ITTF: skip the first pad row in TPC",kFALSE},
  {"StiRnD"   ,"","","",                                  "","StiRnD", "Load StiRnD shared library",kFALSE},
  {"Alignment"   ,"","","",                   "","", "Sti Tpc Alignment, reconstruction per sector",kFALSE},
  {"Cosmics"  ,"","","",                         "","", "Ignore events without recostructed tracks",kFALSE},
  {"StiPulls" ,"","","Sti",                                      "","", "Request to make Sti Pulls",kFALSE},
#if 0
  {"BeamBack" ,"","","StEvent","StBeamBackMaker","Minuit,StBeamBackMaker"
   ,                                                           "Beam background tracker in the TPC",kFALSE},
#else
  {"BeamBack" ,"","","StEvent"                                           ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#endif
  {"dEdxY2"       ,"dEdxY2","","tpcDb,StEvent","StdEdxY2Maker","libMinuit,StdEdxY2Maker"
   ,                                                                 "Bichsel method used for dEdx",kFALSE},

  // Options in need to be done after the tracker
  // second wave of BTOF options needed after Sti
  {"btofMatch" ,"","","db,BTofUtil,vpdCalib,btofCalib","StBTofMatchMaker","StBTofMatchMaker"
   ,                                                                      "TPC-BTOF track matching",kFALSE},
  {"btofCalib","","","db,BTofUtil",        "StBTofCalibMaker","StBTofCalibMaker","BTOF calibration",kFALSE},

  // ETOF hit building and track matching after bTofCalib - needs to be after btof	
  {"ETofHit",   "",      "ETofChain", "db, ETofUtil, muDst", "StETofHitMaker",   "StETofHitMaker",
                                                                                   "ETOF hit maker",kFALSE},
  {"ETofMatch",  "",     "ETofChain", "db, ETofUtil, muDst", "StETofMatchMaker", "StETofMatchMaker",
                                                                                 "ETOF match maker",kFALSE},
  {"ETofQa",     "",     "ETofChain", "db, ETofUtil, muDst", "StETofQAMaker",    "StETofQAMaker",
                                                                                    "ETOF QA maker",kFALSE},

  {"ETofA",  "",    "","etofdat,ETofCalib,etofhit,ETofMatch","","... ETOF chain options for data",  kFALSE},
 

  // the below needs to be done earlier to save time - leaving here for documentation purposes as two
  // makers are part of the same library (let's not forget this)
  //{"mtdTrkMask"  ,"","","db",    "StMtdTrackingMaskMaker","StMtdEvtFilterMaker","MTD track masking",kFALSE},
  {"mtdMatch"    ,"","","db,MtdUtil",  "StMtdMatchMaker","StMtdMatchMaker","TPC-MTD track matching",kFALSE},
  {"mtdCalib"    ,"","","db",                 "StMtdCalibMaker","StMtdCalibMaker","MTD calibration",kFALSE},
  {"mtdEvtFilt"  ,"","","db",       "StMtdEvtFilterMaker","StMtdEvtFilterMaker","MTD event filter" ,kFALSE},

  {"FindVtxSeed"   ,"FindVtxSeed"   ,"","globT,MuDSTDeps,picoDst","StVertexSeedMaker"
   ,                                   "StPass0CalibMaker",          "Performs vertex seed finding",kFALSE},
  {"FindEvtVtxSeed","FindEvtVtxSeed","","MuDSTDeps,picoDst","StEvtVtxSeedMaker"
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
  {"pmdReco"   ,"pmdReco","","PmdUtil,pmdClust"               ,"StMaker","StChain","PMD Reco chain",kFALSE},
  {"pmdRaw"    ,"pmdRaw","pmdReco","pmdReco,pmdRead"        ,"","","PMD Reco chain giving raw data",kFALSE},
  {"pmd"       ,"pmd","pmdReco","pmdReco,pmdSim,pmdDis"              ,"","", "PMD Simulation chain",kFALSE},
  {"pmdRead"   ,"","pmdReco","PmdUtil","StPmdReadMaker"     ,"StPmdReadMaker", "DAQ reader for PMD",kFALSE},
  {"pmdSim"    ,"","pmdReco","PmdUtil","StPmdSimulatorMaker","StPmdSimulatorMaker"
   ,"Hit Simulator for PMD"                                                                        ,kFALSE},
  {"pmdClust"  ,"pmdClust","pmdReco","","StPmdClusterMaker","StPmdClusterMaker"
   ,"ClusterMaker for PMD"                                                                         ,kFALSE},
  {"pmdDis"    ,"pmdDis","pmdReco","PmdClust","StPmdDiscriminatorMaker"
   ,                                              "StPmdDiscriminatorMaker","Discriminator for PMD",kFALSE},

#ifndef __NoStrangeMuDst__
  {"Kink2"       ,"kink2","","db,MuDST,-kink","StKinkMaker","StSecondaryVertexMaker"
   ,                                                                      "Find Kinks from StEvent",kFALSE},
  {"V02"         ,"v02","","db,MuDST,-V0","StV0FinderMaker","StSecondaryVertexMaker"
   ,                                                                        "Find V0s from StEvent",kFALSE},
  {"Xi2"         ,"xi2","","db,MuDST,-V02,-Xi","StXiFinderMaker","StSecondaryVertexMaker"
   ,                                                                     "Xis AND V0s from StEvent",kFALSE},
#else /* ! __NoStrangeMuDst__ */
  {"Kink2"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"V02"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Xi2"         ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#endif /* __NoStrangeMuDst__ */

  {"V0svt"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"Xisvt"       ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"SCEbyE"      ,"scebye","","MuDSTDeps,picoDst","StSpaceChargeEbyEMaker"
   ,                    "StMuDSTMaker,StPass0CalibMaker","Determine EbyE SpaceCharge using StEvent",kFALSE},
  {"SCScalerCal" ,"scscalercal","","MuDSTDeps,picoDst","StSpaceChargeEbyEMaker"
   ,                               "StMuDSTMaker,StPass0CalibMaker","Calibrate SpaceCharge scalers",kFALSE},
  {"compend"     ,"","","event,detDb","StEventCompendiumMaker","StEventCompendiumMaker"
   ,                                                             "Fill event summary in ITTF Chain",kFALSE},
  {"shadow"      ,"","","event,compend"                         ,"StShadowMaker","StShadowMaker","",kFALSE},
  {"TpcAligner"    ,"","","Alignment"         ,"StTpcAlignerMaker","StTpcAlignerMaker","TpcAligner",kFALSE},
  {"pec"         ,"PeC","","Event"                       ,"StPeCMaker","StPeCMaker","PCollAnalysis",kFALSE},
  {"RichSpectra"         ,"","",""                                       ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"TagsChain"   ,"TagsChain","",""                                         ,"StMaker","StChain","",kFALSE},
  {"TpcTag"      ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  //{"Flow"        ,"","TagsChain","StEvent,Stu"                      ,"StFlowMaker","StFlowMaker","",kFALSE},
  {"Flow"        ,"","",","                                              ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"FlowTag"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  //{"FlowAnalysis","","TagsChain","StEvent,Flow"     ,"StFlowAnalysisMaker","StFlowAnalysisMaker","",kFALSE},
  {"FlowAnalysis","","",""                                               ,"",STAR_CHAIN_OBSOLETE,"",kFALSE},
  //#ifndef __NoStrangeMuDst__
  //{"StrangeTags" ,"","TagsChain","StEvent"            ,"StStrangeTagsMaker","StStrangeTagsMaker","",kFALSE},
  //#endif
  {"SpectraTag"  ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"HeavyTags"   ,"","TagsChain","StEVent"                  ,"StHeavyTagMaker","StHeavyTagMaker","",kFALSE},
  {"EbyeScaTags" ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"HighPtTags"  ,"","TagsChain","StEVent"              ,"StHighPtTagsMaker","StHighPtTagsMaker","",kFALSE},
  {"PCollTag"    ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  //#ifndef __NoStrangeMuDst__
  //{"tags"        ,"","TagsChain",      "globT,Event,StrangeTags,HeavyTags,PCollTag,HighPtTags"
  // ,                                        "StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  //{"noTags"      ,"","","-tags,-StrangeTags,-HeavyTags,-PCollTag,-HighPtTags","","","Turn Off tags",kFALSE},
  //#else /* __NoStrangeMuDst__ */
  {"tags"        ,"","TagsChain","globT,Event,HeavyTags,HighPtTags"
   ,                                        "StTagsMaker","StTagsMaker","Collect all tags to TTree",kFALSE},
  {"noTags"      ,"","","-tags,-HeavyTags,-HighPtTags"                       ,"","","Turn Off tags",kFALSE},
  //#endif /* __NoStrangeMuDst__ */

  {"MuDSTChain","MuDSTChain","EMCmDST,CMuDST",""                            ,"StMaker","StChain","",kFALSE},

#ifndef __NoStrangeMuDst__
  {"StrngMuDST","","MuDSTDeps","",              "StStrangeMuDstMaker","","Creates Stangeness MuDST",kFALSE},
#else
  {"StrngMuDST","","",""                                                 ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
#endif /* __NoStrangeMuDst__ */

  {"EMCmDST"   ,"","MuDSTChain","MuDst",                "StEmcMicroDstMaker","","Creates EMC MuDST",kFALSE},

#ifndef __NoStrangeMuDst__
  {"CMuDST"    ,"","MuDSTChain","MuDst,StrngMuDST,Tree",    "StMuDstMaker","","Writes Common MuDST",kFALSE},
#else /* ! __NoStrangeMuDst__ */
  {"CMuDST"    ,"","MuDSTChain","MuDst,Tree",               "StMuDstMaker","","Writes Common MuDST",kFALSE},
#endif /* __NoStrangeMuDst__ */
  {"RMuDST"    ,"","","CMuDST"   ,"","","reads Common MuDST, do not disactivate if no output files",kFALSE},

  {"picoWrite" ,"","PicoChain","picoDst","StPicoDstMaker",""               ,"Writes picoDST format",kFALSE},
  {"picoRead"  ,"","PicoChain","picoDst","StPicoDstMaker",""           ,"WritesRead picoDST format",kFALSE},
  {"PicoVtxDefault" ,"","",""                                       ,"" ,"","pico Vtx default mode",kFALSE},
  {"PicoVtxVpd"     ,"","","-PicoVtxDefault"             ,"" ,"","pico Vtx cut on Tof and VPD mode",kFALSE},
  {"PicoCovMtxSkip" ,"","",""       ,"" ,"","Do not write covariance matrices to picoDst (default)",kFALSE},
  {"PicoCovMtxWrite","","","-PicoCovMtxSkip"   ,"" ,"","Write track covariance matrices to picoDst",kFALSE},
  {"PicoBEmcSmdSkip" ,"","",""                     ,"" ,"","Do not write BSMD to picoDst (default)",kFALSE},
  {"PicoBEmcSmdWrite","","","-PicoBEmcSmdSkip"                      ,"" ,"","Write BSMD to picoDst",kFALSE},


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
  {"McTpcAna"    ,"","McAnaChain","tpcDb,dbutil,McEvent,StEvent"
   ,                                               "StTpcMcAnalysisMaker","StTpcMcAnalysisMaker","",kFALSE},
  {"MiniMcEvent" ,"","","","",                   "StMiniMcEvent","Loads StMiniMcEvent library only",kFALSE},
  {"MiniMcMk"    ,"","","McAss,MiniMcEvent","StMiniMcMaker","StMiniMcMaker"
   ,                                                             "Creates tree in minimc.root file",kFALSE},
  {"SvtMatTree","","","","SvtMatchedTree"
   ,                              "StSvtPoolEventT,StSvtPoolSvtMatchedTree","Create SvtMatchedTree",kFALSE},
  {"LAna"        ,"","","in,detDb,StEvent,tpcDb","StLaserAnalysisMaker"
   ,                                                   "StLaserAnalysisMaker","Laser data Analysis",kFALSE},
  {"EandBDir","","","in,StEvent,TpcHitMover,nodefault"
   ,   "StEandBDirMaker","MathMore,Spectrum,StEandBDirMaker",                   "get E&B direction",kFALSE},
  {"SpinTag"     ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppLPfind1"   ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"SpinSortA"   ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppLPprojectA","","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppDAQfilter1","","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"ppLPeval1"   ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"QA"          ,"","",""                                               ,"","",STAR_CHAIN_OBSOLETE,kFALSE},
  {"EventQA","EventQA","","QUtils,Event","StEventQAMaker"   ,"St_QA_Maker","Filling Y2/Y3 Qa histo",kFALSE},
  {"QAC"         ,"CosmicsQA","globT",""                    ,"StQACosmicMaker","StQACosmicMaker","",kFALSE},
  {"QAalltrigs"  ,"", "","",                                     "","","Analyze all triggers in QA",kFALSE},
  {"HitFilt"     ,"", "","",               "StHitFilterMaker","StHitFilterMaker","Hit filter Maker",kFALSE},
  {"SvtHitFilt"  ,"", "","HitFilt",                                    "","","SVT Hit filter Maker",kFALSE},
  {"TpcHitFilt"  ,"", "","HitFilt",                       "","","filter out TPC Hits not on tracks",kFALSE},
  {"HftHitFilt"  ,"", "","HitFilt",                                 "","","filter out non-HFT hits",kFALSE},
  {"KeepTpcHit"  ,"", "","HitFilt",                   "","","Keep all TPC hits in StHitFilterMaker",kFALSE},
  {"KeepSvtHit"  ,"", "","HitFilt",                   "","","Keep all SVT hits in StHitFilterMaker",kFALSE},
  {"KeepFgtHit"  ,"", "","HitFilt",                   "","","Keep all FGT hits in StHitFilterMaker",kFALSE},
  {"Tree"        ,"OutTree","","","StTreeMaker","StTreeMaker","Write requested branches into files",kFALSE},
  {"logger"      ,""  ,"",""            ,"","","Use log4cxx package to manage the program messages",kFALSE},
  {"NoSimuDb"    ,""  ,"",""                                 ,"","","Switch off Simu Option for DB",kFALSE},
  {"SimuDb"      ,""  ,"","-NoSimuDb"                         ,"","","Switch on Simu Option for DB",kFALSE},
  {"NoOutput"    ,""  ,"","-Tree,-EvOut,noTags"                 ,"","","Suppress Output root files",kFALSE},
  {"EvOutOnly"   ,""  ,"","EvOut,Tree,noTags"                        ,"","","Only event.root files",kFALSE},
  {"NoDefault"   ,""  ,"",""                                  ,"","","No Default consistency check",kFALSE},
  {"Notpc_daq"   ,""  ,"","-tpc_daq"                                            ,"","","No tpc_daq",kFALSE},
  {"analysis"    ,"","","StEvent"        ,"StAnalysisMaker","StAnalysisMaker","Example of Analysis",kFALSE},
  {"NewTpcAlignment","","",""                           ,"","","Switch on new Tpc Alignment schema",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
  {"Aliased     ","time stamps","-----------","------------------------------------------","","","",kFALSE},
  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE}
};
#endif /* __BigFullChain_h__ */
