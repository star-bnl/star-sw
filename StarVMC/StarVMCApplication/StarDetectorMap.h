#ifndef StarDetectorMap_h
#define StarDetectorMap_h
/* Instrument block emc => ecal
StRoot/StEmcUtil/geometry/StEmcGeom
  Int_t det=0;
  if     (!strcmp(cdet,"bemc")) {det=1;}
  else if(!strcmp(cdet,"bprs")) {det=2;}
  else if(!strcmp(cdet,"bsmde")){det=3;}
  else if(!strcmp(cdet,"bsmdp")){det=4;}
  else if(!strcmp(cdet,"eemc")) {det=5;}
  else if(!strcmp(cdet,"eprs")) {det=6;}
  else if(!strcmp(cdet,"esmde")){det=7;}
  else if(!strcmp(cdet,"esmdp")){det=8;}
  switch (det){
  case 1:
  case 2:
    mRadius   = 225.405;   // Edge of SC1 (223.5+2* 0.9525)
    mYWidth   =  11.174;   // Was 11.1716 before 18-apr-2000;  
    initBEMCorBPRS();
    break;
  case 3:
    mRadius   = 230.705;   // Was 230.467 before 18-apr-2000;   
    mYWidth   = 11.2014*2;  
    initBSMDE();
    break;
  case 4:
    mRadius   = 232.742;   // Was 232.467 before 18-apr-2000;
    mYWidth   = 22.835;    // From UCLA drawing
    initBSMDP();
    break;
  case 5:
  case 6:
    initEEMCorEPRS();
    break;
  case 7:
    initESMDE();
    break;
  case 8:
    initESMDP();
    break;
  default:
    LOG_FATAL << Form(" StEmcGeom: Bad value of mDetector %i ", mDetector) << endm;
    assert(0);
  }
StEmcGeom::getVolIdBemc(const Int_t ivid, Int_t &module,Int_t &eta,Int_t &sub, Int_t &detector)
    rl     = emcChid[0];  // right/left: =1 for Z>0, and =2 for Z<0
    eta    = emcChid[1];  // pseudorapidity bin number [1,20]
    phi    = emcChid[2];  // module phi [1,120]
    sub    = emcChid[3];  // d(eta)=0.1 tower number [1,2]
    dep    = emcChid[4];  // depth section [1,2];
   10000000*rl + 100000*eta + 100*phi + 10*sub + dep 
    dep = 1 =>   BPRS 
    dep = 2 =>   BEMC
    phi => module, sub
StEmcGeom::getVolIdBsmd(const Int_t ivid, Int_t &module,Int_t &eta,Int_t &sub, Int_t &detector)
    rl     = smdChid[0];  // right/left: =1 for Z>0, and =2 for Z<0
    eta    = smdChid[1];  // pseudorapidity bin number [-10,10]
    phi    = smdChid[2];  // module phi [1,60]
    t      = smdChid[3];  // SMD type 1->3; BSMDE t = 1,2; BSMDP t = 3;
    strip  = smdChid[4];  // strip number 1-75(type 1,2) 1-15(type 3)
    100000000*rl + 1000000*eta + 1000*phi +100*t  + strip 
    phi,t => module, eta, sub

1       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/SFMO[1]/SFLM[20]/SFSW[16]/SFSL[1]/SFSD[1]
   SET SISH  DETECTOR SFSD  NWHI=  1000  NWDI=  1000
          VOLUME SFLM  NBITSD=   5
          VOLUME SFSW  NBITSD=   4
          VOLUME X     NBITSD=  13
          VOLUME Y     NBITSD=   5
          VOLUME Z     NBITSD=  12
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME STEP  NBITSD=  10
          VOLUME SLEN  NBITSD=  16
          VOLUME TOF   NBITSD=  16
          VOLUME PTOT  NBITSD=  16
          VOLUME ELOS  NBITSD=  16
2       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/IBMO[1]/IBAM[24]/IBLM[6]/IBSS[1]
   SET ISTH  DETECTOR IBSS  NWHI=  1000  NWDI=  1000
          VOLUME IBAM  NBITSD=   5
          VOLUME IBLM  NBITSD=   3
          VOLUME Z     NBITSD=  13
          VOLUME Y     NBITSD=   5
          VOLUME X     NBITSD=  12
          VOLUME PTOT  NBITSD=  16
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME SLEN  NBITSD=  16
          VOLUME TOF   NBITSD=  16
          VOLUME STEP  NBITSD=  10
          VOLUME ELOS  NBITSD=  16
3       HALL[1]/CAVE[1]/TpcRefSys[1]/IDSM[1]/PXMO[1]/PXLA[10]/LADR[4]/PXSI[10]/PLAC[1]
   SET PIXH  DETECTOR PLAC  NWHI=  1000  NWDI=  1000
          VOLUME PXLA  NBITSD=   4
          VOLUME LADR  NBITSD=   2
          VOLUME PXSI  NBITSD=   4
          VOLUME Z     NBITSD=  18
          VOLUME Y     NBITSD=   8
          VOLUME X     NBITSD=  18
          VOLUME PTOT  NBITSD=  16
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME SLEN  NBITSD=  16
          VOLUME TOF   NBITSD=  16
          VOLUME STEP  NBITSD=  15
          VOLUME ELOS  NBITSD=  16
4       HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[73]
   SET TPCH  DETECTOR TPAD  NWHI=  1000  NWDI=  1000
          VOLUME TPGV  NBITSD=   1
          VOLUME TPSS  NBITSD=   4
          VOLUME TPAD  NBITSD=   7
          VOLUME Z     NBITSD=  19
          VOLUME Y     NBITSD=  18
          VOLUME X     NBITSD=  12
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME LPTO  NBITSD=  18
          VOLUME SLEN  NBITSD=  13
          VOLUME TOF   NBITSD=  16
          VOLUME LGAM  NBITSD=  16
          VOLUME STEP  NBITSD=  11
          VOLUME USER  NBITSD=  21
5       HALL[1]/CAVE[1]/TpcRefSys[1]/BTOF[1]/BTOH[2]/BSEC[60]/BTRA[1]/BXTR[1]/BRTC[1]/BGMT[1]/BRMD[32]/BRDT[1]/BRSG[6]
   SET BTOH  DETECTOR BRSG  NWHI=  1000  NWDI=  1000
          VOLUME BTOH  NBITSD=   1
          VOLUME BSEC  NBITSD=   6
          VOLUME BRMD  NBITSD=   5
          VOLUME BRSG  NBITSD=   3
          HIT ELEMENT =X     NBITSH=   2  ORIG =  0.1500E-01  FACT =  0.1000E+03
          HIT ELEMENT =Y     NBITSH=  11  ORIG =  0.1000E+02  FACT =  0.1000E+03
          HIT ELEMENT =Z     NBITSH=  10  ORIG =  0.3050E+01  FACT =  0.1000E+03
          HIT ELEMENT =PTOT  NBITSH=  18  ORIG = -0.0000E+00  FACT =  0.2621E+04
          HIT ELEMENT =SLEN  NBITSH=  13  ORIG = -0.0000E+00  FACT =  0.1000E+02
          HIT ELEMENT =TOF   NBITSH=  16  ORIG = -0.0000E+00  FACT =  0.6554E+12
          HIT ELEMENT =STEP  NBITSH=  12  ORIG =  0.2108E-03  FACT =  0.1000E+03
          HIT ELEMENT =ELOS  NBITSH=  16  ORIG = -0.0000E+00  FACT =  0.6554E+11
6       HALL[1]/CAVE[1]/TpcRefSys[1]/BTOF[1]/BTOH[2]/BSEC[48]/BTRA[1]/BXTR[1]/BRTC[1]/BGMT[1]/GMTS[2]/GSBE[1]/GEMG[1]
7       HALL[1]/CAVE[1]/VPDD[2]/VRNG[1]/VDET[19]/VDTI[1]/VCNV[1]/VRAD[1]
   SET VPDH  DETECTOR VRAD  NWHI=  1000  NWDI=  1000
          VOLUME VPDD  NBITSD=   1
          VOLUME VDET  NBITSD=   5
          VOLUME X     NBITSD=   6
          VOLUME Y     NBITSD=   6
          VOLUME Z     NBITSD=   4
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME STEP  NBITSD=   6
          VOLUME SLEN  NBITSD=  13
          VOLUME PTOT  NBITSD=  16
          VOLUME TOF   NBITSD=  18
          VOLUME ELOS  NBITSD=  16

8       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSCI[19]
   SET CALH  DETECTOR CSUP  NWHI=  1000  NWDI=  1000
          VOLUME CHLV  NBITSD=   1
          VOLUME CPHI  NBITSD=   6
          VOLUME CSUP  NBITSD=   1
          VOLUME ETA   NBITSD=   5
          VOLUME Y     NBITSD=   1
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
9       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSMD[1]/CSDA[4]/CSME[30]/CSHI[2]
   SET CALH  DETECTOR CSDA  NWHI=  1000  NWDI=  1000
          VOLUME CHLV  NBITSD=   1
          VOLUME CPHI  NBITSD=   6
          VOLUME CSDA  NBITSD=   2
          VOLUME TYPE  NBITSD=   2
          VOLUME ETA   NBITSD=   4
          VOLUME ETSP  NBITSD=   7
          HIT ELEMENT =ELOS  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+10

10      HALL[1]/CAVE[1]/ECAL[1]/EAGA[2]/EMSS[1]/ECVO[2]/EMOD[6]/ESEC[3]/EMGT[17]/EPER[5]/ETAR[12]/ESCI[1]
   SET ECAH  DETECTOR ESCI  NWHI=  1000  NWDI=  1000
          VOLUME EAGA  NBITSD=   1
          VOLUME ECVO  NBITSD=   1
          VOLUME EMOD  NBITSD=   3
          VOLUME ESEC  NBITSD=   2
          VOLUME EMGT  NBITSD=   5
          VOLUME EPER  NBITSD=   3
          VOLUME ETAR  NBITSD=   4
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
11      HALL[1]/CAVE[1]/ECAL[1]/EAGA[2]/EMSS[1]/ESHM[1]/ESPL[3]/EXSG[6]/EHMS[288]
   SET ECAH  DETECTOR EHMS  NWHI=  1000  NWDI=  1000
          VOLUME EAGA  NBITSD=   1
          VOLUME ESPL  NBITSD=   2
          VOLUME EXSG  NBITSD=   3
          VOLUME EHMS  NBITSD=   9
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09

12      HALL[1]/CAVE[1]/BBCM[2]/BBCA[2]/THXM[6]/SHXT[3]/BPOL[1]
   SET BBCH  DETECTOR BPOL  NWHI=  1000  NWDI=  1000
          VOLUME BBCM  NBITSD=   1
          VOLUME BBCA  NBITSD=   1
          VOLUME THXM  NBITSD=   3
          VOLUME SHXT  NBITSD=   2
          VOLUME TOF   NBITSD=  16
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
13      HALL[1]/CAVE[1]/ZCAL[2]/QCAL[1]/QDIV[260]/QSCI[1]
   SET ZCAH  DETECTOR QSCI  NWHI=  1000  NWDI=  1000
          VOLUME ZCAL  NBITSD=   1
          VOLUME QDIV  NBITSD=   9
          HIT ELEMENT =ELOS  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
14      HALL[1]/CAVE[1]/MUTD[1]/MTTG[28]/MTRA[5]/MIGS[1]/MIGG[5]
   SET MUTH  DETECTOR MIGG  NWHI=  1000  NWDI=  1000
          VOLUME MTTG  NBITSD=   5
          VOLUME MTRA  NBITSD=   3
          VOLUME MIGG  NBITSD=   3
          VOLUME X     NBITSD=   2
          VOLUME Y     NBITSD=  13
          VOLUME Z     NBITSD=  14
          VOLUME PTOT  NBITSD=  18
          VOLUME CX    NBITSD=  10
          VOLUME CY    NBITSD=  10
          VOLUME CZ    NBITSD=  10
          VOLUME SLEN  NBITSD=  13
          VOLUME TOF   NBITSD=  16
          VOLUME STEP  NBITSD=  14
          VOLUME ELOS  NBITSD=  16
15      HALL[1]/CAVE[1]/FBOX[4]/FTOW[238]/FWAL[1]/FLGR[1]
   SET FPDH  DETECTOR FLGR  NWHI=  1000  NWDI=  1000
          VOLUME FBOX  NBITSD=   2
          VOLUME FTOW  NBITSD=   8
          HIT ELEMENT =FLGR  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
          HIT ELEMENT =ELOS  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
16      HALL[1]/CAVE[1]/FBOX[4]/FTOW[238]/FPCT[1]
17      HALL[1]/CAVE[1]/FBOX[2]/FSHM[1]/FHMS[100]
   SET FPDH  DETECTOR FHMS  NWHI=  1000  NWDI=  1000
          VOLUME FBOX  NBITSD=   1
          VOLUME FHMS  NBITSD=   7
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
18      HALL[1]/CAVE[1]/FBOX[4]/FLXF[394]
   SET FPDH  DETECTOR FLXF  NWHI=  1000  NWDI=  1000
          VOLUME FBOX  NBITSD=   2
          VOLUME FLXF  NBITSD=   9
          HIT ELEMENT =FLXF  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
          HIT ELEMENT =ELOS  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
19      HALL[1]/CAVE[1]/FPRS[1]/FPLY[4]/FPSC[80]
   SET FPDH  DETECTOR FPSC  NWHI=  1000  NWDI=  1000
          VOLUME FPLY  NBITSD=   2
          VOLUME FPSC  NBITSD=   7
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
"bbc","BBCM","BPOL","g2t_ctf_hit","g2t_bbc_hit",  // Bbc POLystyren active scintillator layer
"ctb","BTOF","BXSA","g2t_ctf_hit","g2t_ctb_hit",  // the active trigger scintillator SLAB for ctb
"eem","ECAL","ELGR","g2t_emc_hit","g2t_eem_hit",
"eem","ECAL","EPCT","g2t_emc_hit","g2t_eem_hit",  // the active scintillator (polystyren) layer
"eem","ECAL","ESCI","g2t_emc_hit","g2t_eem_hit",  // the active scintillator (polystyren) layer
"emc","CALB","CSUP","g2t_emc_hit","g2t_emc_hit",  // a super layer with few layers inside
"epd","EPDM","EPAD","g2t_epd_hit","g2t_epd_hit",  // scintillator paddle
"esm","ECAL","EHMS","g2t_emc_hit","g2t_esm_hit",  // the triangular SMD strips
"esm","ECAL","EXSE","g2t_emc_hit","g2t_esm_hit",
"etr","EIDD","TABD","g2t_etr_hit","g2t_etr_hit",
"fgt","FGTD","FGZC","g2t_fgt_hit","g2t_fgt_hit",  // active volume divisions of FGSC
"fgt","FGTD","FGZD","g2t_fgt_hit","g2t_fgt_hit",
"fgt","FGTD","FZCB","g2t_fgt_hit","g2t_fgt_hit",
"fpd","FPDM","FLGR","g2t_emc_hit","g2t_fpd_hit",  // Lead Glass detector
"fpd","FPDM","FLXF","g2t_emc_hit","g2t_fpd_hit",  // Lead Glass detector
"fpd","FPDM","FPSC","g2t_emc_hit","g2t_fpd_hit",
"fsc","FSCE","FSCT","g2t_emc_hit","g2t_fsc_hit",  // a sensitive Tungsten+Sci+Epoxy tower
"fst","FSTD","FDSW","g2t_fst_hit","g2t_fst_hit",  // the Silicon Wafer (all active)
"ftp","FTPC","FSEC","g2t_ftp_hit","g2t_ftp_hit",  // a sensitive gas sector
"fts","FTSD","FTSA","g2t_fts_hit","g2t_fts_hit",  // Active volume for each FTS disk
"gem","GEMB","GMDI","g2t_gem_hit","g2t_gem_hit",  // the sensitive volume of the GEM strip detector, layer 1
"hca","HCAL","BBCB","g2t_emc_hit","g2t_hca_hit",  // Beam Counter front, Back scint paddle 
"hca","HCAL","BBCF","g2t_emc_hit","g2t_hca_hit",  // Beam Counter front, Front scint paddle 
"hca","HCAL","FPSC","g2t_emc_hit","g2t_hca_hit",  // a piece of scintillator in FMS Preshower
"hca","HCAL","HCEL","g2t_emc_hit","g2t_hca_hit",  // 3*3 cell in tower, HCES has 16*16 fibers
"hca","HCAL","HCES","g2t_emc_hit","g2t_hca_hit",  // 3*3 cell in tower, HCES has 16*16 fiber
"hca","HCAL","HSTP","g2t_emc_hit","g2t_hca_hit",  // single strips in pre shower, preshower 
"hca","HCAL","LEDG","g2t_emc_hit","g2t_hca_hit",  // Lead glass
"hpd","HPDT","YPLA","g2t_hpd_hit","g2t_hpd_hit",  // the active layer of the pixel
"igt","IGTD","IGAL","g2t_igt_hit","g2t_igt_hit",
"ist","ISTB","IBSS","g2t_ist_hit","g2t_ist_hit",  // the Silicon Sensor
"mtd","MUTD","MIGG","g2t_mtd_hit","g2t_mtd_hit",  // a single gas gap
"mtd","MUTD","MTTF","g2t_mtd_hit","g2t_mtd_hit",  // the MTD11 5-tray group mother
"mtd","MUTD","MTTT","g2t_mtd_hit","g2t_mtd_hit",  // the MTD11 3-tray group mother
"mwc","TPCE","TMSE","g2t_mwc_hit","g2t_mwc_hit",
"pix","PIXL","PLAC","g2t_pix_hit","g2t_pix_hit",
"pmd","PHMD","PDGS","g2t_pmd_hit","g2t_pmd_hit",  // The inner cell in the PMD module
"rch","RICH","FREO","g2t_rch_hit","g2t_rch_hit",
"rch","RICH","QUAR","g2t_rch_hit","g2t_rch_hit",
"rch","RICH","RCSI","g2t_rch_hit","g2t_rch_hit",
"rch","RICH","RGAP","g2t_rch_hit","g2t_rch_hit",
"smd","CALB","CSDA","g2t_emc_hit","g2t_smd_hit",  // Al block with sensitive gas volume
"ssd","SISD","SFSD","g2t_ssd_hit","g2t_ssd_hit",
"svt","SVTT","SFSD","g2t_svt_hit","g2t_ssd_hit",
"svt","SVTT","SVTD","g2t_svt_hit","g2t_svt_hit",
"tfr","BTOF","BRSG","g2t_ctf_hit","g2t_tfr_hit",  // the sensitive gas layer in the TOFr module
"tof","BTOF","BCSB","g2t_ctf_hit","g2t_tof_hit",  // the active trigger scintillator SLAB for tof
"tpc","TPCE","TPAD',"g2t_tpc_hit","g2t_tpc_hit",
"vpd","VPDD","VRAD","g2t_vpd_hit","g2t_vpd_hit",  // light-producing layer (scintillator or quartz)
"zdc","ZCAL","QSCI","g2t_emc_hit","g2t_zdc_hit",  // sensitive Fiber layer


 */
#include "StEnumerations.h"
struct StarDetectorMap_t {
  const StDetectorId Id;
  const Char_t*      Csys;
  const Char_t*      set; // empty if use 'simple' numbv => volume_id translation
  const Char_t*      det;
  const Char_t*      G2T_type;
  const Char_t*      G2T_name;
  const Char_t*      Collection; // StEvent hit collection name 
};
static const StarDetectorMap_t Detectors[] = {
  {kUnknownId            ,"bbc","BBCM","BPOL","g2t_ctf_hit","g2t_bbc_hit","StBbcTriggerDetector"},  // Bbc POLystyren active scintillator layer
  {kCtbId     	         ,"ctb","BTOF","BXSA","g2t_ctf_hit","g2t_ctb_hit","StCtbTriggerDetector"},  // the active trigger scintillator SLAB for ctb
  {kUnknownId            ,"eem","ECAL","ELGR","g2t_emc_hit","g2t_eem_hit",""},                      // not found
  {kUnknownId            ,"eem","ECAL","EPCT","g2t_emc_hit","g2t_eem_hit",""},                      // the active scintillator (polystyren) layer
  {kEndcapEmcTowerId     ,"eem","ECAL","ESCI","g2t_emc_hit","g2t_eem_hit",""},                      // the active scintillator (polystyren) layer
//{kBarrelEmcTowerId     ,"emc","CALB","CSUP","g2t_emc_hit","g2t_emc_hit","StEmcClusterCollection"},// is a super layer with few layers inside 
  {kBarrelEmcTowerId     ,"emc","CALB","CSCI","g2t_emc_hit","g2t_emc_hit","StEmcClusterCollection"},// is a super layer with few layers inside 
//{kBarrelEmcPreShowerId ,"smd","CALB","CSDA","g2t_emc_hit","g2t_smd_hit",""},                      // Al block with sensitive gas volume
  {kBarrelEmcPreShowerId ,"smd","CALB","CSHI","g2t_emc_hit","g2t_smd_hit",""},                      // Al block with sensitive gas volume
  {kUnknownId            ,"epd","EPDM","EPAD","g2t_epd_hit","g2t_epd_hit",""},                      // scintillator paddle
  {kEndcapEmcPreShowerId ,"esm","ECAL","EHMS","g2t_emc_hit","g2t_esm_hit",""},                      // the triangular SMD strips
  {kEndcapEmcPreShowerId ,"esm","ECAL","EXSE","g2t_emc_hit","g2t_esm_hit",""},                      // SMD section
  {kUnknownId            ,"etr","EIDD","TABD","g2t_etr_hit","g2t_etr_hit",""},                      // 
  {kFgtId                ,"fgt","FGTD","FGZC","g2t_fgt_hit","g2t_fgt_hit",""},                      // active volume divisions of FGSC
  {kFgtId                ,"fgt","FGTD","FGZD","g2t_fgt_hit","g2t_fgt_hit",""},                      //
  {kFgtId                ,"fgt","FGTD","FZCB","g2t_fgt_hit","g2t_fgt_hit",""},                      //
  {kUnknownId            ,"fpd","FPDM","FLGR","g2t_emc_hit","g2t_fpd_hit",""},                      // Lead Glass detector			   
  {kUnknownId            ,"fpd","FPDM","FLXF","g2t_emc_hit","g2t_fpd_hit",""},  		    // Lead Glass detector			  
  {kUnknownId            ,"fpd","FPDM","FPSC","g2t_emc_hit","g2t_fpd_hit",""}, 			    // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"fsc","FSCE","FSCT","g2t_emc_hit","g2t_fsc_hit",""},                      // a sensitive Tungsten+Sci+Epoxy tower
  {kUnknownId            ,"fst","FSTD","FDSW","g2t_fst_hit","g2t_fst_hit",""},                      // the Silicon Wafer (all active)
  {kFtpcWestId           ,"ftp","FTPC","FSEC","g2t_ftp_hit","g2t_ftp_hit","StFtpcHitCollection"},   // a sensitive gas sector
  {kUnknownId            ,"fts","FTSD","FTSA","g2t_fts_hit","g2t_fts_hit",""},                      // Active volume for each FTS disk
  {kUnknownId            ,"gem","GEMB","GMDI","g2t_gem_hit","g2t_gem_hit",""},                      // the sensitive volume of the GEM strip detector, layer 1
  {kUnknownId            ,"hca","HCAL","BBCB","g2t_emc_hit","g2t_hca_hit",""},                      // Beam Counter front, Back scint paddle 
  {kUnknownId            ,"hca","HCAL","BBCF","g2t_emc_hit","g2t_hca_hit",""},                      // Beam Counter front, Front scint paddle 
  {kUnknownId            ,"hca","HCAL","FPSC","g2t_emc_hit","g2t_hca_hit",""},                      // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"hca","HCAL","HCEL","g2t_emc_hit","g2t_hca_hit",""},                      // 3*3 cell in tower, HCES has 16*16 fibers
  {kUnknownId            ,"hca","HCAL","HCES","g2t_emc_hit","g2t_hca_hit",""},                      // 3*3 cell in tower, HCES has 16*16 fiber
  {kUnknownId            ,"hca","HCAL","HSTP","g2t_emc_hit","g2t_hca_hit",""},                      // single strips in pre shower, preshower 
  {kUnknownId            ,"hca","HCAL","LEDG","g2t_emc_hit","g2t_hca_hit",""},                      // Lead glass
  {kUnknownId            ,"igt","IGTD","IGAL","g2t_igt_hit","g2t_igt_hit",""},                      //
  {kIstId                ,"ist","ISTB","IBSS","g2t_ist_hit","g2t_ist_hit","StRnDHitCollection"},    // the Silicon Sensor
  {kMtdId                ,"mtd","MUTD","MIGG","g2t_mtd_hit","g2t_mtd_hit",""},                      // a single gas gap
  {kMtdId		 ,"mtd","MUTD","MTTF","g2t_mtd_hit","g2t_mtd_hit",""},                      // the MTD11 5-tray group mother
  {kMtdId		 ,"mtd","MUTD","MTTT","g2t_mtd_hit","g2t_mtd_hit",""},                      // the MTD11 3-tray group mother
  {kMwpcWestId		 ,"mwc","TPCE","TMSE","g2t_mwc_hit","g2t_mwc_hit",""},                      // single sensitive volume
  {kPxlId                ,"pix","PIXL","PLAC","g2t_pix_hit","g2t_pix_hit","StRnDHitCollection"},    //
  {kPhmdId               ,"pmd","PHMD","PDGS","g2t_pmd_hit","g2t_pmd_hit", ""},                     // The inner cell in the PMD module
  {kUnknownId            ,"rch","RICH","FREO","g2t_rch_hit","g2t_rch_hit",""},
  {kUnknownId            ,"rch","RICH","QUAR","g2t_rch_hit","g2t_rch_hit",""},
  {kUnknownId            ,"rch","RICH","RCSI","g2t_rch_hit","g2t_rch_hit",""},
  {kUnknownId            ,"rch","RICH","RGAP","g2t_rch_hit","g2t_rch_hit",""},
  {kSsdId     		 ,"ssd","SISD","sfsd","g2t_ssd_hit","g2t_ssd_hit","StSsdHitCollection"},
  {kSsdId     		 ,"ssd","SISD","SFSD","g2t_ssd_hit","g2t_ssd_hit","StSsdHitCollection"},
  {kSsdId     		 ,"svt","SVTT","SFSD","g2t_svt_hit","g2t_ssd_hit","StSsdHitCollection"},
  {kSvtId                ,"svt","SVTT","svtd","g2t_svt_hit","g2t_svt_hit","StSvtHitCollection"},
  {kSvtId                ,"svt","SVTT","SVTD","g2t_svt_hit","g2t_svt_hit","StSvtHitCollection"},
  {kTofId     		 ,"tfr","BTOF","BRSG","g2t_ctf_hit","g2t_tfr_hit","StBTofCollection"},      // the sensitive gas layer in the TOFr module
  {kTofId     		 ,"tof","BTOF","BCSB","g2t_ctf_hit","g2t_tof_hit","StBTofCollection"},      // the active trigger scintillator SLAB for tof
  {kUnknownId     	 ,"gmt","BTOF","GEMG","g2t_ctf_hit","g2t_gmt_hit",""},                      // the active trigger scintillator SLAB for ctb
  {kTpcId     		 ,"tpc","TPCE","tpad","g2t_tpc_hit","g2t_tpc_hit","StTpcHitCollection"},
  {kTpcId     		 ,"tpc","TPCE","TPAD","g2t_tpc_hit","g2t_tpc_hit","StTpcHitCollection"},
  {kUnknownId            ,"vpd","VPDD","VRAD","g2t_vpd_hit","g2t_vpd_hit",""},                      // light-producing layer (scintillator or quartz) 
  {kZdcWestId            ,"zdc","ZCAL","QSCI","g2t_emc_hit","g2t_zdc_hit",""}                       // sensitive Fiber layer 
};
static const Int_t NoDetectors = sizeof(Detectors)/sizeof(StarDetectorMap_t);
#endif
