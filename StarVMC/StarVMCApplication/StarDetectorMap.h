#ifndef StarDetectorMap_h
#define StarDetectorMap_h
#include "StEnumerations.h"
struct StarDetectorMap_t {
  const StDetectorId Id;
  const Char_t*      Csys;
  const Char_t*      det;
  const Char_t*      set; // empty if use 'simple' numbv => volume_id translation
  const Char_t*      G2T_type;
  const Char_t*      G2T_name;
  const Char_t*      G2T_sys;
  const Char_t*      G2T_geom;
  const Char_t*      Collection; // StEvent hit collection name 
};
/* Instrument block emc => ecal
detG3  detVMC   Csys
BCSB   BCSB     tof
BRSG   BRSG     tof
BXSA   BXSA     tof
BRDT            tof
BPOL   BPOL     bbc
CSDA   CSDA     smd
CSUP   CSUP     emc
EHMS   EHMS     esm
ESCI   ESCI     eem
FGSC   FGZC     fgt
FHMS   FHMS     fpd
FLGR   FLGR     fpd
FPCT   FPCT     fpd
FSCI            fpd
FREO            rich
FSEC   FSEC     ftpc
IBSS   IBSS     ist
PLAC   PLAC     pxl
PDGS   PDGS     phm
QSCI   QSCI     zdc
QUAR            rich  
rCSI            rich
rGAP            rich
SFSD   SFSD     ssd
SVTD   SVTD     svt
TPAD   TPAD     tpc
TMSE   TMSE     tpc
VRAD   VRAD     vpd
GMDI            gemb
=======================
       BBCB     hca
       BBCF     hca
       EPAD     epd
FDSW            fst
FGZC            fgt
FGZD            fgt
FHCS            fhc
FLGF   FLGF     fpd
FLXF   FLXF     fpd
       FPSC     hca,fpd
FSCT   FSCT     fsc
       FTSA     fts
       FZCB     fgt
       HCEL     hca
       HCES     hca
       HSTP     hca
IGAL            igt   
       LEDG     hca
MIGG   MIGG     mtd
MMRP   MMRP     mtd
MXSA   MXSA     mtd
OQUA            rich
       SCIN     eid
       TABS     eid
       TGAP     eid
YPLA            hpd     

zdc  g2t_zdc:"g2t_emc_hit","zdc","VPDD","VRAD"
tpc  g2t_tpc:"g2t_tpc_hit","tpc","TPCE","TPAD"
tfr  g2t_tof:"g2t_ctf_hit","tfr","BTOF","BRSG"
svt  g2t_svt:"g2t_svt_hit","svt","SVTT","SFSD"
svt  g2t_svt:"g2t_svt_hit","svt","SVTT","SVTD"
ssd  g2t_ssd:"g2t_ssd_hit","ssd","SISD","SFSD"
smd  g2t_smd:"g2t_emc_hit","smd","CALB","CSDA"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","FREO"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","QUAR"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","RCSI"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","RGAP"
pmd  g2t_pmd:"g2t_pmd_hit","pmd","PHMD","PDGS"
pix  g2t_pix:"g2t_pix_hit","pix","PIXL","PLAC"
mwc  g2t_mwc:"g2t_mwc_hit","mwc","TPCE","TMSE"
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MTTF"
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MTTT"
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MIGG"
ist  g2t_ist:"g2t_ist_hit","ist","ISTB","IBSS"
igt  g2t_igt:"g2t_igt_hit","igt","IGTD","IGAL"
hpd  g2t_hpd:"g2t_hpd_hit","hpd","HPDT","YPLA"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HSTP"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","LEDG"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","BBCB"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","BBCF"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","FPSC"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HCES"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HCEL"
gem  g2t_gem:"g2t_gem_hit","gem","GEMB","GMDI"
fts  g2t_fts:"g2t_fts_hit","fts","FTSD","FTSA"
ftp  g2t_ftp:"g2t_ftp_hit","ftp","FTPC","FSEC"
fst  g2t_fst:"g2t_fst_hit","fst","FSTD","FDSW"
fsc  g2t_fsc:"g2t_emc_hit","fsc","FSCE","FSCT"
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FPSC"
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FLXF"
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FLGR"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FGZC"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FGZD"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FZCB"
etr  g2t_etr:"g2t_etr_hit","etr","EIDD","TABD"
esm  g2t_esm:"g2t_emc_hit","esm","ECAL","EHMS"
esm  g2t_esm:"g2t_emc_hit","esm","ECAL","EXSE"
epd  g2t_epd:"g2t_epd_hit","epd","EPDM","BPOL"
emc  g2t_emc:"g2t_emc_hit","emc","CALB","CSUP"
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","EPCT"
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","ELGR"
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","ESCI"
ctb  g2t_ctb:"g2t_ctf_hit","ctb","BTOF","BXSA"
bbc  g2t_bbc:"g2t_ctf_hit","bbc","BBCM","BPOL"

================================================================================
bbc  g2t_bbc:"g2t_ctf_hit","bbc","BBCM","BPOL"
bbc  {kUnknownId            ,"bbc","BPOL",""    ,"g2t_ctf_hit","g2t_bbc_hit","BBCM",""         ,"StBbcTriggerDetector"},
ctb  g2t_ctb:"g2t_ctf_hit","ctb","BTOF","BXSA"
ctb  {kCtbId     		 ,"ctb","BXSA","BTOF","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog","StCtbTriggerDetector"},
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","ELGR"
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","EPCT"
eem  g2t_eem:"g2t_emc_hit","eem","ECAL","ESCI"
   SET ECAH  DETECTOR ESCI  NWHI=  1000  NWDI=  1000
          VOLUME EAGA  NBITSD=   1
          VOLUME ECVO  NBITSD=   1
          VOLUME EMOD  NBITSD=   3
          VOLUME ESEC  NBITSD=   2
          VOLUME EMGT  NBITSD=   5
          VOLUME EPER  NBITSD=   3
          VOLUME ETAR  NBITSD=   4
eem  {kEndcapEmcTowerId     ,"eem","ESCI","ECAL","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg",""},
eem  {kUnknownId            ,"eem","ELGR",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""         ,""}, // not found
eem  {kUnknownId            ,"eem","EPCT",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""	    ,""}, // not found
emc  g2t_emc:"g2t_emc_hit","emc","CALB","CSUP"
emc  {kBarrelEmcTowerId     ,"emc","CSUP","CALB","g2t_emc_hit","g2t_emc_hit","CALB",""	    ,"StEmcClusterCollection"}, 
epd  g2t_epd:"g2t_epd_hit","epd","EPDM","BPOL"
esm  g2t_esm:"g2t_emc_hit","esm","ECAL","EHMS"
   SET ECAH  DETECTOR EHMS  NWHI=  1000  NWDI=  1000
          VOLUME EAGA  NBITSD=   1
          VOLUME ESPL  NBITSD=   2
          VOLUME EXSG  NBITSD=   3
          VOLUME EHMS  NBITSD=   9
esm  g2t_esm:"g2t_emc_hit","esm","ECAL","EXSE"
esm  {kEndcapEmcPreShowerId ,"esm","EHMS","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg",""},
esm  {kEndcapEmcPreShowerId ,"esm","EXSE","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL",""         ,""}, // not found
etr  g2t_etr:"g2t_etr_hit","etr","EIDD","TABD"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FGZC"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FGZD"
fgt  g2t_fgt:"g2t_fgt_hit","fgt","FGTD","FZCB"
fgt  {kFgtId                ,"fgt","FGSC",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FLGR"
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FLXF"
fpd  g2t_fpd:"g2t_emc_hit","fpd","FPDM","FPSC"
fpd  {kUnknownId            ,"fpd","FHMS",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
fpd  {kUnknownId            ,"fpd","FLGR",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
fpd  {kUnknownId            ,"fpd","FPCT",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
fpd  {kUnknownId            ,"fpd","FSCI",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
fsc  g2t_fsc:"g2t_emc_hit","fsc","FSCE","FSCT"
fst  g2t_fst:"g2t_fst_hit","fst","FSTD","FDSW"
ftp  g2t_ftp:"g2t_ftp_hit","ftp","FTPC","FSEC"
ftp  {kFtpcWestId           ,"ftp","FSEC","FTPC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg","StFtpcHitCollection"},
fts  g2t_fts:"g2t_fts_hit","fts","FTSD","FTSA"
gem  g2t_gem:"g2t_gem_hit","gem","GEMB","GMDI"
gem  {kUnknownId            ,"gem","GMDI","GEMB","g2t_gem_hit","g2t_gem_hit","GEMD",""         ,""} 
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","BBCB"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","BBCF"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","FPSC"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HCEL"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HCES"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","HSTP"
hca  g2t_hca:"g2t_emc_hit","hca","HCAL","LEDG"
igt  g2t_igt:"g2t_igt_hit","igt","IGTD","IGAL"
ist  g2t_ist:"g2t_ist_hit","ist","ISTB","IBSS"
ist  {kIstId                ,"ist","IBSS",""    ,"g2t_ist_hit","g2t_ist_hit","ISTB",""         ,"StRnDHitCollection"},
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MIGG"
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MTTF"
mtd  g2t_mtd:"g2t_mtd_hit","mtd","MUTD","MTTT"
mwc  g2t_mwc:"g2t_mwc_hit","mwc","TPCE","TMSE"
mwc  {kMwpcWestId		 ,"mwc","TMSE",""    ,"g2t_mwc_hit","g2t_mwc_hit","TPCE",""         ,""},
pix  g2t_pix:"g2t_pix_hit","pix","PIXL","PLAC"
pix  {kPxlId                ,"pix","PLAC",""    ,"g2t_pix_hit","g2t_pix_hit","PIXL",""         ,"StRnDHitCollection"},
pmd  g2t_pmd:"g2t_pmd_hit","pmd","PHMD","PDGS"
pmd  {kPhmdId               ,"pmd","PDGS",""    ,"g2t_pmd_hit","g2t_pmd_hit","PHMD",""	    ,""},
rch  g2t_rch:"g2t_rch_hit","rch","RICH","FREO"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","QUAR"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","RCSI"
rch  g2t_rch:"g2t_rch_hit","rch","RICH","RGAP"
rch  {kUnknownId            ,"rch","FREO",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""         ,""},
rch  {kUnknownId            ,"rch","QUAR",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
rch  {kUnknownId            ,"rch","RCSI",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
rch  {kUnknownId            ,"rch","RGAP",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
smd  g2t_smd:"g2t_emc_hit","smd","CALB","CSDA"
smd  {kBarrelEmcPreShowerId ,"smd","CSDA","CALB","g2t_emc_hit","g2t_smd_hit","CALB",""	    ,""}, 
ssd  g2t_ssd:"g2t_ssd_hit","ssd","SISD","SFSD"
ssd  {kSsdId     		 ,"ssd","sfsd","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
ssd  {kSsdId     		 ,"ssd","SFSD","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
svt  g2t_svt:"g2t_svt_hit","svt","SVTT","SFSD"
svt  g2t_svt:"g2t_svt_hit","svt","SVTT","SVTD"
svt  {kSvtId                ,"svt","svtd","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
svt  {kSvtId                ,"svt","SVTD","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
tfr  g2t_tof:"g2t_ctf_hit","tfr","BTOF","BRSG"
tfr  {kTofId     		 ,"tfr","BRSG","BTOF","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog","StBTofCollection"},
tof  {kTofId     		 ,"tof","BCSB","BTOF","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog","StBTofCollection"},
tpc  g2t_tpc:"g2t_tpc_hit","tpc","TPCE","TPAD"
tpc  {kTpcId     		 ,"tpc","tpad","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
tpc  {kTpcId     		 ,"tpc","TPAD","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
vpd  {kUnknownId            ,"vpd","VRAD","VPDD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg",""},
zdc  g2t_zdc:"g2t_emc_hit","zdc","VPDD","VRAD"
zdc  {kZdcWestId            ,"zdc","QSCI",""    ,"g2t_emc_hit","g2t_zdc_hit","ZCAL",""	    ,""},
================================================================================
// etr  g2t_etr:"g2t_etr_hit","etr","EIDD","TABD" ??
  {kEndcapEmcTowerId     ,"eem","ESCI","ECAL","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg",""}, // is the active scintillator (polystyrene) layer
  {kUnknownId            ,"eem","ELGR",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""         ,""}, // not found
  {kUnknownId            ,"eem","EPCT",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""	    ,""}, // SMD section
  {kEndCapEmcPreShowerId ,"esm","EHMS","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg",""}, // defines the triangular SMD strips
  {kEndCapEmcPreShowerId ,"esm","EXSE","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL",""         ,""}, // SMD section
  {kBarrelEmcTowerId     ,"emc","CSUP","CALB","g2t_emc_hit","g2t_emc_hit","CALB","","StEmcClusterCollection"}, // is a super layer with few layers inside
  {kBarrelEmcPreShowerId ,"smd","CSDA","CALB","g2t_emc_hit","g2t_smd_hit","CALB",""	    ,""}, // Al block with sensitive gas volume
  {kUnknownId            ,"fpd","FLXF",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDM",""         ,""}, // Lead Glass detector
  {kUnknownId            ,"fpd","FPSC",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDM",""         ,""}, // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"fpd","FHMS",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // sHower Max Strip
  {kUnknownId            ,"fpd","FLGR",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // Lead Glass detector
  {kUnknownId            ,"fpd","FPCT",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // Photo Cathode
  {kUnknownId            ,"fpd","FSCI",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // the active scintillator (polystyren) layer
  {kUnknownId            ,"fsc","FSCE",""    ,"g2t_emc_hit","g2t_emc_hit","FSCT",""         ,""}, // Fiber Sampling Calorimeter
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","BBCB",""         ,""}, // Beam Counter back
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","BBCF",""         ,""}, // Beam Counter front
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","FPSC",""         ,""}, // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HCEL",""         ,""}, // 3*3 cells/pixels in each tower, 16*16 fibers
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HCES",""         ,""}, // 3*3 cells/pixels in each tower, 15*15 fibers
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HSTP",""         ,""}, // each stripes in one pre-shower, 20 stripes in one pre shower
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","LEDG",""         ,""}, // Lead glass block
  {kZdcWestId            ,"zdc","QSCI",""    ,"g2t_emc_hit","g2t_zdc_hit","ZCAL",""	    ,""}, // a sensitive Fiber layer
  {kUnknownId            ,"vpd","VRAD","VPDD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg",""}, // light-producing layer
  {kUnknownId            ,"bbc","BPOL",""    ,"g2t_ctf_hit","g2t_bbc_hit","BBCM","","StBbcTriggerDetector"}, // active scintillator layer
  {kCtbId     	         ,"ctb","BXSA","BTOF","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog","StCtbTriggerDetector"}, // the active trigger scintillator SLAB for ctb
  {kTofId     		 ,"tfr","BRSG","BTOF","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog","StBTofCollection"}, // the sensitive gas layer in the TOFr module
  {kTofId     		 ,"tof","BCSB","BTOF","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog","StBTofCollection"}, // the active trigger scintillator SLAB for tof
  {kUnknownId            ,"epd","EPDM","BPOL","g2t_epd_hit","g2t_epd_hit","BPOL",""	    ,""}, // event plane detector mother volume, EPAD ?
  {kFgtId                ,"fgt","FGSC",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kFgtId                ,"fgt","FGZD",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kFgtId                ,"fgt","FZCB",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kUnknownId            ,"fst","FDSW","FSTD","g2t_fst_hit","g2t_fst_hit","FSTD",""         ,""}, // the Silicon Wafer
  {kFtpcWestId           ,"ftp","FSEC","FTPC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg","StFtpcHitCollection"},
  {kUnknownId            ,"fts","FTSA","FTSD","g2t_fts_hit","g2t_fts_hit","FTSD",""         ,""},
  {kUnknownId            ,"gem","GMDI","GEMB","g2t_gem_hit","g2t_gem_hit","GEMD",""         ,""} // GEM strip detector
  {kUnknownId            ,"igt","IGAL","IGTD","g2t_igt_hit","g2t_igt_hit","IGTD",""         ,""} //
  {kIstId                ,"ist","IBSS",""    ,"g2t_ist_hit","g2t_ist_hit","ISTB",""         ,"StRnDHitCollection"},
  {kMtdId                ,"mtd","MIGG","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""} // a single gas gap
  {kMtdId		 ,"mtd","MTTF","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""} // 5-tray group mother
  {kMtdId		 ,"mtd","MTTT","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""} // 3-tray group mother
  {kMwpcWestId		 ,"mwc","TMSE",""    ,"g2t_mwc_hit","g2t_mwc_hit","TPCE",""         ,""}, // single sensitive volume
  {kPxlId                ,"pix","PLAC",""    ,"g2t_pix_hit","g2t_pix_hit","PIXL",""         ,"StRnDHitCollection"},
  {kPhmdId               ,"pmd","PDGS",""    ,"g2t_pmd_hit","g2t_pmd_hit","PHMD",""	    ,""},
  {kUnknownId            ,"rch","FREO",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""         ,""},
  {kUnknownId            ,"rch","QUAR",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RCSI",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RGAP",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kSsdId     		 ,"ssd","sfsd","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSsdId     		 ,"ssd","SFSD","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSvtId                ,"svt","svtd","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kSvtId                ,"svt","SVTD","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kTpcId     		 ,"tpc","tpad","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
  {kTpcId     		 ,"tpc","TPAD","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
================================================================================
HITS: y2016a
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
          USER PARAMETERS
4       HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[146]
   SET TPCH  DETECTOR TPAD  NWHI=  1000  NWDI=  1000
          VOLUME TPGV  NBITSD=   1
          VOLUME TPSS  NBITSD=   4
          VOLUME TPAD  NBITSD=   8

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
6       HALL[1]/CAVE[1]/TpcRefSys[1]/BTOF[1]/BTOH[2]/BSEC[48]/BTRA[1]/BXTR[1]/BRTC[1]/BGMT[1]/GMTS[2]/GSBE[1]/GEMG[1] ?
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
8       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSCI[19]  : CSCI => ETA
   SET CALH  DETECTOR CSUP  NWHI=  1000  NWDI=  1000
          VOLUME CHLV  NBITSD=   1
          VOLUME CPHI  NBITSD=   6
          VOLUME CSUP  NBITSD=   1
          VOLUME ETA   NBITSD=   5

          VOLUME Y     NBITSD=   1
          HIT ELEMENT =BIRK  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.2147E+09
          USER PARAMETERS
9       HALL[1]/CAVE[1]/CALB[1]/CHLV[2]/CPHI[60]/CSUP[2]/CSMD[1]/CSDA[4]/CSME[30]/CSHI[2] : CSME/CSHI => TYPE/ETA/ETSP
   SET CALH  DETECTOR CSDA  NWHI=  1000  NWDI=  1000
          VOLUME CHLV  NBITSD=   1
          VOLUME CPHI  NBITSD=   6
          VOLUME CSDA  NBITSD=   2
          VOLUME TYPE  NBITSD=   2
          VOLUME ETA   NBITSD=   4
          VOLUME ETSP  NBITSD=   7

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
16      HALL[1]/CAVE[1]/FBOX[4]/FTOW[238]/FPCT[1]
   SET FPDH  DETECTOR FLGR  NWHI=  1000  NWDI=  1000
          VOLUME FBOX  NBITSD=   2
          VOLUME FTOW  NBITSD=   8

          HIT ELEMENT =FLGR  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
          HIT ELEMENT =ELOS  NBITSH=  32  ORIG =  0.0000E+00  FACT =  0.8590E+07
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

 */
static const StarDetectorMap_t Detectors[] = {
  {kEndcapEmcTowerId     ,"eem","ESCI","ECAL","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg",""}, // is the active scintillator (polystyrene) layer
  {kUnknownId            ,"eem","ELGR",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""         ,""}, // not found
  {kUnknownId            ,"eem","EPCT",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""	    ,""}, // SMD section
  {kEndcapEmcPreShowerId ,"esm","EHMS","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg",""}, // defines the triangular SMD strips
  {kEndcapEmcPreShowerId ,"esm","EXSE","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL",""         ,""}, // SMD section
  {kBarrelEmcTowerId     ,"emc","CSUP","CALB","g2t_emc_hit","g2t_emc_hit","CALB","","StEmcClusterCollection"}, // is a super layer with few layers inside
  {kBarrelEmcPreShowerId ,"smd","CSDA","CALB","g2t_emc_hit","g2t_smd_hit","CALB",""	    ,""}, // Al block with sensitive gas volume
  {kUnknownId            ,"fpd","FLXF",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDM",""         ,""}, // Lead Glass detector
  {kUnknownId            ,"fpd","FPSC",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDM",""         ,""}, // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"fpd","FHMS",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // sHower Max Strip
  {kUnknownId            ,"fpd","FLGR",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // Lead Glass detector
  {kUnknownId            ,"fpd","FPCT",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // Photo Cathode
  {kUnknownId            ,"fpd","FSCI",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""}, // the active scintillator (polystyren) layer
  {kUnknownId            ,"fsc","FSCE",""    ,"g2t_emc_hit","g2t_emc_hit","FSCT",""         ,""}, // Fiber Sampling Calorimeter
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","BBCB",""         ,""}, // Beam Counter back
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","BBCF",""         ,""}, // Beam Counter front
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","FPSC",""         ,""}, // a piece of scintillator in FMS Preshower
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HCEL",""         ,""}, // 3*3 cells/pixels in each tower, 16*16 fibers
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HCES",""         ,""}, // 3*3 cells/pixels in each tower, 15*15 fibers
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","HSTP",""         ,""}, // each stripes in one pre-shower, 20 stripes in one pre shower
  {kUnknownId            ,"hca","HCAL",""    ,"g2t_emc_hit","g2t_emc_hit","LEDG",""         ,""}, // Lead glass block
  {kZdcWestId            ,"zdc","QSCI",""    ,"g2t_emc_hit","g2t_zdc_hit","ZCAL",""	    ,""}, // a sensitive Fiber layer
  {kUnknownId            ,"vpd","VRAD","VPDD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg",""}, // light-producing layer
  {kUnknownId            ,"bbc","BPOL",""    ,"g2t_ctf_hit","g2t_bbc_hit","BBCM","","StBbcTriggerDetector"}, // active scintillator layer
  {kCtbId     	         ,"ctb","BXSA","BTOF","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog","StCtbTriggerDetector"}, // the active trigger scintillator SLAB for ctb
  {kTofId     		 ,"tfr","BRSG","BTOF","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog","StBTofCollection"}, // the sensitive gas layer in the TOFr module
  {kTofId     		 ,"tof","BCSB","BTOF","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog","StBTofCollection"}, // the active trigger scintillator SLAB for tof
  {kUnknownId            ,"epd","EPDM","BPOL","g2t_epd_hit","g2t_epd_hit","BPOL",""	    ,""}, // event plane detector mother volume, EPAD ?
  {kFgtId                ,"fgt","FGSC",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kFgtId                ,"fgt","FGZD",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kFgtId                ,"fgt","FZCB",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kUnknownId            ,"fst","FDSW","FSTD","g2t_fst_hit","g2t_fst_hit","FSTD",""         ,""}, // the Silicon Wafer
  {kFtpcWestId           ,"ftp","FSEC","FTPC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg","StFtpcHitCollection"},
  {kUnknownId            ,"fts","FTSA","FTSD","g2t_fts_hit","g2t_fts_hit","FTSD",""         ,""},
  {kUnknownId            ,"gem","GMDI","GEMB","g2t_gem_hit","g2t_gem_hit","GEMD",""         ,""}, // GEM strip detector
  {kUnknownId            ,"gmt","GEMG",""    ,"g2t_gem_hit","g2t_gmt_hit","GEMG",""         ,""}, // GEM strip detector
  {kUnknownId            ,"igt","IGAL","IGTD","g2t_igt_hit","g2t_igt_hit","IGTD",""         ,""}, //
  {kIstId                ,"ist","IBSS",""    ,"g2t_ist_hit","g2t_ist_hit","ISTB",""         ,"StRnDHitCollection"},
  {kMtdId                ,"mtd","MIGG","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""},// a single gas gap
  {kMtdId		 ,"mtd","MTTF","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""},// 5-tray group mother
  {kMtdId		 ,"mtd","MTTT","MUTD","g2t_mtd_hit","g2t_mtd_hit","MUTD",""         ,""},// 3-tray group mother
  {kMwpcWestId		 ,"mwc","TMSE",""    ,"g2t_mwc_hit","g2t_mwc_hit","TPCE",""         ,""}, // single sensitive volume
  {kPxlId                ,"pix","PLAC",""    ,"g2t_pix_hit","g2t_pix_hit","PIXL",""         ,"StRnDHitCollection"},
  {kPhmdId               ,"pmd","PDGS",""    ,"g2t_pmd_hit","g2t_pmd_hit","PHMD",""	    ,""},
  {kUnknownId            ,"rch","FREO",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""         ,""},
  {kUnknownId            ,"rch","QUAR",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RCSI",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RGAP",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kSsdId     		 ,"ssd","sfsd","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSsdId     		 ,"ssd","SFSD","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSvtId                ,"svt","svtd","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kSvtId                ,"svt","SVTD","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kTpcId     		 ,"tpc","tpad","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
  {kTpcId     		 ,"tpc","TPAD","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"}
};
static const Int_t NoDetectors = sizeof(Detectors)/sizeof(StarDetectorMap_t);

#endif
