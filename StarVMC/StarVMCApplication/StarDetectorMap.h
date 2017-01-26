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
  {kSsdId     		 ,"ssd","sfsd","SISD","g2t_ssd_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSsdId     		 ,"ssd","SFSD","SISD","g2t_ssd_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSvtId                ,"svt","svtd","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kSvtId                ,"svt","SVTD","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kTpcId     		 ,"tpc","tpad","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
  {kTpcId     		 ,"tpc","TPAD","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"}
};
static const Int_t NoDetectors = sizeof(Detectors)/sizeof(StarDetectorMap_t);

#endif
