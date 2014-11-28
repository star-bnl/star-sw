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
static const StarDetectorMap_t Detectors[] = {
  {kTofId     		 ,"tof","BCSB","BTOF","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog","StBTofCollection"},
  {kTofId     		 ,"tfr","BRSG","BTOF","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog","StBTofCollection"},
  {kCtbId     		 ,"ctb","BXSA","BTOF","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog","StCtbTriggerDetector"},
  {kUnknownId            ,"bbc","BPOL",""    ,"g2t_ctf_hit","g2t_bbc_hit","BBCM",""         ,"StBbcTriggerDetector"},
#if 1
  {kBarrelEmcPreShowerId ,"smd","CSDA","CALB","g2t_emc_hit","g2t_smd_hit","CALB",""	    ,""}, 
  {kBarrelEmcTowerId     ,"emc","CSUP","CALB","g2t_emc_hit","g2t_emc_hit","CALB",""	    ,"StEmcClusterCollection"}, 
  {kEndcapEmcPreShowerId ,"esm","EHMS","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg",""},
  {kUnknownId            ,"eem","ELGR",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""         ,""},
  {kUnknownId            ,"eem","EPCT",""    ,"g2t_emc_hit","g2t_eem_hit","ECAL",""	    ,""},
  {kEndcapEmcTowerId     ,"eem","ESCI","ECAL","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg",""},
  {kEndcapEmcPreShowerId ,"esm","EXSE","ECAL","g2t_emc_hit","g2t_esm_hit","ECAL",""         ,""},
  {kFgtId                ,"fgt","FGSC",""    ,"g2t_fgt_hit","g2t_fgt_hit","FGTD",""         ,""},
  {kUnknownId            ,"fpd","FHMS",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
  {kUnknownId            ,"fpd","FLGR",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
  {kUnknownId            ,"fpd","FPCT",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
  {kUnknownId            ,"fpd","FSCI",""    ,"g2t_emc_hit","g2t_fpd_hit","FPDH",""         ,""},
  {kUnknownId            ,"rch","FREO",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""         ,""},
#endif
  {kFtpcWestId           ,"ftp","FSEC","FTPC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg","StFtpcHitCollection"},
  {kIstId                ,"ist","IBSS",""    ,"g2t_ist_hit","g2t_ist_hit","ISTB",""         ,"StRnDHitCollection"},
  {kPxlId                ,"pix","PLAC",""    ,"g2t_pix_hit","g2t_pix_hit","PIXL",""         ,"StRnDHitCollection"},
#if 1
  {kPhmdId               ,"pmd","PDGS",""    ,"g2t_pmd_hit","g2t_pmd_hit","PHMD",""	    ,""},
  {kZdcWestId            ,"zdc","QSCI",""    ,"g2t_emc_hit","g2t_zdc_hit","ZCAL",""	    ,""},
  {kUnknownId            ,"rch","QUAR",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RCSI",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kUnknownId            ,"rch","RGAP",""    ,"g2t_rch_hit","g2t_rch_hit","RICH",""	    ,""},
  {kSsdId     		 ,"ssd","SFSD","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSvtId                ,"svt","SVTD","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kTpcId     		 ,"tpc","TPAD","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
  {kMwpcWestId		 ,"mwc","TMSE",""    ,"g2t_mwc_hit","g2t_mwc_hit","TPCE",""         ,""},
#endif
  {kSsdId     		 ,"ssd","sfsd","SISD","g2t_svt_hit","g2t_ssd_hit","SISD",""	    ,"StSsdHitCollection"},
  {kSvtId                ,"svt","svtd","SVTT","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg","StSvtHitCollection"},
  {kTpcId     		 ,"tpc","tpad","TPCE","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg","StTpcHitCollection"},
#if 1
  {kUnknownId            ,"vpd","VRAD","VPDD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg",""},
  {kUnknownId            ,"gem","GMDI","GEMB","g2t_gem_hit","g2t_gem_hit","GEMD",""         ,""} 
#endif
};
static const Int_t NoDetectors = sizeof(Detectors)/sizeof(StarDetectorMap_t);

#endif
