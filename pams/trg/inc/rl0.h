/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
        rl0.idl
 
         description: Trigger Level 0 simulator analysis module
 
 */
/* rl0.h */
#ifndef RL0_H
#define RL0_H
/*----------------------------------------------- INCLUDES   --*/
#include "ctg_geo.h"
#include "mwc_geo.h"
#include "ctu_raw.h"
#include "mwc_sector.h"
#include "mwc_raw.h"
#include "rl0_ctrl.h"
#include "rl0_data.h"
#include "rl0_ctbcal.h"
#include "rl0_mwccal.h"
#include "PAM.h"
/*----------------------------------------------- MACROS     --*/
#define RL0_RANK 9
/*----------------------------------------------- TYPEDEFS   --*/
typedef STAFCV_T (*RL0_FT)
(
  TABLE_HEAD_ST        *ctb_geo_h,       CTG_GEO_ST    *ctb_geo_d ,
  TABLE_HEAD_ST        *mwc_geo_h,       MWC_GEO_ST    *mwc_geo_d ,
  TABLE_HEAD_ST        *ctb_raw_h,       CTU_RAW_ST    *ctb_raw_d ,
  TABLE_HEAD_ST     *mwc_sector_h,    MWC_SECTOR_ST    *mwc_sector_d ,
  TABLE_HEAD_ST        *mwc_raw_h,       MWC_RAW_ST    *mwc_raw_d ,
  TABLE_HEAD_ST            *ctr_h,      RL0_CTRL_ST    *ctr_d ,
  TABLE_HEAD_ST             *L0_h,      RL0_DATA_ST    *L0_d ,
  TABLE_HEAD_ST         *ctbcal_h,    RL0_CTBCAL_ST    *ctbcal_d ,
  TABLE_HEAD_ST         *mwccal_h,    RL0_MWCCAL_ST    *mwccal_d 
);
/*----------------------------------------------- PROTOTYPES --*/
extern CC_P STAFCV_T rl0_ (
  TABLE_HEAD_ST        *ctb_geo_h,       CTG_GEO_ST    *ctb_geo_d ,
  TABLE_HEAD_ST        *mwc_geo_h,       MWC_GEO_ST    *mwc_geo_d ,
  TABLE_HEAD_ST        *ctb_raw_h,       CTU_RAW_ST    *ctb_raw_d ,
  TABLE_HEAD_ST     *mwc_sector_h,    MWC_SECTOR_ST    *mwc_sector_d ,
  TABLE_HEAD_ST        *mwc_raw_h,       MWC_RAW_ST    *mwc_raw_d ,
  TABLE_HEAD_ST            *ctr_h,      RL0_CTRL_ST    *ctr_d ,
  TABLE_HEAD_ST             *L0_h,      RL0_DATA_ST    *L0_d ,
  TABLE_HEAD_ST         *ctbcal_h,    RL0_CTBCAL_ST    *ctbcal_d ,
  TABLE_HEAD_ST         *mwccal_h,    RL0_MWCCAL_ST    *mwccal_d 
);
#ifdef __cplusplus
extern CC_P STAFCV_T rl0_load_ami(amiBroker *broker);
#endif /* __cplusplus */
#endif /* RL0_H */
