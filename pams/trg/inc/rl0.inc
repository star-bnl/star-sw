/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
        rl0.idl
 
         description: Trigger Level 0 simulator analysis module
 
 */
C   rl0.inc
#ifndef RL0_INC
#define RL0_INC
#endif ! RL0_INC
#include "PAM.inc"
#include "ctg_geo.inc"
#include "mwc_geo.inc"
#include "ctu_raw.inc"
#include "mwc_sector.inc"
#include "mwc_raw.inc"
#include "rl0_ctrl.inc"
#include "rl0_data.inc"
#include "rl0_ctbcal.inc"
#include "rl0_mwccal.inc"
      INTEGER*4 RL0_RANK
      PARAMETER (RL0_RANK=9)
      RECORD      /TABLE_HEAD_ST/               ctb_geo_h
      RECORD         /CTG_GEO_ST/              ctb_geo(*)
      RECORD      /TABLE_HEAD_ST/               mwc_geo_h
      RECORD         /MWC_GEO_ST/              mwc_geo(*)
      RECORD      /TABLE_HEAD_ST/               ctb_raw_h
      RECORD         /CTU_RAW_ST/              ctb_raw(*)
      RECORD      /TABLE_HEAD_ST/            mwc_sector_h
      RECORD      /MWC_SECTOR_ST/           mwc_sector(*)
      RECORD      /TABLE_HEAD_ST/               mwc_raw_h
      RECORD         /MWC_RAW_ST/              mwc_raw(*)
      RECORD      /TABLE_HEAD_ST/                   ctr_h
      RECORD        /RL0_CTRL_ST/                  ctr(*)
      RECORD      /TABLE_HEAD_ST/                    L0_h
      RECORD        /RL0_DATA_ST/                   L0(*)
      RECORD      /TABLE_HEAD_ST/                ctbcal_h
      RECORD      /RL0_CTBCAL_ST/               ctbcal(*)
      RECORD      /TABLE_HEAD_ST/                mwccal_h
      RECORD      /RL0_MWCCAL_ST/               mwccal(*)
