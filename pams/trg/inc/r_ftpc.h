/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
	rft.idl

	Module: rft
	description: Fast tracking interface for FTPC

 */
/* r_ftpc.h */
#ifndef R_FTPC_H
#define R_FTPC_H
/*----------------------------------------------- INCLUDES   --*/
#include "PAM.h"
#include "r_ftpc_ctrl.h"
#include "fcl_fppoint.h"
#include "tpt_track.h"
/*----------------------------------------------- MACROS     --*/
#define R_FTPC_RANK 3
/*----------------------------------------------- TYPEDEFS   --*/
typedef STAFCV_T (*R_FTPC_FT)
(
  TABLE_HEAD_ST           *ctrl_h,   R_FTPC_CTRL_ST    *ctrl_d ,
  TABLE_HEAD_ST            *hit_h,   FCL_FPPOINT_ST    *hit_d ,
  TABLE_HEAD_ST          *track_h,     TPT_TRACK_ST    *track_d 
);
/*----------------------------------------------- PROTOTYPES --*/
extern CC_P STAFCV_T r_ftpc_ (
  TABLE_HEAD_ST           *ctrl_h,   R_FTPC_CTRL_ST    *ctrl_d ,
  TABLE_HEAD_ST            *hit_h,   FCL_FPPOINT_ST    *hit_d ,
  TABLE_HEAD_ST          *track_h,     TPT_TRACK_ST    *track_d 
);
#ifdef __cplusplus
extern CC_P STAFCV_T r_ftpc_load_ami(amiBroker *broker);
#endif /* __cplusplus */
#endif /* R_FTPC_H */
