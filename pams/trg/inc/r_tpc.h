/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
	r_tpc.idl

	Module: r_tpc
	description: Fast tracking interface

 */
/* r_tpc.h */
#ifndef R_TPC_H
#define R_TPC_H
/*----------------------------------------------- INCLUDES   --*/
#include "PAM.h"
#include "tcl_tphit.h"
#include "tpt_track.h"
/*----------------------------------------------- MACROS     --*/
#define R_TPC_RANK 2
/*----------------------------------------------- TYPEDEFS   --*/
typedef STAFCV_T (*R_TPC_FT)
(
  TABLE_HEAD_ST            *hit_h,     TCL_TPHIT_ST    *hit_d ,
  TABLE_HEAD_ST          *track_h,     TPT_TRACK_ST    *track_d 
);
/*----------------------------------------------- PROTOTYPES --*/
extern CC_P STAFCV_T r_tpc_ (
  TABLE_HEAD_ST            *hit_h,     TCL_TPHIT_ST    *hit_d ,
  TABLE_HEAD_ST          *track_h,     TPT_TRACK_ST    *track_d 
);
#ifdef __cplusplus
extern CC_P STAFCV_T r_tpc_load_ami(amiBroker *broker);
#endif /* __cplusplus */
#endif /* R_TPC_H */
