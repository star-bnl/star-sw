/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
	rft.idl

	Module: rft
	description: Fast tracking interface for FTPC

 */
C   r_ftpc.inc
#ifndef R_FTPC_INC
#define R_FTPC_INC
#endif ! R_FTPC_INC
#include "PAM.inc"
#include "r_ftpc_ctrl.inc"
#include "fcl_fppoint.inc"
#include "tpt_track.inc"
      INTEGER*4 R_FTPC_RANK
      PARAMETER (R_FTPC_RANK=3)
      RECORD      /TABLE_HEAD_ST/                  ctrl_h
      RECORD     /R_FTPC_CTRL_ST/                 ctrl(*)
      RECORD      /TABLE_HEAD_ST/                   hit_h
      RECORD     /FCL_FPPOINT_ST/                  hit(*)
      RECORD      /TABLE_HEAD_ST/                 track_h
      RECORD       /TPT_TRACK_ST/                track(*)
