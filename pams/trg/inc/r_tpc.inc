/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
	r_tpc.idl

	Module: r_tpc
	description: Fast tracking interface

 */
C   r_tpc.inc
#ifndef R_TPC_INC
#define R_TPC_INC
#endif ! R_TPC_INC
#include "PAM.inc"
#include "tcl_tphit.inc"
#include "tpt_track.inc"
      INTEGER*4 R_TPC_RANK
      PARAMETER (R_TPC_RANK=2)
      RECORD      /TABLE_HEAD_ST/                   hit_h
      RECORD       /TCL_TPHIT_ST/                  hit(*)
      RECORD      /TABLE_HEAD_ST/                 track_h
      RECORD       /TPT_TRACK_ST/                track(*)
