/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 sgr.idl
      Module: sgr
      description: PURPOSE OF FUNCTION: Make SVT groups that are 
                      topologically consistent with tracks.                                         Requires svt control to get Vertex position.
                      Requires SVT hits.
                      Create Groups table.

 */
C   sgr.inc
#ifndef SGR_INC
#define SGR_INC
#endif ! SGR_INC
#include "PAM.inc"
#include "sgr_config.inc"
#include "sgr_pixmap.inc"
#include "stk_vtx.inc"
#include "svg_geom.inc"
#include "scs_spt.inc"
#include "stk_ctrack.inc"
      INTEGER*4 SGR_RANK
      PARAMETER (SGR_RANK=7)
      RECORD      /TABLE_HEAD_ST/                     v_h
      RECORD         /STK_VTX_ST/                    v(*)
      RECORD      /TABLE_HEAD_ST/              svt_geom_h
      RECORD        /SVG_GEOM_ST/             svt_geom(*)
      RECORD      /TABLE_HEAD_ST/              svt_hits_h
      RECORD         /SCS_SPT_ST/             svt_hits(*)
      RECORD      /TABLE_HEAD_ST/                groups_h
      RECORD      /SGR_CONFIG_ST/               groups(*)
      RECORD      /TABLE_HEAD_ST/              pix_info_h
      RECORD      /SGR_PIXMAP_ST/             pix_info(*)
      RECORD      /TABLE_HEAD_ST/      candidate_groups_h
      RECORD      /SGR_CONFIG_ST/     candidate_groups(*)
      RECORD      /TABLE_HEAD_ST/            stk_ctrack_h
      RECORD      /STK_CTRACK_ST/           stk_ctrack(*)
