C .inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C      
C   
C   l3totphit.inc
#ifndef L3TOTPHIT_INC
#define L3TOTPHIT_INC
#endif
#include "PAM.inc"
#include "tcl_tphit.inc"
#include "hitarray.inc"
      INTEGER*4 L3TOTPHIT_RANK
      PARAMETER (L3TOTPHIT_RANK=2)
      RECORD      /TABLE_HEAD_ST/              hitarray_h
      RECORD        /HITARRAY_ST/             hitarray(*)
      RECORD      /TABLE_HEAD_ST/                 tpHit_h
      RECORD       /TCL_TPHIT_ST/                tpHit(*)
