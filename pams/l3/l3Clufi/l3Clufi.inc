C .inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C   l3Clufi.inc
#ifndef L3CLUFI_INC
#define L3CLUFI_INC
#endif
#include "PAM.inc"
#include "hitarray.inc"
#include "pixelarray.inc"
      INTEGER*4 L3CLUFI_RANK
      PARAMETER (L3CLUFI_RANK=2)
      RECORD      /TABLE_HEAD_ST/                pixels_h
      RECORD      /PIXELARRAY_ST/               pixels(*)
      RECORD      /TABLE_HEAD_ST/                  hits_h
      RECORD        /HITARRAY_ST/                 hits(*)
