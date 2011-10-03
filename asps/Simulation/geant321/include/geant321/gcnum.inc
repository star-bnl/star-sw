#ifndef CERNLIB_GEANT321_GCNUM_INC
#define CERNLIB_GEANT321_GCNUM_INC
# if defined (__cplusplus) || defined (__STDC__)  /* C++ or ANSI C */
#define gcnum gcnum_
extern struct
{
  int nmate;
  int nvolum;
  int nrotm;
  int ntmed;
  int ntmult;
  int ntrack;
  int npart;
  int nstmax;
  int nvertx;
  int nhead;
  int nbit;
} gcnum;

#define gcnumx gcnumx_
extern struct
{
  int nalive;
  int ntmsto;
} gcnumx;


#else
*
*
* gcnum.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtnum.inc"
#endif
#endif
*
* gcnum.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcnum
#include "geant321/incend.inc"
#endif

#endif
#endif
