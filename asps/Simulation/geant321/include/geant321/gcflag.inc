#ifndef CERNLIB_GEANT321_GCFLAG_INC
#define CERNLIB_GEANT321_GCFLAG_INC
# if defined (__cplusplus) || defined (__STDC__)  /* C++ or ANSI C */
#define gcflag gcflag_
extern struct
{
  int IDEBUG;
  int IDEMIN;
  int IDEMAX;
  int ITEST;
  int IDRUN;
  int IDEVT;
  int IEORUN;
  int IEOTRI;
  int IEVENT;
  int ISWIT[10];
  int IFINIT[20];
  int NEVENT;
  int NRNDM[2];
} gcflag;

#else
*
*
* gcflag.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtflag.inc"
#endif
#endif
*
* gcflag.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcflag
#include "geant321/incend.inc"
#endif

#endif
#endif
