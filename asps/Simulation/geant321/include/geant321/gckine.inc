#ifndef CERNLIB_GEANT321_GCKINE_INC
#define CERNLIB_GEANT321_GCKINE_INC
# if defined (__cplusplus) || defined (__STDC__)  /* C++ or ANSI C */
#define gckine gckine_
extern struct
{
  int ikine;
  float pkine[10];
  int itra;
  int istak;
  int ivert;
  int ipart;
  int itrtyp;
  int napart[5];
  float amass;
  float charge;
  float tlife;
  float vert[3];
  float pvert[4];
  int ipaold;
} gckine;

#else
*
*
* gckine.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtkine.inc"
#endif
#endif
*
* gckine.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gckine
#include "geant321/incend.inc"
#endif

#endif
#endif
