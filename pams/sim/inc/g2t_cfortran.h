/*------------------------------------------------------------------
This file contains the necessary definitions for the use of GEANT
calls from C using cfortran; at the end also my own routines.
                                                  dhwi, 7/9/94
pn, 03/04/97:       GFPART call corrected
------------------------------------------------------------------*/
/* Include the main cfortran header file
*/
#include  "cfortran.h"

/* Here comes GEANT
*/
/*
#define gfhits_ELEMS_1          ZTRINGV_NUM(1)
#define gfhits_ELEMLEN_1        ZTRINGV_NUM(4)
#define gfhits_ELEMS_2          ZTRINGV_NUM(1)
#define gfhits_ELEMLEN_2        ZTRINGV_NUM(4)
*/

#ifdef OLD
#define GFHITS(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)  CCALLSFSUB11(GFHITS,gfhits,STRING,STRING,INT,INT,INT,INT,INTV,INTV,INTVV,FLOATVV,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)
#else
#define GFHITS(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)  CCALLSFSUB11(GFHITS,gfhits,STRING,STRING,INT,INT,INT,INT,INTV,INTV,INTV,FLOATV,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)
#endif

#define GFPATH(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(GFPATH,gfpath,INT,INT,INTV,PINT,INTV,INTV,A1,A2,A3,A4,A5,A6)

/*
#define gfin_ELEMS_2          ZTRINGV_NUM(#{>({=È@#)
#define gfin_ELEMLEN_2        ZTRINGV_NUM(4)
#define gfin_ELEMS_5          ZTRINGV_NUM(1)
#define gfin_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define GFIN(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(GFIN,gfin,INT,STRINGV,INT,PINT,STRING,PINT,A1,A2,A3,A4,A5,A6)


#define GFKINE(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(GFKINE,gfkine,INT,FLOATV,FLOATV,PINT,PINT,FLOATV,PINT,A1,A2,A3,A4,A5,A6,A7)


#define GFPART(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(GFPART,gfpart,INT,STRING,PINT,PFLOAT,PFLOAT,PFLOAT,FLOATV,PINT,A1,A2,A3,A4,A5,A6,A7,A8)


#define GFVERT(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(GFVERT,gfvert,INT,FLOATV,PINT,PINT,PFLOAT,FLOATV,PINT,A1,A2,A3,A4,A5,A6,A7)

/*
#define gget_ELEMS_2          ZTRINGV_NUM(1)
#define gget_ELEMLEN_2        ZTRINGV_NUM(4)
*/

#define GGET(A1,A2,A3,A4,A5)  CCALLSFSUB5(GGET,gget,INT,STRINGV,INT,PINT,PINT,A1,A2,A3,A4,A5)


#define GTRIGC() CCALLSFSUB0(GTRIGC,gtrigc)


#define GFHEAD(A1,A2,A3,A4)  CCALLSFSUB4(GFHEAD,gfhead,PINT,INTV,PINT,FLOATV,A1,A2,A3,A4)


#define GLVOLU(A1,A2,A3,A4)  CCALLSFSUB4(GLVOLU,glvolu,INT,INTV,INTV,PINT,A1,A2,A3,A4)

/* ZEBRA */
#define ZVERIF(A1,A2,A3)  CCALLSFSUB3(ZVERIF,zverif,INT,PINT,STRING,A1,A2,A3)


/* GEANT common blocks */
typedef struct 
    {
    int         nmate, nvolum, nrotm, ntmed, ntmult, ntrack, npart,
                mstmax, nvertx, nhead, nbit;
    } GCNUM_DEF ;
#define GCnum COMMON_BLOCK(GCNUM,gcnum)
COMMON_BLOCK_DEF(GCNUM_DEF,GCnum);
GCNUM_DEF GCnum;

typedef struct 
    {
    int         nlevel, names[15], number[15], lvolum[15], lindex[15],
                infrom, nlevmx, nldev[15], linmx[15];
    float       gtran[15][3], grmat[15][10], gonly[15], glx[3];
    } GCVOLU_DEF ;
#define GCvolu COMMON_BLOCK(GCVOLU,gcvolu)
COMMON_BLOCK_DEF(GCVOLU_DEF,GCvolu);
GCVOLU_DEF GCvolu;


/* from here on my own routines
*/

#define gfvert1(A1,A2,A3,A4)  CCALLSFSUB4(GFVERT1,gfvert1,INT,PINT,INTV,PINT,A1,A2,A3,A4)

#define init_geant(A1,A2) CCALLSFSUB2(INIT_GEANT,init_geant,PINT,STRING,A1,A2)

#define exit_geant(A1) CCALLSFSUB1(EXIT_GEANT,exit_geant,INT,A1)

#define find_hits(A1,A2,A3,A4,A5)  CCALLSFSUB5(FIND_HITS,find_hits,STRING,STRING,PINT,PINT,PINT,A1,A2,A3,A4,A5)

