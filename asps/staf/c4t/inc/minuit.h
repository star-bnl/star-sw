/*
 *  Interface to minuit using cfortran.h
 *
 *  Edit history:
 *  G.Folger  12-Dec-94  change some to use ROUTINE for passing fcn/futil
 */

/*------------------------------------------------------------------
fortran filename   : minuit.f
------------------------------------------------------------------*/

#define MINUIT(A1,A2)  CCALLSFSUB2(MINUIT,minuit,ROUTINE,ROUTINE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnamin.f
------------------------------------------------------------------*/

#define MNAMIN(A1,A2)  CCALLSFSUB2(MNAMIN,mnamin,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnbins.f
------------------------------------------------------------------*/

#define MNBINS(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNBINS,mnbins,DOUBLE,DOUBLE,INT,PDOUBLE,PDOUBLE,PINT,PDOUBLE,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mncalf.f
------------------------------------------------------------------*/

#define MNCALF(A1,A2,A3,A4)  CCALLSFSUB4(MNCALF,mncalf,DOUBLE,DOUBLEV,PDOUBLE,DOUBLE,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : mncler.f
------------------------------------------------------------------*/

#define MNCLER() CCALLSFSUB0(MNCLER,mncler)

/*------------------------------------------------------------------
fortran filename   : mncntr.f
------------------------------------------------------------------*/

#define MNCNTR(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNCNTR,mncntr,DOUBLE,INT,INT,PINT,DOUBLE,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mncomd.f
------------------------------------------------------------------*/
/*
#define mncomd_ELEMS_2          ZTRINGV_NUM(1)
#define mncomd_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define MNCOMD(A1,A2,A3,A4)  CCALLSFSUB4(MNCOMD,mncomd,DOUBLE,STRING,PINT,DOUBLE,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : mncont.f
------------------------------------------------------------------*/

#define MNCONT(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(MNCONT,mncont,ROUTINE,INT,INT,INT,PDOUBLE,PDOUBLE,PINT,ROUTINE,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : mncrck.f
------------------------------------------------------------------*/
/*
#define mncrck_ELEMS_1          ZTRINGV_NUM(1)
#define mncrck_ELEMLEN_1        ZTRINGV_NUM(255)
#define mncrck_ELEMS_3          ZTRINGV_NUM(1)
#define mncrck_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define MNCRCK(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(MNCRCK,mncrck,STRING,INT,PSTRING,PINT,INT,PDOUBLE,PINT,PINT,INT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : mncros.f
------------------------------------------------------------------*/

#define MNCROS(A1,A2,A3,A4)  CCALLSFSUB4(MNCROS,mncros,DOUBLE,PDOUBLE,PINT,DOUBLE,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : mncuve.f
------------------------------------------------------------------*/

#define MNCUVE(A1,A2)  CCALLSFSUB2(MNCUVE,mncuve,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnderi.f
------------------------------------------------------------------*/

#define MNDERI(A1,A2)  CCALLSFSUB2(MNDERI,mnderi,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mndxdi.f
------------------------------------------------------------------*/

#define MNDXDI(A1,A2,A3)  CCALLSFSUB3(MNDXDI,mndxdi,DOUBLE,INT,PDOUBLE,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : mneig.f
------------------------------------------------------------------*/

#define MNEIG(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNEIG,mneig,PDOUBLE,INT,INT,INT,PDOUBLE,DOUBLE,PINT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mnemat.f
------------------------------------------------------------------*/

#define MNEMAT(A1,A2)  CCALLSFSUB2(MNEMAT,mnemat,PDOUBLE,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnerrs.f
------------------------------------------------------------------*/

#define MNERRS(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNERRS,mnerrs,INT,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mneval.f
------------------------------------------------------------------*/

#define MNEVAL(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNEVAL,mneval,DOUBLE,DOUBLE,PDOUBLE,PINT,DOUBLE,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mnexcm.f
------------------------------------------------------------------*/
/*
#define mnexcm_ELEMS_2          ZTRINGV_NUM(1)
#define mnexcm_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define MNEXCM(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(MNEXCM,mnexcm,ROUTINE,STRING,DOUBLEV,INT,PINT,ROUTINE,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : mnexin.f
------------------------------------------------------------------*/

#define MNEXIN(A1)  CCALLSFSUB1(MNEXIN,mnexin,PDOUBLE,A1)

/*------------------------------------------------------------------
fortran filename   : mnfixp.f
------------------------------------------------------------------*/

#define MNFIXP(A1,A2)  CCALLSFSUB2(MNFIXP,mnfixp,INT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnfree.f
------------------------------------------------------------------*/

#define MNFREE(A1)  CCALLSFSUB1(MNFREE,mnfree,INT,A1)

/*------------------------------------------------------------------
fortran filename   : mngrad.f
------------------------------------------------------------------*/

#define MNGRAD(A1,A2)  CCALLSFSUB2(MNGRAD,mngrad,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnhelp.f
------------------------------------------------------------------*/
/*
#define mnhelp_ELEMS_1          ZTRINGV_NUM(1)
#define mnhelp_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define MNHELP(A1,A2)  CCALLSFSUB2(MNHELP,mnhelp,STRING,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnhes1.f
------------------------------------------------------------------*/

#define MNHES1(A1,A2)  CCALLSFSUB2(MNHES1,mnhes1,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnhess.f
------------------------------------------------------------------*/

#define MNHESS(A1,A2)  CCALLSFSUB2(MNHESS,mnhess,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnimpr.f
------------------------------------------------------------------*/

#define MNIMPR(A1,A2)  CCALLSFSUB2(MNIMPR,mnimpr,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mninex.f
------------------------------------------------------------------*/

#define MNINEX(A1)  CCALLSFSUB1(MNINEX,mninex,DOUBLEV,A1)

/*------------------------------------------------------------------
fortran filename   : mninit.f
------------------------------------------------------------------*/

#define MNINIT(A1,A2,A3)  CCALLSFSUB3(MNINIT,mninit,INT,INT,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : mninpu.f
------------------------------------------------------------------*/

#define MNINPU(A1,A2)  CCALLSFSUB2(MNINPU,mninpu,INT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnintr.f
------------------------------------------------------------------*/

#define MNINTR(A1,A2)  CCALLSFSUB2(MNINTR,mnintr,ROUTINE,ROUTINE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnlims.f
------------------------------------------------------------------*/

#define MNLIMS(A1,A2)  CCALLSFSUB2(MNLIMS,mnlims,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnline.f
------------------------------------------------------------------*/

#define MNLINE(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNLINE,mnline,DOUBLE,DOUBLEV,DOUBLE,DOUBLEV,DOUBLE,DOUBLE,DOUBLE,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mnmatu.f
------------------------------------------------------------------*/

#define MNMATU(A1)  CCALLSFSUB1(MNMATU,mnmatu,INT,A1)

/*------------------------------------------------------------------
fortran filename   : mnmigr.f
------------------------------------------------------------------*/

#define MNMIGR(A1,A2)  CCALLSFSUB2(MNMIGR,mnmigr,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnmnos.f
------------------------------------------------------------------*/

#define MNMNOS(A1,A2)  CCALLSFSUB2(MNMNOS,mnmnos,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnmnot.f
------------------------------------------------------------------*/

#define MNMNOT(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(MNMNOT,mnmnot,DOUBLE,INT,INT,DOUBLE,DOUBLE,DOUBLE,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : mnparm.f
------------------------------------------------------------------*/
/*
#define mnparm_ELEMS_2          ZTRINGV_NUM(1)
#define mnparm_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define MNPARM(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNPARM,mnparm,INT,STRING,DOUBLE,DOUBLE,PDOUBLE,PDOUBLE,PINT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mnpars.f
------------------------------------------------------------------*/

#define MNPARS(A1,A2)  CCALLSFSUB2(MNPARS,mnpars,BYTE,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnpfit.f
------------------------------------------------------------------*/

#define MNPFIT(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNPFIT,mnpfit,DOUBLEV,DOUBLEV,INT,PDOUBLE,PDOUBLE,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mnpint.f
------------------------------------------------------------------*/

#define MNPINT(A1,A2,A3)  CCALLSFSUB3(MNPINT,mnpint,PDOUBLE,INT,PDOUBLE,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : mnplot.f
------------------------------------------------------------------*/
/*
#define mnplot_ELEMS_3          ZTRINGV_NUM(#?ð#)
#define mnplot_ELEMLEN_3        ZTRINGV_NUM(1)
*/

#define MNPLOT(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNPLOT,mnplot,PDOUBLE,PDOUBLE,PSTRINGV,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mnpout.f
------------------------------------------------------------------*/
/*
#define mnpout_ELEMS_2          ZTRINGV_NUM(1)
#define mnpout_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define MNPOUT(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(MNPOUT,mnpout,INT,PSTRING,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PINT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : mnprin.f
------------------------------------------------------------------*/

#define MNPRIN(A1,A2)  CCALLSFSUB2(MNPRIN,mnprin,INT,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnpsdf.f
------------------------------------------------------------------*/

#define MNPSDF() CCALLSFSUB0(MNPSDF,mnpsdf)

/*------------------------------------------------------------------
fortran filename   : mnrazz.f
------------------------------------------------------------------*/

#define MNRAZZ(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNRAZZ,mnrazz,DOUBLE,DOUBLEV,PDOUBLE,PINT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mnread.f
------------------------------------------------------------------*/

#define MNREAD(A1,A2,A3,A4)  CCALLSFSUB4(MNREAD,mnread,DOUBLE,INT,PINT,DOUBLE,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : mnrn15.f
------------------------------------------------------------------*/

#define MNRN15(A1,A2)  CCALLSFSUB2(MNRN15,mnrn15,PDOUBLE,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnrset.f
------------------------------------------------------------------*/

#define MNRSET(A1)  CCALLSFSUB1(MNRSET,mnrset,INT,A1)

/*------------------------------------------------------------------
fortran filename   : mnsave.f
------------------------------------------------------------------*/

#define MNSAVE() CCALLSFSUB0(MNSAVE,mnsave)

/*------------------------------------------------------------------
fortran filename   : mnscan.f
------------------------------------------------------------------*/

#define MNSCAN(A1,A2)  CCALLSFSUB2(MNSCAN,mnscan,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnseek.f
------------------------------------------------------------------*/

#define MNSEEK(A1,A2)  CCALLSFSUB2(MNSEEK,mnseek,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnset.f
------------------------------------------------------------------*/

#define MNSET(A1,A2)  CCALLSFSUB2(MNSET,mnset,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnseti.f
------------------------------------------------------------------*/
/*
#define mnseti_ELEMS_1          ZTRINGV_NUM(1)
#define mnseti_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define MNSETI(A1)  CCALLSFSUB1(MNSETI,mnseti,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : mnsimp.f
------------------------------------------------------------------*/

#define MNSIMP(A1,A2)  CCALLSFSUB2(MNSIMP,mnsimp,DOUBLE,DOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnstat.f
------------------------------------------------------------------*/

#define MNSTAT(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(MNSTAT,mnstat,PDOUBLE,PDOUBLE,PDOUBLE,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : mnstin.f
------------------------------------------------------------------*/

#define MNSTIN(A1,A2)  CCALLSFSUB2(MNSTIN,mnstin,BYTE,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mntiny.f
------------------------------------------------------------------*/

#define MNTINY(A1,A2)  CCALLSFSUB2(MNTINY,mntiny,DOUBLE,PDOUBLE,A1,A2)

/*------------------------------------------------------------------
fortran filename   : mnunpt.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(LOGICAL,MNUNPT,mnunpt,BYTE)
#define MNUNPT(A2)  CCALLSFFUN1(MNUNPT,mnunpt,BYTE,A2)

/*------------------------------------------------------------------
fortran filename   : mnvert.f
------------------------------------------------------------------*/

#define MNVERT(A1,A2,A3,A4,A5)  CCALLSFSUB5(MNVERT,mnvert,PDOUBLE,INT,INT,INT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : mnwarn.f
------------------------------------------------------------------*/

#define MNWARN(A1,A2,A3)  CCALLSFSUB3(MNWARN,mnwarn,BYTE,BYTE,BYTE,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : mnwerr.f
------------------------------------------------------------------*/

#define MNWERR() CCALLSFSUB0(MNWERR,mnwerr)

/*------------------------------------------------------------------
fortran filename   : stand.f
------------------------------------------------------------------*/

#define STAND() CCALLSFSUB0(STAND,stand)

