/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplabl.f
------------------------------------------------------------------*/
/*
#define hplabl_ELEMS_3          ZTRINGV_NUM(#{7¸{7X#)
#define hplabl_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLABL(A1,A2,A3)  CCALLSFSUB3(HPLABL,hplabl,INT,INT,STRINGV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplaer.f
------------------------------------------------------------------*/
/*
#define hplaer_ELEMS_8          ZTRINGV_NUM(1)
#define hplaer_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define HPLAER(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)  CCALLSFSUB10(HPLAER,hplaer,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,INT,STRING,INT,FLOAT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplarc.f
------------------------------------------------------------------*/

#define HPLARC(A1,A2,A3,A4,A5)  CCALLSFSUB5(HPLARC,hplarc,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplax.f
------------------------------------------------------------------*/
/*
#define hplax_ELEMS_1          ZTRINGV_NUM(1)
#define hplax_ELEMLEN_1        ZTRINGV_NUM(255)
#define hplax_ELEMS_2          ZTRINGV_NUM(1)
#define hplax_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define HPLAX(A1,A2)  CCALLSFSUB2(HPLAX,hplax,STRING,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplax2.f
------------------------------------------------------------------*/

#define HPLAX2(A1)  CCALLSFSUB1(HPLAX2,hplax2,PFLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplaxi.f
------------------------------------------------------------------*/

#define HPLAXI(A1,A2,A3)  CCALLSFSUB3(HPLAXI,hplaxi,FLOATV,FLOATV,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplbox.f
------------------------------------------------------------------*/
/*
#define hplbox_ELEMS_5          ZTRINGV_NUM(1)
#define hplbox_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define HPLBOX(A1,A2,A3,A4,A5)  CCALLSFSUB5(HPLBOX,hplbox,FLOAT,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplc.f
------------------------------------------------------------------*/

#define HPLC(A1)  CCALLSFSUB1(HPLC,hplc,PINT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplcap.f
------------------------------------------------------------------*/

#define HPLCAP(A1)  CCALLSFSUB1(HPLCAP,hplcap,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplcha.f
------------------------------------------------------------------*/

#define HPLCHA(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(HPLCHA,hplcha,INT,FLOAT,FLOAT,PINT,PFLOAT,PFLOAT,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplcol.f
------------------------------------------------------------------*/

#define HPLCOL(A1)  CCALLSFSUB1(HPLCOL,hplcol,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplcom.f
------------------------------------------------------------------*/
/*
#define hplcom_ELEMS_3          ZTRINGV_NUM(1)
#define hplcom_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLCOM(A1,A2,A3)  CCALLSFSUB3(HPLCOM,hplcom,FLOAT,FLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplcon.f
------------------------------------------------------------------*/

#define HPLCON(A1,A2,A3)  CCALLSFSUB3(HPLCON,hplcon,INT,INT,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpldat.f
------------------------------------------------------------------*/

#define HPLDAT() CCALLSFSUB0(HPLDAT,hpldat)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpldes.f
------------------------------------------------------------------*/

#define HPLDES(A1)  CCALLSFSUB1(HPLDES,hpldes,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpldo.f
------------------------------------------------------------------*/

#define HPLDO(A1)  CCALLSFSUB1(HPLDO,hpldo,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplego.f
------------------------------------------------------------------*/

#define HPLEGO(A1,A2,A3)  CCALLSFSUB3(HPLEGO,hplego,INT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplend.f
------------------------------------------------------------------*/

#define HPLEND() CCALLSFSUB0(HPLEND,hplend)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplerr.f
------------------------------------------------------------------*/
/*
#define hplerr_ELEMS_6          ZTRINGV_NUM(1)
#define hplerr_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define HPLERR(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(HPLERR,hplerr,FLOATV,FLOATV,FLOATV,FLOATV,INT,STRING,INT,FLOAT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplf.f
------------------------------------------------------------------*/

#define HPLF(A1)  CCALLSFSUB1(HPLF,hplf,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplfil.f
------------------------------------------------------------------*/

#define HPLFIL() CCALLSFSUB0(HPLFIL,hplfil)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplfit.f
------------------------------------------------------------------*/

#define HPLFIT() CCALLSFSUB0(HPLFIT,hplfit)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplfr3.f
------------------------------------------------------------------*/
/*
#define hplfr3_ELEMS_9          ZTRINGV_NUM(1)
#define hplfr3_ELEMLEN_9        ZTRINGV_NUM(255)
*/

#define HPLFR3(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(HPLFR3,hplfr3,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplfra.f
------------------------------------------------------------------*/
/*
#define hplfra_ELEMS_5          ZTRINGV_NUM(1)
#define hplfra_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define HPLFRA(A1,A2,A3,A4,A5)  CCALLSFSUB5(HPLFRA,hplfra,FLOAT,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplfun.f
------------------------------------------------------------------*/
/*
#define hplfun_ELEMS_4          ZTRINGV_NUM(1)
#define hplfun_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define HPLFUN(A1,A2,A3,A4)  CCALLSFSUB4(HPLFUN,hplfun,FLOATV,FLOATV,INT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplgiv.f
------------------------------------------------------------------*/

#define HPLGIV(A1,A2,A3,A4)  CCALLSFSUB4(HPLGIV,hplgiv,PFLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplglb.f
------------------------------------------------------------------*/
/*
#define hplglb_ELEMS_3          ZTRINGV_NUM(1)
#define hplglb_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLGLB(A1,A2,A3,A4)  CCALLSFSUB4(HPLGLB,hplglb,INT,PINT,PSTRING,PINT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplgtl.f
------------------------------------------------------------------*/

#define HPLGTL() CCALLSFSUB0(HPLGTL,hplgtl)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplgzo.f
------------------------------------------------------------------*/

#define HPLGZO(A1,A2)  CCALLSFSUB2(HPLGZO,hplgzo,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplh.f
------------------------------------------------------------------*/

#define HPLH() CCALLSFSUB0(HPLH,hplh)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplhip.f
------------------------------------------------------------------*/
/*
#define hplhip_ELEMS_3          ZTRINGV_NUM(1)
#define hplhip_ELEMLEN_3        ZTRINGV_NUM(1)
*/

 PROTOCCALLSFFUN2(INT,HPLHIP,hplhip,INT,STRING)
#define HPLHIP(A2,A3)  CCALLSFFUN2(HPLHIP,hplhip,INT,STRING,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplhup.f
------------------------------------------------------------------*/

#define HPLHUP(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(HPLHUP,hplhup,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,LOGICAL,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpli.f
------------------------------------------------------------------*/

#define HPLI() CCALLSFSUB0(HPLI,hpli)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpline.f
------------------------------------------------------------------*/
/*
#define hpline_ELEMS_4          ZTRINGV_NUM(1)
#define hpline_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define HPLINE(A1,A2,A3,A4)  CCALLSFSUB4(HPLINE,hpline,FLOATV,FLOATV,INT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplint.f
------------------------------------------------------------------*/

#define HPLINT(A1)  CCALLSFSUB1(HPLINT,hplint,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplist.f
------------------------------------------------------------------*/

#define HPLIST(A1,A2,A3)  CCALLSFSUB3(HPLIST,hplist,INT,INT,INTV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplive.f
------------------------------------------------------------------*/

#define HPLIVE() CCALLSFSUB0(HPLIVE,hplive)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplj.f
------------------------------------------------------------------*/

#define HPLJ() CCALLSFSUB0(HPLJ,hplj)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplkey.f
------------------------------------------------------------------*/
/*
#define hplkey_ELEMS_4          ZTRINGV_NUM(1)
#define hplkey_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define HPLKEY(A1,A2,A3,A4)  CCALLSFSUB4(HPLKEY,hplkey,FLOAT,FLOAT,INT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplnew.f
------------------------------------------------------------------*/

#define HPLNEW() CCALLSFSUB0(HPLNEW,hplnew)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplnt.f
------------------------------------------------------------------*/

#define HPLNT(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(HPLNT,hplnt,INT,INT,FLOAT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplnul.f
------------------------------------------------------------------*/

#define HPLNUL() CCALLSFSUB0(HPLNUL,hplnul)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplnxt.f
------------------------------------------------------------------*/

#define HPLNXT() CCALLSFSUB0(HPLNXT,hplnxt)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hploc.f
------------------------------------------------------------------*/

#define HPLOC(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(HPLOC,hploc,INT,INT,PFLOAT,PFLOAT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplone.f
------------------------------------------------------------------*/
/*
#define hplone_ELEMS_1          ZTRINGV_NUM(1)
#define hplone_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define HPLONE(A1)  CCALLSFSUB1(HPLONE,hplone,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplopt.f
------------------------------------------------------------------*/
/*
#define hplopt_ELEMS_1          ZTRINGV_NUM(1)
#define hplopt_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define HPLOPT(A1,A2)  CCALLSFSUB2(HPLOPT,hplopt,STRINGV,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplot.f
------------------------------------------------------------------*/
/*
#define hplot_ELEMS_2          ZTRINGV_NUM(1)
#define hplot_ELEMLEN_2        ZTRINGV_NUM(255)
#define hplot_ELEMS_3          ZTRINGV_NUM(1)
#define hplot_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLOT(A1,A2,A3,A4)  CCALLSFSUB4(HPLOT,hplot,INT,STRING,STRING,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplpgn.f
------------------------------------------------------------------*/

#define HPLPGN() CCALLSFSUB0(HPLPGN,hplpgn)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplpro.f
------------------------------------------------------------------*/
/*
#define hplpro_ELEMS_2          ZTRINGV_NUM(1)
#define hplpro_ELEMLEN_2        ZTRINGV_NUM(255)
#define hplpro_ELEMS_3          ZTRINGV_NUM(1)
#define hplpro_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLPRO(A1,A2,A3)  CCALLSFSUB3(HPLPRO,hplpro,INT,STRING,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplpto.f
------------------------------------------------------------------*/
/*
#define hplpto_ELEMS_1          ZTRINGV_NUM(1)
#define hplpto_ELEMLEN_1        ZTRINGV_NUM(255)
#define hplpto_ELEMS_2          ZTRINGV_NUM(1)
#define hplpto_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define HPLPTO(A1,A2)  CCALLSFSUB2(HPLPTO,hplpto,STRING,PSTRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplrdw.f
------------------------------------------------------------------*/

#define HPLRDW(A1,A2,A3,A4)  CCALLSFSUB4(HPLRDW,hplrdw,INT,INT,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplscm.f
------------------------------------------------------------------*/

#define HPLSCM() CCALLSFSUB0(HPLSCM,hplscm)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsel.f
------------------------------------------------------------------*/

#define HPLSEL(A1)  CCALLSFSUB1(HPLSEL,hplsel,INT,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplset.f
------------------------------------------------------------------*/
/*
#define hplset_ELEMS_1          ZTRINGV_NUM(1)
#define hplset_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define HPLSET(A1,A2)  CCALLSFSUB2(HPLSET,hplset,STRING,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsiz.f
------------------------------------------------------------------*/
/*
#define hplsiz_ELEMS_3          ZTRINGV_NUM(1)
#define hplsiz_ELEMLEN_3        ZTRINGV_NUM(1)
*/

#define HPLSIZ(A1,A2,A3)  CCALLSFSUB3(HPLSIZ,hplsiz,PFLOAT,PFLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsof.f
------------------------------------------------------------------*/
/*
#define hplsof_ELEMS_3          ZTRINGV_NUM(1)
#define hplsof_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLSOF(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(HPLSOF,hplsof,FLOAT,FLOAT,STRING,FLOAT,FLOAT,FLOAT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsta.f
------------------------------------------------------------------*/
/*
#define hplsta_ELEMS_2          ZTRINGV_NUM(1)
#define hplsta_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define HPLSTA(A1,A2,A3)  CCALLSFSUB3(HPLSTA,hplsta,INT,STRING,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsur.f
------------------------------------------------------------------*/

#define HPLSUR(A1,A2,A3,A4)  CCALLSFSUB4(HPLSUR,hplsur,INT,FLOAT,FLOAT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplswn.f
------------------------------------------------------------------*/

#define HPLSWN() CCALLSFSUB0(HPLSWN,hplswn)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplsym.f
------------------------------------------------------------------*/
/*
#define hplsym_ELEMS_6          ZTRINGV_NUM(1)
#define hplsym_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define HPLSYM(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(HPLSYM,hplsym,FLOATV,FLOATV,INT,INT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpltab.f
------------------------------------------------------------------*/

#define HPLTAB(A1,A2,A3,A4)  CCALLSFSUB4(HPLTAB,hpltab,INT,INT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpltit.f
------------------------------------------------------------------*/
/*
#define hpltit_ELEMS_1          ZTRINGV_NUM(1)
#define hpltit_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define HPLTIT(A1)  CCALLSFSUB1(HPLTIT,hpltit,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpltoc.f
------------------------------------------------------------------*/

#define HPLTOC(A1,A2,A3,A4,A5)  CCALLSFSUB5(HPLTOC,hpltoc,FLOAT,FLOAT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpltx1.f
------------------------------------------------------------------*/

#define HPLTX1(A1,A2)  CCALLSFSUB2(HPLTX1,hpltx1,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hpltxt.f
------------------------------------------------------------------*/
/*
#define hpltxt_ELEMS_3          ZTRINGV_NUM(1)
#define hpltxt_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define HPLTXT(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(HPLTXT,hpltxt,FLOAT,FLOAT,STRING,INT,INT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplu.f
------------------------------------------------------------------*/

#define HPLU(A1,A2)  CCALLSFSUB2(HPLU,hplu,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplusr.f
------------------------------------------------------------------*/
/*
#define hplusr_ELEMS_2          ZTRINGV_NUM(1)
#define hplusr_ELEMLEN_2        ZTRINGV_NUM(4)
*/

#define HPLUSR(A1,A2,A3)  CCALLSFSUB3(HPLUSR,hplusr,INT,STRING,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplwin.f
------------------------------------------------------------------*/

#define HPLWIN(A1,A2,A3,A4)  CCALLSFSUB4(HPLWIN,hplwin,INT,INT,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplwir.f
------------------------------------------------------------------*/
/*
#define hplwir_ELEMS_1          ZTRINGV_NUM(1)
#define hplwir_ELEMLEN_1        ZTRINGV_NUM(255)
#define hplwir_ELEMS_4          ZTRINGV_NUM(1)
#define hplwir_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define HPLWIR(A1,A2,A3,A4)  CCALLSFSUB4(HPLWIR,hplwir,STRING,FLOAT,FLOAT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplwn.f
------------------------------------------------------------------*/

#define HPLWN(A1,A2,A3,A4,A5)  CCALLSFSUB5(HPLWN,hplwn,INT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplzom.f
------------------------------------------------------------------*/
/*
#define hplzom_ELEMS_2          ZTRINGV_NUM(1)
#define hplzom_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define HPLZOM(A1,A2,A3,A4)  CCALLSFSUB4(HPLZOM,hplzom,INT,STRING,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : /users/cernlib/cern/new/src/cfs/hplot/hplzon.f
------------------------------------------------------------------*/
/*
#define hplzon_ELEMS_4          ZTRINGV_NUM(1)
#define hplzon_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define HPLZON(A1,A2,A3,A4)  CCALLSFSUB4(HPLZON,hplzon,INT,INT,INT,STRING,A1,A2,A3,A4)

