/*------------------------------------------------------------------
fortran filename   : iacwk.f
------------------------------------------------------------------*/

#define IACWK(A1)  CCALLSFSUB1(IACWK,iacwk,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iatr3.f
------------------------------------------------------------------*/
/*
#define iatr3_ELEMS_7          ZTRINGV_NUM(1)
#define iatr3_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IATR3(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IATR3,iatr3,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ica.f
------------------------------------------------------------------*/
/*
#define ica_ELEMS_8          ZTRINGV_NUM(1)
#define ica_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define ICA(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(ICA,ica,FLOAT,FLOAT,FLOAT,FLOAT,INT,INT,PINT,STRING,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : iclks.f
------------------------------------------------------------------*/

#define ICLKS() CCALLSFSUB0(ICLKS,iclks)

/*------------------------------------------------------------------
fortran filename   : iclrwk.f
------------------------------------------------------------------*/

#define ICLRWK(A1,A2)  CCALLSFSUB2(ICLRWK,iclrwk,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iclwk.f
------------------------------------------------------------------*/

#define ICLWK(A1)  CCALLSFSUB1(ICLWK,iclwk,INT,A1)

/*------------------------------------------------------------------
fortran filename   : idawk.f
------------------------------------------------------------------*/

#define IDAWK(A1)  CCALLSFSUB1(IDAWK,idawk,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iealpt.f
------------------------------------------------------------------*/

#define IEALPT(A1,A2)  CCALLSFSUB2(IEALPT,iealpt,PFLOAT,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iearc.f
------------------------------------------------------------------*/

#define IEARC(A1,A2)  CCALLSFSUB2(IEARC,iearc,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iearro.f
------------------------------------------------------------------*/

#define IEARRO(A1,A2)  CCALLSFSUB2(IEARRO,iearro,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ieaxis.f
------------------------------------------------------------------*/

#define IEAXIS(A1,A2)  CCALLSFSUB2(IEAXIS,ieaxis,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iebox.f
------------------------------------------------------------------*/

#define IEBOX(A1,A2)  CCALLSFSUB2(IEBOX,iebox,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iecpa.f
------------------------------------------------------------------*/

#define IECPA(A1,A2)  CCALLSFSUB2(IECPA,iecpa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iedele.f
------------------------------------------------------------------*/

#define IEDELE(A1,A2)  CCALLSFSUB2(IEDELE,iedele,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iefa.f
------------------------------------------------------------------*/

#define IEFA(A1,A2)  CCALLSFSUB2(IEFA,iefa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iefbox.f
------------------------------------------------------------------*/

#define IEFBOX(A1,A2)  CCALLSFSUB2(IEFBOX,iefbox,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iegrid.f
------------------------------------------------------------------*/

#define IEGRID() CCALLSFSUB0(IEGRID,iegrid)

/*------------------------------------------------------------------
fortran filename   : iemaca.f
------------------------------------------------------------------*/

#define IEMACA(A1,A2)  CCALLSFSUB2(IEMACA,iemaca,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemaxa.f
------------------------------------------------------------------*/

#define IEMAXA(A1,A2)  CCALLSFSUB2(IEMAXA,iemaxa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemboa.f
------------------------------------------------------------------*/

#define IEMBOA(A1,A2)  CCALLSFSUB2(IEMBOA,iemboa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemfaa.f
------------------------------------------------------------------*/

#define IEMFAA(A1,A2)  CCALLSFSUB2(IEMFAA,iemfaa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemfba.f
------------------------------------------------------------------*/

#define IEMFBA(A1,A2)  CCALLSFSUB2(IEMFBA,iemfba,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemhia.f
------------------------------------------------------------------*/

#define IEMHIA(A1,A2)  CCALLSFSUB2(IEMHIA,iemhia,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemopr.f
------------------------------------------------------------------*/

#define IEMOPR(A1,A2)  CCALLSFSUB2(IEMOPR,iemopr,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iempla.f
------------------------------------------------------------------*/

#define IEMPLA(A1,A2)  CCALLSFSUB2(IEMPLA,iempla,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iempma.f
------------------------------------------------------------------*/

#define IEMPMA(A1,A2)  CCALLSFSUB2(IEMPMA,iempma,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemsta.f
------------------------------------------------------------------*/

#define IEMSTA(A1,A2)  CCALLSFSUB2(IEMSTA,iemsta,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iemtxa.f
------------------------------------------------------------------*/

#define IEMTXA(A1,A2)  CCALLSFSUB2(IEMTXA,iemtxa,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iepave.f
------------------------------------------------------------------*/

#define IEPAVE(A1,A2)  CCALLSFSUB2(IEPAVE,iepave,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iepl.f
------------------------------------------------------------------*/

#define IEPL(A1,A2)  CCALLSFSUB2(IEPL,iepl,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iepm.f
------------------------------------------------------------------*/

#define IEPM(A1,A2)  CCALLSFSUB2(IEPM,iepm,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iepop.f
------------------------------------------------------------------*/

#define IEPOP(A1,A2)  CCALLSFSUB2(IEPOP,iepop,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iestx.f
------------------------------------------------------------------*/

#define IESTX(A1,A2)  CCALLSFSUB2(IESTX,iestx,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ietx.f
------------------------------------------------------------------*/

#define IETX(A1,A2)  CCALLSFSUB2(IETX,ietx,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iewin.f
------------------------------------------------------------------*/

#define IEWIN(A1,A2)  CCALLSFSUB2(IEWIN,iewin,PINT,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifa.f
------------------------------------------------------------------*/

#define IFA(A1,A2,A3)  CCALLSFSUB3(IFA,ifa,INT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ifa3.f
------------------------------------------------------------------*/

#define IFA3(A1,A2,A3,A4)  CCALLSFSUB4(IFA3,ifa3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ifclip.f
------------------------------------------------------------------*/

#define IFCLIP(A1,A2,A3,A4,A5)  CCALLSFSUB5(IFCLIP,ifclip,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PLOGICAL,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ifclr.f
------------------------------------------------------------------*/

#define IFCLR(A1)  CCALLSFSUB1(IFCLR,ifclr,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ifdraw.f
------------------------------------------------------------------*/

#define IFDRAW(A1,A2)  CCALLSFSUB2(IFDRAW,ifdraw,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifdrw9.f
------------------------------------------------------------------*/

#define IFDRW9(A1,A2)  CCALLSFSUB2(IFDRW9,ifdrw9,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifinit.f
------------------------------------------------------------------*/

#define IFINIT() CCALLSFSUB0(IFINIT,ifinit)

/*------------------------------------------------------------------
fortran filename   : ifmov6.f
------------------------------------------------------------------*/

#define IFMOV6(A1,A2)  CCALLSFSUB2(IFMOV6,ifmov6,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifmov9.f
------------------------------------------------------------------*/

#define IFMOV9(A1,A2)  CCALLSFSUB2(IFMOV9,ifmov9,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifmove.f
------------------------------------------------------------------*/

#define IFMOVE(A1,A2)  CCALLSFSUB2(IFMOVE,ifmove,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ifpl.f
------------------------------------------------------------------*/

#define IFPL(A1,A2,A3)  CCALLSFSUB3(IFPL,ifpl,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ifput.f
------------------------------------------------------------------*/

#define IFPUT(A1)  CCALLSFSUB1(IFPUT,ifput,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ifswin.f
------------------------------------------------------------------*/

#define IFSWIN(A1,A2,A3,A4)  CCALLSFSUB4(IFSWIN,ifswin,INT,INT,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ifvwin.f
------------------------------------------------------------------*/

#define IFVWIN(A1,A2,A3,A4)  CCALLSFSUB4(IFVWIN,ifvwin,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igadid.f
------------------------------------------------------------------*/

#define IGADID(A1,A2,A3)  CCALLSFSUB3(IGADID,igadid,INT,INT,INTV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igarc.f
------------------------------------------------------------------*/

#define IGARC(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGARC,igarc,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igarc1.f
------------------------------------------------------------------*/

#define IGARC1(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGARC1,igarc1,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : igarc2.f
------------------------------------------------------------------*/

#define IGARC2(A1,A2,A3)  CCALLSFSUB3(IGARC2,igarc2,FLOAT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igarc3.f
------------------------------------------------------------------*/

#define IGARC3(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGARC3,igarc3,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : igarc4.f
------------------------------------------------------------------*/

#define IGARC4(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGARC4,igarc4,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igasci.f
------------------------------------------------------------------*/
/*
#define igasci_ELEMS_2          ZTRINGV_NUM(1)
#define igasci_ELEMLEN_2        ZTRINGV_NUM(1)
*/

 PROTOCCALLSFFUN1(INT,IGASCI,igasci,STRING)
#define IGASCI(A2)  CCALLSFFUN1(IGASCI,igasci,STRING,A2)

/*------------------------------------------------------------------
fortran filename   : igaxi0.f
------------------------------------------------------------------*/

#define IGAXI0(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGAXI0,igaxi0,FLOAT,FLOAT,INT,PFLOAT,PFLOAT,PINT,FLOAT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igaxi1.f
------------------------------------------------------------------*/

#define IGAXI1(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IGAXI1,igaxi1,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igaxi2.f
------------------------------------------------------------------*/
/*
#define igaxi2_ELEMS_1          ZTRINGV_NUM(1)
#define igaxi2_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGAXI2(A1,A2,A3)  CCALLSFSUB3(IGAXI2,igaxi2,STRING,PINT,PINT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igaxi3.f
------------------------------------------------------------------*/

#define IGAXI3(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGAXI3,igaxi3,FLOAT,FLOAT,INT,PFLOAT,PFLOAT,PINT,PFLOAT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igaxi4.f
------------------------------------------------------------------*/

#define IGAXI4(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGAXI4,igaxi4,FLOAT,FLOAT,INT,PFLOAT,PFLOAT,PINT,PFLOAT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igaxis.f
------------------------------------------------------------------*/
/*
#define igaxis_ELEMS_8          ZTRINGV_NUM(1)
#define igaxis_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define IGAXIS(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IGAXIS,igaxis,FLOAT,FLOAT,FLOAT,FLOAT,PFLOAT,PFLOAT,PINT,STRING,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igbord.f
------------------------------------------------------------------*/

#define IGBORD(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGBORD,igbord,INT,FLOAT,FLOAT,FLOAT,FLOAT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igbox.f
------------------------------------------------------------------*/

#define IGBOX(A1,A2,A3,A4)  CCALLSFSUB4(IGBOX,igbox,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igccol.f
------------------------------------------------------------------*/
/*
#define igccol_ELEMS_2          ZTRINGV_NUM(1)
#define igccol_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGCCOL(A1,A2)  CCALLSFSUB2(IGCCOL,igccol,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igcell.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN5(FLOAT,IGCELL,igcell,INT,INT,PFLOAT,INT,INT)
#define IGCELL(A2,A3,A4,A5,A6)  CCALLSFFUN5(IGCELL,igcell,INT,INT,PFLOAT,INT,INT,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igcfai.f
------------------------------------------------------------------*/
/*
#define igcfai_ELEMS_2          ZTRINGV_NUM(1)
#define igcfai_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGCFAI(A1,A2)  CCALLSFSUB2(IGCFAI,igcfai,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igchwk.f
------------------------------------------------------------------*/

#define IGCHWK(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGCHWK,igchwk,INT,INT,INT,INT,INT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : igcles.f
------------------------------------------------------------------*/

#define IGCLES() CCALLSFSUB0(IGCLES,igcles)

/*------------------------------------------------------------------
fortran filename   : igcli1.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN6(INT,IGCLI1,igcli1,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT)
#define IGCLI1(A2,A3,A4,A5,A6,A7)  CCALLSFFUN6(IGCLI1,igcli1,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igclip.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN6(INT,IGCLIP,igclip,PFLOAT,PFLOAT,FLOAT,FLOAT,FLOAT,FLOAT)
#define IGCLIP(A2,A3,A4,A5,A6,A7)  CCALLSFFUN6(IGCLIP,igclip,PFLOAT,PFLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igclwn.f
------------------------------------------------------------------*/

#define IGCLWN(A1)  CCALLSFSUB1(IGCLWN,igclwn,INT,A1)

/*------------------------------------------------------------------
fortran filename   : igcmp.f
------------------------------------------------------------------*/
/*
#define igcmp_ELEMS_1          ZTRINGV_NUM(1)
#define igcmp_ELEMLEN_1        ZTRINGV_NUM(255)
#define igcmp_ELEMS_2          ZTRINGV_NUM(#{7ê{70#)
#define igcmp_ELEMLEN_2        ZTRINGV_NUM(255)
#define igcmp_ELEMS_3          ZTRINGV_NUM(#{7ê{70#)
#define igcmp_ELEMLEN_3        ZTRINGV_NUM(255)
#define igcmp_ELEMS_4          ZTRINGV_NUM(#{7ê{70#)
#define igcmp_ELEMLEN_4        ZTRINGV_NUM(255)
#define igcmp_ELEMS_5          ZTRINGV_NUM(1)
#define igcmp_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define IGCMP(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGCMP,igcmp,STRING,STRINGV,STRINGV,STRINGV,STRING,INTV,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igcolm.f
------------------------------------------------------------------*/
/*
#define igcolm_ELEMS_9          ZTRINGV_NUM(1)
#define igcolm_ELEMLEN_9        ZTRINGV_NUM(255)
*/

#define IGCOLM(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(IGCOLM,igcolm,FLOAT,FLOAT,FLOAT,FLOAT,PINT,PINT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : igconv.f
------------------------------------------------------------------*/

#define IGCONV(A1,A2,A3)  CCALLSFSUB3(IGCONV,igconv,INT,PFLOAT,PFLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igdate.f
------------------------------------------------------------------*/
/*
#define igdate_ELEMS_1          ZTRINGV_NUM(1)
#define igdate_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGDATE(A1)  CCALLSFSUB1(IGDATE,igdate,PSTRING,A1)

/*------------------------------------------------------------------
fortran filename   : igdelm.f
------------------------------------------------------------------*/

#define IGDELM(A1)  CCALLSFSUB1(IGDELM,igdelm,INTV,A1)

/*------------------------------------------------------------------
fortran filename   : igdime.f
------------------------------------------------------------------*/
/*
#define igdime_ELEMS_1          ZTRINGV_NUM(1)
#define igdime_ELEMLEN_1        ZTRINGV_NUM(255)
#define igdime_ELEMS_2          ZTRINGV_NUM(#{7ê{70#)
#define igdime_ELEMLEN_2        ZTRINGV_NUM(255)
#define igdime_ELEMS_3          ZTRINGV_NUM(#{7ê{70#)
#define igdime_ELEMLEN_3        ZTRINGV_NUM(255)
#define igdime_ELEMS_4          ZTRINGV_NUM(#{7ê{70#)
#define igdime_ELEMLEN_4        ZTRINGV_NUM(255)
#define igdime_ELEMS_5          ZTRINGV_NUM(#{7ê{70#)
#define igdime_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define IGDIME(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGDIME,igdime,STRING,STRINGV,STRINGV,STRINGV,STRINGV,INTV,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igdwk.f
------------------------------------------------------------------*/

#define IGDWK(A1)  CCALLSFSUB1(IGDWK,igdwk,PINT,A1)

/*------------------------------------------------------------------
fortran filename   : igend.f
------------------------------------------------------------------*/

#define IGEND() CCALLSFSUB0(IGEND,igend)

/*------------------------------------------------------------------
fortran filename   : igerr.f
------------------------------------------------------------------*/
/*
#define igerr_ELEMS_1          ZTRINGV_NUM(1)
#define igerr_ELEMLEN_1        ZTRINGV_NUM(255)
#define igerr_ELEMS_2          ZTRINGV_NUM(1)
#define igerr_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGERR(A1,A2)  CCALLSFSUB2(IGERR,igerr,STRING,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igex.f
------------------------------------------------------------------*/

#define IGEX(A1,A2)  CCALLSFSUB2(IGEX,igex,PFLOAT,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igfa.f
------------------------------------------------------------------*/

#define IGFA(A1,A2,A3)  CCALLSFSUB3(IGFA,igfa,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igfbox.f
------------------------------------------------------------------*/

#define IGFBOX(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IGFBOX,igfbox,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igfirs.f
------------------------------------------------------------------*/
/*
#define igfirs_ELEMS_1          ZTRINGV_NUM(1)
#define igfirs_ELEMLEN_1        ZTRINGV_NUM(255)
#define igfirs_ELEMS_2          ZTRINGV_NUM(1)
#define igfirs_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGFIRS(A1,A2)  CCALLSFSUB2(IGFIRS,igfirs,PSTRING,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igfr3d.f
------------------------------------------------------------------*/

#define IGFR3D(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGFR3D,igfr3d,INT,FLOATV,FLOATV,FLOATV,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : iggch.f
------------------------------------------------------------------*/

#define IGGCH(A1,A2,A3,A4)  CCALLSFSUB4(IGGCH,iggch,FLOAT,FLOAT,PINT,INTV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : iggdef.f
------------------------------------------------------------------*/
/*
#define iggdef_ELEMS_1          ZTRINGV_NUM(1)
#define iggdef_ELEMLEN_1        ZTRINGV_NUM(255)
#define iggdef_ELEMS_3          ZTRINGV_NUM(1)
#define iggdef_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IGGDEF(A1,A2,A3)  CCALLSFSUB3(IGGDEF,iggdef,STRING,INT,PSTRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : iggfin.f
------------------------------------------------------------------*/

#define IGGFIN(A1)  CCALLSFSUB1(IGGFIN,iggfin,PINT,A1)

/*------------------------------------------------------------------
fortran filename   : iggrid.f
------------------------------------------------------------------*/

#define IGGRID(A1,A2)  CCALLSFSUB2(IGGRID,iggrid,PFLOAT,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ighatc.f
------------------------------------------------------------------*/

#define IGHATC(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGHATC,ighatc,FLOAT,FLOAT,INT,FLOATV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ighcxy.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN3(FLOAT,IGHCXY,ighcxy,INT,INT,INT)
#define IGHCXY(A2,A3,A4)  CCALLSFFUN3(IGHCXY,ighcxy,INT,INT,INT,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ighigh.f
------------------------------------------------------------------*/
/*
#define ighigh_ELEMS_2          ZTRINGV_NUM(#{7ê{70#)
#define ighigh_ELEMLEN_2        ZTRINGV_NUM(255)
#define ighigh_ELEMS_3          ZTRINGV_NUM(#{7ê{70#)
#define ighigh_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IGHIGH(A1,A2,A3,A4)  CCALLSFSUB4(IGHIGH,ighigh,INT,STRINGV,STRINGV,INTV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ighis1.f
------------------------------------------------------------------*/

#define IGHIS1(A1,A2,A3,A4)  CCALLSFSUB4(IGHIS1,ighis1,INT,INT,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ighist.f
------------------------------------------------------------------*/
/*
#define ighist_ELEMS_4          ZTRINGV_NUM(1)
#define ighist_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGHIST(A1,A2,A3,A4)  CCALLSFSUB4(IGHIST,ighist,INT,FLOATV,FLOATV,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ighr01.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN3(FLOAT,IGHR01,ighr01,FLOAT,FLOAT,FLOAT)
#define IGHR01(A2,A3,A4)  CCALLSFFUN3(IGHR01,ighr01,FLOAT,FLOAT,FLOAT,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ightor.f
------------------------------------------------------------------*/

#define IGHTOR(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGHTOR,ightor,FLOAT,FLOAT,FLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igicol.f
------------------------------------------------------------------*/
/*
#define igicol_ELEMS_2          ZTRINGV_NUM(1)
#define igicol_ELEMLEN_2        ZTRINGV_NUM(255)
*/

 PROTOCCALLSFFUN1(INT,IGICOL,igicol,STRING)
#define IGICOL(A2)  CCALLSFFUN1(IGICOL,igicol,STRING,A2)

/*------------------------------------------------------------------
fortran filename   : igifai.f
------------------------------------------------------------------*/
/*
#define igifai_ELEMS_2          ZTRINGV_NUM(1)
#define igifai_ELEMLEN_2        ZTRINGV_NUM(255)
*/

 PROTOCCALLSFFUN1(INT,IGIFAI,igifai,STRING)
#define IGIFAI(A2)  CCALLSFFUN1(IGIFAI,igifai,STRING,A2)

/*------------------------------------------------------------------
fortran filename   : iginit.f
------------------------------------------------------------------*/

#define IGINIT(A1)  CCALLSFSUB1(IGINIT,iginit,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iginme.f
------------------------------------------------------------------*/

#define IGINME() CCALLSFSUB0(IGINME,iginme)

/*------------------------------------------------------------------
fortran filename   : igiwin.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,IGIWIN,igiwin,INT)
#define IGIWIN(A2)  CCALLSFFUN1(IGIWIN,igiwin,INT,A2)

/*------------------------------------------------------------------
fortran filename   : igiwty.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,IGIWTY,igiwty,INT)
#define IGIWTY(A2)  CCALLSFFUN1(IGIWTY,igiwty,INT,A2)

/*------------------------------------------------------------------
fortran filename   : iglbl.f
------------------------------------------------------------------*/
/*
#define iglbl_ELEMS_2          ZTRINGV_NUM(#{7ê{70#)
#define iglbl_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGLBL(A1,A2)  CCALLSFSUB2(IGLBL,iglbl,INT,STRINGV,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iglev.f
------------------------------------------------------------------*/
/*
#define iglev_ELEMS_5          ZTRINGV_NUM(1)
#define iglev_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define IGLEV(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGLEV,iglev,INT,FLOAT,FLOAT,INTV,STRING,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : igline.f
------------------------------------------------------------------*/

#define IGLINE(A1,A2,A3,A4)  CCALLSFSUB4(IGLINE,igline,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igloc.f
------------------------------------------------------------------*/

#define IGLOC(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGLOC,igloc,INT,PINT,PINT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igloc2.f
------------------------------------------------------------------*/
/*
#define igloc2_ELEMS_8          ZTRINGV_NUM(1)
#define igloc2_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define IGLOC2(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IGLOC2,igloc2,INT,INT,FLOAT,FLOAT,FLOAT,FLOAT,INT,STRING,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igm100.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(LOGICAL,IGM100,igm100,INT)
#define IGM100(A2)  CCALLSFFUN1(IGM100,igm100,INT,A2)

/*------------------------------------------------------------------
fortran filename   : igmenu.f
------------------------------------------------------------------*/
/*
#define igmenu_ELEMS_2          ZTRINGV_NUM(1)
#define igmenu_ELEMLEN_2        ZTRINGV_NUM(255)
#define igmenu_ELEMS_8          ZTRINGV_NUM(##)
#define igmenu_ELEMLEN_8        ZTRINGV_NUM(255)
#define igmenu_ELEMS_10         ZTRINGV_NUM(##)
#define igmenu_ELEMLEN_10       ZTRINGV_NUM(255)
#define igmenu_ELEMS_11         ZTRINGV_NUM(##)
#define igmenu_ELEMLEN_11       ZTRINGV_NUM(255)
#define igmenu_ELEMS_12         ZTRINGV_NUM(##)
#define igmenu_ELEMLEN_12       ZTRINGV_NUM(255)
#define igmenu_ELEMS_14         ZTRINGV_NUM(1)
#define igmenu_ELEMLEN_14       ZTRINGV_NUM(255)
*/

#define IGMENU(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)  CCALLSFSUB14(IGMENU,igmenu,INT,STRING,PFLOAT,PFLOAT,PFLOAT,PFLOAT,INT,STRINGV,INT,STRINGV,STRINGV,PSTRINGV,PINT,STRING,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14)

/*------------------------------------------------------------------
fortran filename   : igmess.f
------------------------------------------------------------------*/
/*
#define igmess_ELEMS_2          ZTRINGV_NUM(ZTRINGV_ARGS(1))
#define igmess_ELEMLEN_2        ZTRINGV_NUM(255)
#define igmess_ELEMS_3          ZTRINGV_NUM(1)
#define igmess_ELEMLEN_3        ZTRINGV_NUM(255)
#define igmess_ELEMS_4          ZTRINGV_NUM(1)
#define igmess_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGMESS(A1,A2,A3,A4)  CCALLSFSUB4(IGMESS,igmess,INT,STRINGV,STRING,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igmeta.f
------------------------------------------------------------------*/

#define IGMETA(A1,A2)  CCALLSFSUB2(IGMETA,igmeta,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igmetn.f
------------------------------------------------------------------*/
/*
#define igmetn_ELEMS_1          ZTRINGV_NUM(1)
#define igmetn_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGMETN(A1)  CCALLSFSUB1(IGMETN,igmetn,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : igobj.f
------------------------------------------------------------------*/
/*
#define igobj_ELEMS_5          ZTRINGV_NUM(##)
#define igobj_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define IGOBJ(A1,A2,A3,A4,A5)  CCALLSFSUB5(IGOBJ,igobj,INT,INT,PINT,PINT,PSTRINGV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : igparf.f
------------------------------------------------------------------*/
/*
#define igparf_ELEMS_2          ZTRINGV_NUM(1)
#define igparf_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGPARF(A1,A2)  CCALLSFSUB2(IGPARF,igparf,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igpave.f
------------------------------------------------------------------*/
/*
#define igpave_ELEMS_8          ZTRINGV_NUM(1)
#define igpave_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define IGPAVE(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IGPAVE,igpave,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,INT,INT,STRING,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igphsl.f
------------------------------------------------------------------*/
/*
#define igphsl_ELEMS_3          ZTRINGV_NUM(1)
#define igphsl_ELEMLEN_3        ZTRINGV_NUM(255)
#define igphsl_ELEMS_4          ZTRINGV_NUM(1)
#define igphsl_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGPHSL(A1,A2,A3,A4)  CCALLSFSUB4(IGPHSL,igphsl,FLOAT,FLOAT,STRING,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igphst.f
------------------------------------------------------------------*/

#define IGPHST(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGPHST,igphst,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igpick.f
------------------------------------------------------------------*/
/*
#define igpick_ELEMS_5          ZTRINGV_NUM(#{7ê{70#)
#define igpick_ELEMLEN_5        ZTRINGV_NUM(255)
#define igpick_ELEMS_7          ZTRINGV_NUM(1)
#define igpick_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IGPICK(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGPICK,igpick,INT,FLOAT,FLOAT,PINT,PSTRINGV,PINT,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igpid.f
------------------------------------------------------------------*/
/*
#define igpid_ELEMS_2          ZTRINGV_NUM(1)
#define igpid_ELEMLEN_2        ZTRINGV_NUM(255)
#define igpid_ELEMS_4          ZTRINGV_NUM(1)
#define igpid_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGPID(A1,A2,A3,A4)  CCALLSFSUB4(IGPID,igpid,INT,STRING,INT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igpie.f
------------------------------------------------------------------*/
/*
#define igpie_ELEMS_6          ZTRINGV_NUM(1)
#define igpie_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IGPIE(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(IGPIE,igpie,FLOAT,FLOAT,FLOAT,INT,FLOATV,STRING,INTV,INTV,INTV,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : igpkfa.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN5(INT,IGPKFA,igpkfa,FLOAT,FLOAT,INT,FLOATV,FLOATV)
#define IGPKFA(A2,A3,A4,A5,A6)  CCALLSFFUN5(IGPKFA,igpkfa,FLOAT,FLOAT,INT,FLOATV,FLOATV,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igpkpl.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN7(INT,IGPKPL,igpkpl,FLOAT,FLOAT,INT,FLOATV,FLOATV,FLOAT,FLOAT)
#define IGPKPL(A2,A3,A4,A5,A6,A7,A8)  CCALLSFFUN7(IGPKPL,igpkpl,FLOAT,FLOAT,INT,FLOATV,FLOATV,FLOAT,FLOAT,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igpkpm.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN7(INT,IGPKPM,igpkpm,FLOAT,FLOAT,INT,FLOATV,FLOATV,FLOAT,FLOAT)
#define IGPKPM(A2,A3,A4,A5,A6,A7,A8)  CCALLSFFUN7(IGPKPM,igpkpm,FLOAT,FLOAT,INT,FLOATV,FLOATV,FLOAT,FLOAT,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : igplot.f
------------------------------------------------------------------*/

#define IGPLOT(A1,A2)  CCALLSFSUB2(IGPLOT,igplot,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igpm.f
------------------------------------------------------------------*/

#define IGPM(A1,A2,A3,A4)  CCALLSFSUB4(IGPM,igpm,INT,FLOATV,FLOATV,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igpxmp.f
------------------------------------------------------------------*/
/*
#define igpxmp_ELEMS_4          ZTRINGV_NUM(1)
#define igpxmp_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGPXMP(A1,A2,A3,A4)  CCALLSFSUB4(IGPXMP,igpxmp,PINT,INT,INT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igq.f
------------------------------------------------------------------*/
/*
#define igq_ELEMS_1          ZTRINGV_NUM(1)
#define igq_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGQ(A1,A2)  CCALLSFSUB2(IGQ,igq,STRING,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igqwk.f
------------------------------------------------------------------*/
/*
#define igqwk_ELEMS_2          ZTRINGV_NUM(1)
#define igqwk_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGQWK(A1,A2,A3)  CCALLSFSUB3(IGQWK,igqwk,INT,STRING,PFLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igrap1.f
------------------------------------------------------------------*/

#define IGRAP1(A1,A2,A3,A4)  CCALLSFSUB4(IGRAP1,igrap1,PFLOAT,PFLOAT,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igrap2.f
------------------------------------------------------------------*/

#define IGRAP2(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGRAP2,igrap2,PINT,FLOAT,FLOAT,FLOAT,PFLOAT,PFLOAT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igraph.f
------------------------------------------------------------------*/
/*
#define igraph_ELEMS_4          ZTRINGV_NUM(1)
#define igraph_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IGRAPH(A1,A2,A3,A4)  CCALLSFSUB4(IGRAPH,igraph,INT,FLOATV,FLOATV,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igreq.f
------------------------------------------------------------------*/
/*
#define igreq_ELEMS_6          ZTRINGV_NUM(ZTRINGV_ARGS(3))
#define igreq_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IGREQ(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGREQ,igreq,INT,INT,INT,PINT,PINT,STRINGV,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igrng.f
------------------------------------------------------------------*/

#define IGRNG(A1,A2)  CCALLSFSUB2(IGRNG,igrng,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igrsiz.f
------------------------------------------------------------------*/

#define IGRSIZ(A1)  CCALLSFSUB1(IGRSIZ,igrsiz,INT,A1)

/*------------------------------------------------------------------
fortran filename   : igrtoh.f
------------------------------------------------------------------*/

#define IGRTOH(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGRTOH,igrtoh,FLOAT,FLOAT,FLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igsa.f
------------------------------------------------------------------*/

#define IGSA(A1)  CCALLSFSUB1(IGSA,igsa,INT,A1)

/*------------------------------------------------------------------
fortran filename   : igscut.f
------------------------------------------------------------------*/

#define IGSCUT(A1,A2,A3)  CCALLSFSUB3(IGSCUT,igscut,FLOAT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igset.f
------------------------------------------------------------------*/
/*
#define igset_ELEMS_1          ZTRINGV_NUM(1)
#define igset_ELEMLEN_1        ZTRINGV_NUM(4)
*/

#define IGSET(A1,A2)  CCALLSFSUB2(IGSET,igset,STRING,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igset1.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(FLOAT,IGSET1,igset1,FLOAT)
#define IGSET1(A2)  CCALLSFFUN1(IGSET1,igset1,FLOAT,A2)

/*------------------------------------------------------------------
fortran filename   : igsg.f
------------------------------------------------------------------*/

#define IGSG(A1)  CCALLSFSUB1(IGSG,igsg,INT,A1)

/*------------------------------------------------------------------
fortran filename   : igsmp.f
------------------------------------------------------------------*/

#define IGSMP(A1)  CCALLSFSUB1(IGSMP,igsmp,INTV,A1)

/*------------------------------------------------------------------
fortran filename   : igsrap.f
------------------------------------------------------------------*/

#define IGSRAP(A1)  CCALLSFSUB1(IGSRAP,igsrap,PFLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : igsse.f
------------------------------------------------------------------*/

#define IGSSE(A1,A2)  CCALLSFSUB2(IGSSE,igsse,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igstr.f
------------------------------------------------------------------*/
/*
#define igstr_ELEMS_2          ZTRINGV_NUM(1)
#define igstr_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IGSTR(A1,A2)  CCALLSFSUB2(IGSTR,igstr,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : igtab1.f
------------------------------------------------------------------*/
/*
 PROTOCCALLSFFUN12(INT,IGTAB1,igtab1,FLOAT,INT,FLOAT,FLOAT,FLOAT,INT,FLOAT,FLOAT,PFLOAT,PFLOAT,PINT,INT)
#define IGTAB1(A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)  CCALLSFFUN12(IGTAB1,igtab1,FLOAT,INT,FLOAT,FLOAT,FLOAT,INT,FLOAT,FLOAT,PFLOAT,PFLOAT,PINT,INT,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)
*/
/*------------------------------------------------------------------
fortran filename   : igtab2.f
------------------------------------------------------------------*/

#define IGTAB2(A1,A2,A3,A4)  CCALLSFSUB4(IGTAB2,igtab2,INT,INT,PFLOAT,PFLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igtab3.f
------------------------------------------------------------------*/

#define IGTAB3(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGTAB3,igtab3,INT,INT,PINT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igtab4.f
------------------------------------------------------------------*/

#define IGTAB4(A1,A2,A3)  CCALLSFSUB3(IGTAB4,igtab4,FLOATV,FLOATV,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : igtab5.f
------------------------------------------------------------------*/

#define IGTAB5(A1,A2,A3,A4)  CCALLSFSUB4(IGTAB5,igtab5,FLOATV,FLOATV,FLOAT,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igtab6.f
------------------------------------------------------------------*/

#define IGTAB6(A1,A2,A3,A4)  CCALLSFSUB4(IGTAB6,igtab6,INT,INT,PFLOAT,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : igtabl.f
------------------------------------------------------------------*/

#define IGTABL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGTABL,igtabl,INT,INT,FLOAT,INT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igtcch.f
------------------------------------------------------------------*/
/*
#define igtcch_ELEMS_1          ZTRINGV_NUM(1)
#define igtcch_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGTCCH(A1)  CCALLSFSUB1(IGTCCH,igtcch,PSTRING,A1)

/*------------------------------------------------------------------
fortran filename   : igterm.f
------------------------------------------------------------------*/

#define IGTERM() CCALLSFSUB0(IGTERM,igterm)

/*------------------------------------------------------------------
fortran filename   : igtext.f
------------------------------------------------------------------*/
/*
#define igtext_ELEMS_3          ZTRINGV_NUM(1)
#define igtext_ELEMLEN_3        ZTRINGV_NUM(255)
#define igtext_ELEMS_6          ZTRINGV_NUM(1)
#define igtext_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IGTEXT(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IGTEXT,igtext,FLOAT,FLOAT,STRING,FLOAT,PFLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : igwkty.f
------------------------------------------------------------------*/

#define IGWKTY(A1)  CCALLSFSUB1(IGWKTY,igwkty,PINT,A1)

/*------------------------------------------------------------------
fortran filename   : igxmes.f
------------------------------------------------------------------*/
/*
#define igxmes_ELEMS_5          ZTRINGV_NUM(ZTRINGV_ARGS(4))
#define igxmes_ELEMLEN_5        ZTRINGV_NUM(255)
#define igxmes_ELEMS_6          ZTRINGV_NUM(1)
#define igxmes_ELEMLEN_6        ZTRINGV_NUM(255)
#define igxmes_ELEMS_7          ZTRINGV_NUM(1)
#define igxmes_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IGXMES(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IGXMES,igxmes,INT,INT,INT,INT,STRINGV,STRING,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : igzset.f
------------------------------------------------------------------*/
/*
#define igzset_ELEMS_1          ZTRINGV_NUM(1)
#define igzset_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IGZSET(A1)  CCALLSFSUB1(IGZSET,igzset,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : ihaxiv.f
------------------------------------------------------------------*/

#define IHAXIV(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)  CCALLSFSUB10(IHAXIV,ihaxiv,FLOATV,FLOATV,FLOAT,PFLOAT,PINT,PINT,PINT,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)

/*------------------------------------------------------------------
fortran filename   : ihbbox.f
------------------------------------------------------------------*/

#define IHBBOX(A1,A2,A3,A4)  CCALLSFSUB4(IHBBOX,ihbbox,FLOATV,FLOATV,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihclev.f
------------------------------------------------------------------*/

#define IHCLEV(A1,A2,A3,A4)  CCALLSFSUB4(IHCLEV,ihclev,INT,FLOATV,INTV,PINT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihcspe.f
------------------------------------------------------------------*/

#define IHCSPE(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHCSPE,ihcspe,INT,FLOAT,FLOAT,INT,INT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihdf01.f
------------------------------------------------------------------*/

#define IHDF01(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDF01,ihdf01,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdf02.f
------------------------------------------------------------------*/

#define IHDF02(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDF02,ihdf02,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdf03.f
------------------------------------------------------------------*/

#define IHDF03(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDF03,ihdf03,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdfl1.f
------------------------------------------------------------------*/

#define IHDFL1(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDFL1,ihdfl1,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdfl2.f
------------------------------------------------------------------*/

#define IHDFL2(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDFL2,ihdfl2,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdfr1.f
------------------------------------------------------------------*/

#define IHDFR1(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDFR1,ihdfr1,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihdfr2.f
------------------------------------------------------------------*/

#define IHDFR2(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHDFR2,ihdfr2,INTV,PFLOAT,INT,INTV,FLOATV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : iheran.f
------------------------------------------------------------------*/

#define IHERAN(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IHERAN,iheran,FLOATV,FLOATV,FLOAT,FLOAT,FLOAT,FLOAT,PINT,PFLOAT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : ihfbox.f
------------------------------------------------------------------*/

#define IHFBOX(A1,A2,A3,A4)  CCALLSFSUB4(IHFBOX,ihfbox,FLOATV,FLOATV,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihimpf.f
------------------------------------------------------------------*/
/*
#define ihimpf_ELEMS_8          ZTRINGV_NUM(1)
#define ihimpf_ELEMLEN_8        ZTRINGV_NUM(255)
*/

#define IHIMPF(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IHIMPF,ihimpf,FLOAT,FLOATV,FLOATV,INT,INT,INT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : ihiso.f
------------------------------------------------------------------*/
/*
#define ihiso_ELEMS_11         ZTRINGV_NUM(1)
#define ihiso_ELEMLEN_11       ZTRINGV_NUM(255)
*/

#define IHISO(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)  CCALLSFSUB11(IHISO,ihiso,INT,FLOATV,INT,INT,INT,FLOATV,FLOATV,FLOATV,PFLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)

/*------------------------------------------------------------------
fortran filename   : ihlegc.f
------------------------------------------------------------------*/
/*
#define ihlegc_ELEMS_6          ZTRINGV_NUM(1)
#define ihlegc_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHLEGC(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHLEGC,ihlegc,FLOAT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihlegp.f
------------------------------------------------------------------*/
/*
#define ihlegp_ELEMS_6          ZTRINGV_NUM(1)
#define ihlegp_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHLEGP(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHLEGP,ihlegp,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihlegr.f
------------------------------------------------------------------*/
/*
#define ihlegr_ELEMS_6          ZTRINGV_NUM(1)
#define ihlegr_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHLEGR(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHLEGR,ihlegr,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihlegs.f
------------------------------------------------------------------*/
/*
#define ihlegs_ELEMS_7          ZTRINGV_NUM(1)
#define ihlegs_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IHLEGS(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHLEGS,ihlegs,INT,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihligh.f
------------------------------------------------------------------*/

#define IHLIGH(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHLIGH,ihligh,INT,FLOAT,FLOAT,FLOAT,FLOAT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihlumi.f
------------------------------------------------------------------*/

#define IHLUMI(A1,A2)  CCALLSFSUB2(IHLUMI,ihlumi,FLOATV,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihmc00.f
------------------------------------------------------------------*/

#define IHMC00(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)  CCALLSFSUB11(IHMC00,ihmc00,INT,INT,INT,INT,INT,INT,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)

/*------------------------------------------------------------------
fortran filename   : ihmc03.f
------------------------------------------------------------------*/

#define IHMC03(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC03,ihmc03,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc04.f
------------------------------------------------------------------*/

#define IHMC04(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC04,ihmc04,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc06.f
------------------------------------------------------------------*/

#define IHMC06(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC06,ihmc06,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc07.f
------------------------------------------------------------------*/

#define IHMC07(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC07,ihmc07,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc10.f
------------------------------------------------------------------*/

#define IHMC10(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC10,ihmc10,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc12.f
------------------------------------------------------------------*/

#define IHMC12(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC12,ihmc12,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmc13.f
------------------------------------------------------------------*/

#define IHMC13(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHMC13,ihmc13,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihmcmp.f
------------------------------------------------------------------*/

#define IHMCMP(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHMCMP,ihmcmp,INT,PFLOAT,PFLOAT,PINT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihmcpp.f
------------------------------------------------------------------*/

#define IHMCPP(A1,A2,A3,A4)  CCALLSFSUB4(IHMCPP,ihmcpp,INT,INTV,PFLOAT,PFLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihmctt.f
------------------------------------------------------------------*/

#define IHMCTT(A1,A2,A3)  CCALLSFSUB3(IHMCTT,ihmctt,INT,PINT,PINT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ihmcub.f
------------------------------------------------------------------*/

#define IHMCUB(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(IHMCUB,ihmcub,FLOAT,PFLOAT,FLOATV,PFLOAT,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : ihmcx.f
------------------------------------------------------------------*/

#define IHMCX(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(IHMCX,ihmcx,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,PINT,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : ihpff.f
------------------------------------------------------------------*/

#define IHPFF(A1,A2,A3)  CCALLSFSUB3(IHPFF,ihpff,INT,PFLOAT,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ihprop.f
------------------------------------------------------------------*/

#define IHPROP(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHPROP,ihprop,FLOAT,FLOAT,FLOAT,INT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihpvie.f
------------------------------------------------------------------*/

#define IHPVIE(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHPVIE,ihpvie,FLOATV,FLOATV,FLOATV,FLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihrcle.f
------------------------------------------------------------------*/

#define IHRCLE() CCALLSFSUB0(IHRCLE,ihrcle)

/*------------------------------------------------------------------
fortran filename   : ihrfil.f
------------------------------------------------------------------*/

#define IHRFIL(A1,A2)  CCALLSFSUB2(IHRFIL,ihrfil,INT,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihrini.f
------------------------------------------------------------------*/

#define IHRINI(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHRINI,ihrini,FLOAT,FLOAT,FLOAT,FLOAT,INT,INT,INTV,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihrlin.f
------------------------------------------------------------------*/

#define IHRLIN(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHRLIN,ihrlin,FLOATV,FLOATV,INT,PINT,PFLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihroff.f
------------------------------------------------------------------*/

#define IHROFF() CCALLSFSUB0(IHROFF,ihroff)

/*------------------------------------------------------------------
fortran filename   : ihsdrl.f
------------------------------------------------------------------*/

#define IHSDRL(A1,A2)  CCALLSFSUB2(IHSDRL,ihsdrl,FLOATV,FLOATV,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihsini.f
------------------------------------------------------------------*/

#define IHSINI(A1,A2)  CCALLSFSUB2(IHSINI,ihsini,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihsmdf.f
------------------------------------------------------------------*/

#define IHSMDF(A1,A2)  CCALLSFSUB2(IHSMDF,ihsmdf,FLOATV,FLOATV,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihsurc.f
------------------------------------------------------------------*/
/*
#define ihsurc_ELEMS_6          ZTRINGV_NUM(1)
#define ihsurc_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHSURC(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHSURC,ihsurc,FLOAT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihsurp.f
------------------------------------------------------------------*/
/*
#define ihsurp_ELEMS_6          ZTRINGV_NUM(1)
#define ihsurp_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHSURP(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHSURP,ihsurp,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihsurr.f
------------------------------------------------------------------*/
/*
#define ihsurr_ELEMS_6          ZTRINGV_NUM(1)
#define ihsurr_ELEMLEN_6        ZTRINGV_NUM(255)
*/

#define IHSURR(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHSURR,ihsurr,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihsurs.f
------------------------------------------------------------------*/
/*
#define ihsurs_ELEMS_7          ZTRINGV_NUM(1)
#define ihsurs_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IHSURS(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHSURS,ihsurs,INT,INT,INT,INT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihsvie.f
------------------------------------------------------------------*/

#define IHSVIE(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHSVIE,ihsvie,FLOATV,FLOATV,FLOAT,FLOAT,FLOAT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihwlin.f
------------------------------------------------------------------*/

#define IHWLIN(A1,A2,A3)  CCALLSFSUB3(IHWLIN,ihwlin,INT,PFLOAT,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ihwphi.f
------------------------------------------------------------------*/

#define IHWPHI(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHWPHI,ihwphi,INT,PINT,PFLOAT,PINT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihwth.f
------------------------------------------------------------------*/

#define IHWTH(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHWTH,ihwth,INT,FLOAT,PINT,PFLOAT,PINT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihwton.f
------------------------------------------------------------------*/

#define IHWTON(A1,A2)  CCALLSFSUB2(IHWTON,ihwton,FLOATV,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihwv01.f
------------------------------------------------------------------*/

#define IHWV01(A1,A2,A3,A4,A5)  CCALLSFSUB5(IHWV01,ihwv01,FLOATV,FLOATV,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ihwv02.f
------------------------------------------------------------------*/

#define IHWV02(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)  CCALLSFSUB10(IHWV02,ihwv02,FLOATV,FLOATV,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)

/*------------------------------------------------------------------
fortran filename   : ihwvr1.f
------------------------------------------------------------------*/

#define IHWVR1(A1,A2,A3,A4)  CCALLSFSUB4(IHWVR1,ihwvr1,INT,FLOAT,FLOAT,PFLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihwvr2.f
------------------------------------------------------------------*/

#define IHWVR2(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IHWVR2,ihwvr2,FLOAT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : ihwwnn.f
------------------------------------------------------------------*/

#define IHWWNN(A1,A2)  CCALLSFSUB2(IHWWNN,ihwwnn,FLOATV,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihwzn.f
------------------------------------------------------------------*/

#define IHWZN(A1,A2,A3,A4)  CCALLSFSUB4(IHWZN,ihwzn,FLOAT,FLOAT,FLOAT,PFLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihzbuf.f
------------------------------------------------------------------*/

#define IHZBUF(A1,A2,A3,A4)  CCALLSFSUB4(IHZBUF,ihzbuf,INT,INT,INTV,INTV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihzcle.f
------------------------------------------------------------------*/

#define IHZCLE(A1,A2)  CCALLSFSUB2(IHZCLE,ihzcle,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ihzcli.f
------------------------------------------------------------------*/

#define IHZCLI(A1,A2,A3,A4)  CCALLSFSUB4(IHZCLI,ihzcli,INT,INT,INT,INT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ihzdep.f
------------------------------------------------------------------*/

#define IHZDEP(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHZDEP,ihzdep,PFLOAT,PINT,PINT,PFLOAT,PFLOAT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihzfil.f
------------------------------------------------------------------*/

#define IHZFIL(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHZFIL,ihzfil,INT,INT,INTV,INTV,INTV,INTV,INTV,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihzlin.f
------------------------------------------------------------------*/

#define IHZLIN(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHZLIN,ihzlin,INT,INT,INT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihzpor.f
------------------------------------------------------------------*/

#define IHZPOR(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHZPOR,ihzpor,INT,INT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihzscn.f
------------------------------------------------------------------*/

#define IHZSCN(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHZSCN,ihzscn,INT,INT,INT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihztra.f
------------------------------------------------------------------*/

#define IHZTRA(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHZTRA,ihztra,FLOAT,FLOAT,FLOAT,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ihztst.f
------------------------------------------------------------------*/

#define IHZTST(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IHZTST,ihztst,FLOAT,PFLOAT,INT,INT,INTV,FLOATV,PINT,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : ihzwin.f
------------------------------------------------------------------*/

#define IHZWIN(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IHZWIN,ihzwin,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ikbox.f
------------------------------------------------------------------*/

#define IKBOX(A1,A2,A3,A4)  CCALLSFSUB4(IKBOX,ikbox,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ikfile.f
------------------------------------------------------------------*/

#define IKFILE(A1)  CCALLSFSUB1(IKFILE,ikfile,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ikfntx.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,IKFNTX,ikfntx,INT)
#define IKFNTX(A2)  CCALLSFFUN1(IKFNTX,ikfntx,INT,A2)

/*------------------------------------------------------------------
fortran filename   : ikpl.f
------------------------------------------------------------------*/

#define IKPL(A1,A2,A3)  CCALLSFSUB3(IKPL,ikpl,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ikuwk.f
------------------------------------------------------------------*/

#define IKUWK(A1)  CCALLSFSUB1(IKUWK,ikuwk,INT,A1)

/*------------------------------------------------------------------
fortran filename   : imfin.f
------------------------------------------------------------------*/
/*
#define imfin_ELEMS_2          ZTRINGV_NUM(1)
#define imfin_ELEMLEN_2        ZTRINGV_NUM(255)
*/

 PROTOCCALLSFFUN3(INT,IMFIN,imfin,STRING,INT,PFLOAT)
#define IMFIN(A2,A3,A4)  CCALLSFFUN3(IMFIN,imfin,STRING,INT,PFLOAT,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : imfout.f
------------------------------------------------------------------*/

#define IMFOUT(A1,A2)  CCALLSFSUB2(IMFOUT,imfout,INT,FLOATV,A1,A2)

/*------------------------------------------------------------------
fortran filename   : imint.f
------------------------------------------------------------------*/
/*
#define imint_ELEMS_1          ZTRINGV_NUM(1)
#define imint_ELEMLEN_1        ZTRINGV_NUM(80)
*/

#define IMINT(A1,A2)  CCALLSFSUB2(IMINT,imint,PSTRING,PINT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iml.f
------------------------------------------------------------------*/

#define IML(A1,A2,A3)  CCALLSFSUB3(IML,iml,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : imtek.f
------------------------------------------------------------------*/

#define IMTEK(A1)  CCALLSFSUB1(IMTEK,imtek,INT,A1)

/*------------------------------------------------------------------
fortran filename   : imwrit.f
------------------------------------------------------------------*/

#define IMWRIT(A1)  CCALLSFSUB1(IMWRIT,imwrit,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iopks.f
------------------------------------------------------------------*/

#define IOPKS(A1)  CCALLSFSUB1(IOPKS,iopks,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iopwk.f
------------------------------------------------------------------*/

#define IOPWK(A1,A2,A3)  CCALLSFSUB3(IOPWK,iopwk,INT,INT,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipdef.f
------------------------------------------------------------------*/

#define IPDEF() CCALLSFSUB0(IPDEF,ipdef)

/*------------------------------------------------------------------
fortran filename   : ipdlin.f
------------------------------------------------------------------*/

#define IPDLIN(A1,A2,A3)  CCALLSFSUB3(IPDLIN,ipdlin,PINT,PINT,PINT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipdraw.f
------------------------------------------------------------------*/

#define IPDRAW(A1,A2,A3)  CCALLSFSUB3(IPDRAW,ipdraw,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipend.f
------------------------------------------------------------------*/

#define IPEND() CCALLSFSUB0(IPEND,ipend)

/*------------------------------------------------------------------
fortran filename   : ipfon.f
------------------------------------------------------------------*/

#define IPFON() CCALLSFSUB0(IPFON,ipfon)

/*------------------------------------------------------------------
fortran filename   : ipfout.f
------------------------------------------------------------------*/

#define IPFOUT(A1)  CCALLSFSUB1(IPFOUT,ipfout,FLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : ipinit.f
------------------------------------------------------------------*/

#define IPINIT(A1,A2,A3)  CCALLSFSUB3(IPINIT,ipinit,INT,INT,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipiout.f
------------------------------------------------------------------*/

#define IPIOUT(A1)  CCALLSFSUB1(IPIOUT,ipiout,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ipjout.f
------------------------------------------------------------------*/

#define IPJOUT(A1,A2)  CCALLSFSUB2(IPJOUT,ipjout,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ipl.f
------------------------------------------------------------------*/

#define IPL(A1,A2,A3)  CCALLSFSUB3(IPL,ipl,INT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipl3.f
------------------------------------------------------------------*/

#define IPL3(A1,A2,A3,A4)  CCALLSFSUB4(IPL3,ipl3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ipltyp.f
------------------------------------------------------------------*/

#define IPLTYP(A1)  CCALLSFSUB1(IPLTYP,ipltyp,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iplwid.f
------------------------------------------------------------------*/

#define IPLWID(A1)  CCALLSFSUB1(IPLWID,iplwid,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ipm.f
------------------------------------------------------------------*/

#define IPM(A1,A2,A3)  CCALLSFSUB3(IPM,ipm,INT,FLOAT,FLOAT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipm3.f
------------------------------------------------------------------*/

#define IPM3(A1,A2,A3,A4)  CCALLSFSUB4(IPM3,ipm3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : ipm3id.f
------------------------------------------------------------------*/

#define IPM3ID(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IPM3ID,ipm3id,INT,FLOATV,FLOATV,FLOATV,INT,INTV,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : ipm4id.f
------------------------------------------------------------------*/

#define IPM4ID(A1,A2,A3,A4,A5,A6,A7,A8,A9)  CCALLSFSUB9(IPM4ID,ipm4id,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOAT,FLOAT,INT,INTV,A1,A2,A3,A4,A5,A6,A7,A8,A9)

/*------------------------------------------------------------------
fortran filename   : ipmark.f
------------------------------------------------------------------*/

#define IPMARK() CCALLSFSUB0(IPMARK,ipmark)

/*------------------------------------------------------------------
fortran filename   : ipmid.f
------------------------------------------------------------------*/

#define IPMID(A1,A2,A3,A4,A5)  CCALLSFSUB5(IPMID,ipmid,INT,FLOATV,FLOATV,INT,INTV,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ippstr.f
------------------------------------------------------------------*/
/*
#define ippstr_ELEMS_1          ZTRINGV_NUM(1)
#define ippstr_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IPPSTR(A1)  CCALLSFSUB1(IPPSTR,ippstr,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : iprng.f
------------------------------------------------------------------*/

#define IPRNG(A1,A2)  CCALLSFSUB2(IPRNG,iprng,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : ipscol.f
------------------------------------------------------------------*/

#define IPSCOL(A1)  CCALLSFSUB1(IPSCOL,ipscol,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ipspec.f
------------------------------------------------------------------*/

#define IPSPEC() CCALLSFSUB0(IPSPEC,ipspec)

/*------------------------------------------------------------------
fortran filename   : iptext.f
------------------------------------------------------------------*/
/*
#define iptext_ELEMS_3          ZTRINGV_NUM(1)
#define iptext_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IPTEXT(A1,A2,A3)  CCALLSFSUB3(IPTEXT,iptext,FLOAT,FLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : iptlin.f
------------------------------------------------------------------*/

#define IPTLIN(A1,A2,A3)  CCALLSFSUB3(IPTLIN,iptlin,BYTE,INT,INT,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : iptmac.f
------------------------------------------------------------------*/

#define IPTMAC() CCALLSFSUB0(IPTMAC,iptmac)

/*------------------------------------------------------------------
fortran filename   : ipttex.f
------------------------------------------------------------------*/
/*
#define ipttex_ELEMS_3          ZTRINGV_NUM(1)
#define ipttex_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IPTTEX(A1,A2,A3)  CCALLSFSUB3(IPTTEX,ipttex,FLOAT,FLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : ipzone.f
------------------------------------------------------------------*/

#define IPZONE() CCALLSFSUB0(IPZONE,ipzone)

/*------------------------------------------------------------------
fortran filename   : irqlc.f
------------------------------------------------------------------*/

#define IRQLC(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IRQLC,irqlc,INT,INT,INT,INT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : irqst.f
------------------------------------------------------------------*/

#define IRQST(A1,A2,A3,A4,A5)  CCALLSFSUB5(IRQST,irqst,INT,INT,INT,INT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : ischh.f
------------------------------------------------------------------*/

#define ISCHH(A1)  CCALLSFSUB1(ISCHH,ischh,FLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : ischup.f
------------------------------------------------------------------*/

#define ISCHUP(A1,A2)  CCALLSFSUB2(ISCHUP,ischup,FLOAT,FLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : isclip.f
------------------------------------------------------------------*/

#define ISCLIP(A1)  CCALLSFSUB1(ISCLIP,isclip,INT,A1)

/*------------------------------------------------------------------
fortran filename   : iscr.f
------------------------------------------------------------------*/

#define ISCR(A1,A2,A3,A4,A5)  CCALLSFSUB5(ISCR,iscr,INT,INT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : iselnt.f
------------------------------------------------------------------*/

#define ISELNT(A1)  CCALLSFSUB1(ISELNT,iselnt,INT,A1)

/*------------------------------------------------------------------
fortran filename   : isfaci.f
------------------------------------------------------------------*/

#define ISFACI(A1)  CCALLSFSUB1(ISFACI,isfaci,INT,A1)

/*------------------------------------------------------------------
fortran filename   : isfais.f
------------------------------------------------------------------*/

#define ISFAIS(A1)  CCALLSFSUB1(ISFAIS,isfais,INT,A1)

/*------------------------------------------------------------------
fortran filename   : isfasi.f
------------------------------------------------------------------*/

#define ISFASI(A1)  CCALLSFSUB1(ISFASI,isfasi,INT,A1)

/*------------------------------------------------------------------
fortran filename   : isln.f
------------------------------------------------------------------*/

#define ISLN(A1)  CCALLSFSUB1(ISLN,isln,INT,A1)

/*------------------------------------------------------------------
fortran filename   : islwsc.f
------------------------------------------------------------------*/

#define ISLWSC(A1)  CCALLSFSUB1(ISLWSC,islwsc,FLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : ismk.f
------------------------------------------------------------------*/

#define ISMK(A1)  CCALLSFSUB1(ISMK,ismk,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ismksc.f
------------------------------------------------------------------*/

#define ISMKSC(A1)  CCALLSFSUB1(ISMKSC,ismksc,FLOAT,A1)

/*------------------------------------------------------------------
fortran filename   : isplci.f
------------------------------------------------------------------*/

#define ISPLCI(A1)  CCALLSFSUB1(ISPLCI,isplci,INT,A1)

/*------------------------------------------------------------------
fortran filename   : ispmci.f
------------------------------------------------------------------*/

#define ISPMCI(A1)  CCALLSFSUB1(ISPMCI,ispmci,INT,A1)

/*------------------------------------------------------------------
fortran filename   : istxal.f
------------------------------------------------------------------*/

#define ISTXAL(A1,A2)  CCALLSFSUB2(ISTXAL,istxal,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : istxci.f
------------------------------------------------------------------*/

#define ISTXCI(A1)  CCALLSFSUB1(ISTXCI,istxci,INT,A1)

/*------------------------------------------------------------------
fortran filename   : istxfp.f
------------------------------------------------------------------*/

#define ISTXFP(A1,A2)  CCALLSFSUB2(ISTXFP,istxfp,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : isvp.f
------------------------------------------------------------------*/

#define ISVP(A1,A2,A3,A4,A5)  CCALLSFSUB5(ISVP,isvp,INT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : iswkvp.f
------------------------------------------------------------------*/

#define ISWKVP(A1,A2,A3,A4,A5)  CCALLSFSUB5(ISWKVP,iswkvp,INT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : iswkwn.f
------------------------------------------------------------------*/

#define ISWKWN(A1,A2,A3,A4,A5)  CCALLSFSUB5(ISWKWN,iswkwn,INT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : iswn.f
------------------------------------------------------------------*/

#define ISWN(A1,A2,A3,A4,A5)  CCALLSFSUB5(ISWN,iswn,INT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : itx.f
------------------------------------------------------------------*/

/*  f2h gives wrong result here:
#define ITX(A1,A2,A3)  CCALLSFSUB3(ITX,itx,FLOAT,FLOAT,FLOAT,A1,A2,A3)
*/
#define ITX(A1,A2,A3)  CCALLSFSUB3(ITX,itx,FLOAT,FLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : itx3.f
------------------------------------------------------------------*/
/*
#define itx3_ELEMS_4          ZTRINGV_NUM(1)
#define itx3_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define ITX3(A1,A2,A3,A4)  CCALLSFSUB4(ITX3,itx3,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : iuwk.f
------------------------------------------------------------------*/

#define IUWK(A1,A2)  CCALLSFSUB2(IUWK,iuwk,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izarc.f
------------------------------------------------------------------*/

#define IZARC(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IZARC,izarc,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : izatt.f
------------------------------------------------------------------*/

#define IZATT(A1)  CCALLSFSUB1(IZATT,izatt,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izaxis.f
------------------------------------------------------------------*/

#define IZAXIS(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IZAXIS,izaxis,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,INT,INTV,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : izbox.f
------------------------------------------------------------------*/

#define IZBOX(A1,A2,A3,A4)  CCALLSFSUB4(IZBOX,izbox,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izbrak.f
------------------------------------------------------------------*/
/*
#define izbrak_ELEMS_2          ZTRINGV_NUM(1)
#define izbrak_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IZBRAK(A1,A2)  CCALLSFSUB2(IZBRAK,izbrak,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izcda.f
------------------------------------------------------------------*/

#define IZCDA(A1)  CCALLSFSUB1(IZCDA,izcda,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izcfa.f
------------------------------------------------------------------*/

#define IZCFA(A1,A2)  CCALLSFSUB2(IZCFA,izcfa,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izclrd.f
------------------------------------------------------------------*/

#define IZCLRD() CCALLSFSUB0(IZCLRD,izclrd)

/*------------------------------------------------------------------
fortran filename   : izcopy.f
------------------------------------------------------------------*/
/*
#define izcopy_ELEMS_1          ZTRINGV_NUM(1)
#define izcopy_ELEMLEN_1        ZTRINGV_NUM(255)
#define izcopy_ELEMS_2          ZTRINGV_NUM(1)
#define izcopy_ELEMLEN_2        ZTRINGV_NUM(255)
#define izcopy_ELEMS_3          ZTRINGV_NUM(1)
#define izcopy_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IZCOPY(A1,A2,A3)  CCALLSFSUB3(IZCOPY,izcopy,STRING,STRING,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izctoi.f
------------------------------------------------------------------*/
/*
#define izctoi_ELEMS_1          ZTRINGV_NUM(1)
#define izctoi_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZCTOI(A1,A2)  CCALLSFSUB2(IZCTOI,izctoi,STRING,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izctor.f
------------------------------------------------------------------*/
/*
#define izctor_ELEMS_1          ZTRINGV_NUM(1)
#define izctor_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZCTOR(A1,A2)  CCALLSFSUB2(IZCTOR,izctor,STRING,PFLOAT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izdatt.f
------------------------------------------------------------------*/

#define IZDATT(A1,A2)  CCALLSFSUB2(IZDATT,izdatt,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izdipi.f
------------------------------------------------------------------*/

#define IZDIPI(A1)  CCALLSFSUB1(IZDIPI,izdipi,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izdnb.f
------------------------------------------------------------------*/

#define IZDNB(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IZDNB,izdnb,INT,INT,INT,INT,INT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : izend.f
------------------------------------------------------------------*/

#define IZEND() CCALLSFSUB0(IZEND,izend)

/*------------------------------------------------------------------
fortran filename   : izfa.f
------------------------------------------------------------------*/

#define IZFA(A1,A2,A3)  CCALLSFSUB3(IZFA,izfa,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izfa3.f
------------------------------------------------------------------*/

#define IZFA3(A1,A2,A3,A4)  CCALLSFSUB4(IZFA3,izfa3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izfbox.f
------------------------------------------------------------------*/

#define IZFBOX(A1,A2,A3,A4,A5,A6,A7,A8)  CCALLSFSUB8(IZFBOX,izfbox,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5,A6,A7,A8)

/*------------------------------------------------------------------
fortran filename   : izfile.f
------------------------------------------------------------------*/
/*
#define izfile_ELEMS_2          ZTRINGV_NUM(1)
#define izfile_ELEMLEN_2        ZTRINGV_NUM(255)
#define izfile_ELEMS_3          ZTRINGV_NUM(1)
#define izfile_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IZFILE(A1,A2,A3)  CCALLSFSUB3(IZFILE,izfile,INT,STRING,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izgadr.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN2(INT,IZGADR,izgadr,INT,INT)
#define IZGADR(A2,A3)  CCALLSFFUN2(IZGADR,izgadr,INT,INT,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izgcod.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN2(INT,IZGCOD,izgcod,INT,INT)
#define IZGCOD(A2,A3)  CCALLSFFUN2(IZGCOD,izgcod,INT,INT,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izged.f
------------------------------------------------------------------*/
/*
#define izged_ELEMS_1          ZTRINGV_NUM(1)
#define izged_ELEMLEN_1        ZTRINGV_NUM(255)
#define izged_ELEMS_2          ZTRINGV_NUM(1)
#define izged_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IZGED(A1,A2)  CCALLSFSUB2(IZGED,izged,STRING,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izghnp.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,IZGHNP,izghnp,INT)
#define IZGHNP(A2)  CCALLSFFUN1(IZGHNP,izghnp,INT,A2)

/*------------------------------------------------------------------
fortran filename   : izgngt.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN2(INT,IZGNGT,izgngt,INT,INT)
#define IZGNGT(A2,A3)  CCALLSFFUN2(IZGNGT,izgngt,INT,INT,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izgnt.f
------------------------------------------------------------------*/

#define IZGNT(A1,A2,A3,A4,A5)  CCALLSFSUB5(IZGNT,izgnt,INT,PINT,PINT,PINT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izgntp.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN1(INT,IZGNTP,izgntp,INT)
#define IZGNTP(A2)  CCALLSFFUN1(IZGNTP,izgntp,INT,A2)

/*------------------------------------------------------------------
fortran filename   : izgrap.f
------------------------------------------------------------------*/

#define IZGRAP(A1,A2,A3,A4,A5)  CCALLSFSUB5(IZGRAP,izgrap,INT,FLOATV,FLOATV,INTV,INT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izhist.f
------------------------------------------------------------------*/

#define IZHIST(A1,A2,A3,A4)  CCALLSFSUB4(IZHIST,izhist,INT,FLOATV,FLOATV,INTV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izin.f
------------------------------------------------------------------*/
/*
#define izin_ELEMS_1          ZTRINGV_NUM(1)
#define izin_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZIN(A1,A2)  CCALLSFSUB2(IZIN,izin,PSTRING,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izincf.f
------------------------------------------------------------------*/

#define IZINCF(A1)  CCALLSFSUB1(IZINCF,izincf,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izinci.f
------------------------------------------------------------------*/

#define IZINCI(A1)  CCALLSFSUB1(IZINCI,izinci,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izincs.f
------------------------------------------------------------------*/

#define IZINCS(A1)  CCALLSFSUB1(IZINCS,izincs,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izinit.f
------------------------------------------------------------------*/

#define IZINIT(A1)  CCALLSFSUB1(IZINIT,izinit,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izitoc.f
------------------------------------------------------------------*/
/*
#define izitoc_ELEMS_2          ZTRINGV_NUM(1)
#define izitoc_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IZITOC(A1,A2)  CCALLSFSUB2(IZITOC,izitoc,INT,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izlbl.f
------------------------------------------------------------------*/

#define IZLBL() CCALLSFSUB0(IZLBL,izlbl)

/*------------------------------------------------------------------
fortran filename   : izmerg.f
------------------------------------------------------------------*/
/*
#define izmerg_ELEMS_1          ZTRINGV_NUM(1)
#define izmerg_ELEMLEN_1        ZTRINGV_NUM(255)
#define izmerg_ELEMS_5          ZTRINGV_NUM(1)
#define izmerg_ELEMLEN_5        ZTRINGV_NUM(255)
*/

#define IZMERG(A1,A2,A3,A4,A5)  CCALLSFSUB5(IZMERG,izmerg,STRING,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izml.f
------------------------------------------------------------------*/

#define IZML(A1,A2,A3)  CCALLSFSUB3(IZML,izml,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izml3.f
------------------------------------------------------------------*/

#define IZML3(A1,A2,A3,A4)  CCALLSFSUB4(IZML3,izml3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izopen.f
------------------------------------------------------------------*/
/*
#define izopen_ELEMS_2          ZTRINGV_NUM(1)
#define izopen_ELEMLEN_2        ZTRINGV_NUM(255)
#define izopen_ELEMS_3          ZTRINGV_NUM(1)
#define izopen_ELEMLEN_3        ZTRINGV_NUM(255)
#define izopen_ELEMS_4          ZTRINGV_NUM(1)
#define izopen_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IZOPEN(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IZOPEN,izopen,INT,STRING,STRING,STRING,INT,INT,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : izout.f
------------------------------------------------------------------*/
/*
#define izout_ELEMS_1          ZTRINGV_NUM(1)
#define izout_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZOUT(A1,A2)  CCALLSFSUB2(IZOUT,izout,STRING,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izpick.f
------------------------------------------------------------------*/
/*
#define izpick_ELEMS_2          ZTRINGV_NUM(1)
#define izpick_ELEMLEN_2        ZTRINGV_NUM(255)
#define izpick_ELEMS_4          ZTRINGV_NUM(1)
#define izpick_ELEMLEN_4        ZTRINGV_NUM(255)
*/

#define IZPICK(A1,A2,A3,A4)  CCALLSFSUB4(IZPICK,izpick,PINT,PSTRING,PINT,STRING,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izpict.f
------------------------------------------------------------------*/
/*
#define izpict_ELEMS_1          ZTRINGV_NUM(1)
#define izpict_ELEMLEN_1        ZTRINGV_NUM(255)
#define izpict_ELEMS_2          ZTRINGV_NUM(1)
#define izpict_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IZPICT(A1,A2)  CCALLSFSUB2(IZPICT,izpict,PSTRING,STRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izpiwi.f
------------------------------------------------------------------*/
/*
#define izpiwi_ELEMS_1          ZTRINGV_NUM(1)
#define izpiwi_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZPIWI(A1)  CCALLSFSUB1(IZPIWI,izpiwi,STRING,A1)

/*------------------------------------------------------------------
fortran filename   : izpkfa.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN5(INT,IZPKFA,izpkfa,FLOAT,FLOAT,INT,FLOATV,FLOATV)
#define IZPKFA(A2,A3,A4,A5,A6)  CCALLSFFUN5(IZPKFA,izpkfa,FLOAT,FLOAT,INT,FLOATV,FLOATV,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : izpkpl.f
------------------------------------------------------------------*/

 PROTOCCALLSFFUN5(INT,IZPKPL,izpkpl,FLOAT,FLOAT,INT,FLOATV,FLOATV)
#define IZPKPL(A2,A3,A4,A5,A6)  CCALLSFFUN5(IZPKPL,izpkpl,FLOAT,FLOAT,INT,FLOATV,FLOATV,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : izpl.f
------------------------------------------------------------------*/

#define IZPL(A1,A2,A3)  CCALLSFSUB3(IZPL,izpl,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izpl3.f
------------------------------------------------------------------*/

#define IZPL3(A1,A2,A3,A4)  CCALLSFSUB4(IZPL3,izpl3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izpm.f
------------------------------------------------------------------*/

#define IZPM(A1,A2,A3)  CCALLSFSUB3(IZPM,izpm,INT,FLOATV,FLOATV,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : izpm3.f
------------------------------------------------------------------*/

#define IZPM3(A1,A2,A3,A4)  CCALLSFSUB4(IZPM3,izpm3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)

/*------------------------------------------------------------------
fortran filename   : izpush.f
------------------------------------------------------------------*/
/*
#define izpush_ELEMS_5          ZTRINGV_NUM(1)
#define izpush_ELEMLEN_5        ZTRINGV_NUM(255)
*/

 PROTOCCALLSFFUN4(INT,IZPUSH,izpush,INT,INT,INT,STRING)
#define IZPUSH(A2,A3,A4,A5)  CCALLSFFUN4(IZPUSH,izpush,INT,INT,INT,STRING,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izrpip.f
------------------------------------------------------------------*/
/*
#define izrpip_ELEMS_2          ZTRINGV_NUM(1)
#define izrpip_ELEMLEN_2        ZTRINGV_NUM(255)
*/

 PROTOCCALLSFFUN1(INT,IZRPIP,izrpip,STRING)
#define IZRPIP(A2)  CCALLSFFUN1(IZRPIP,izrpip,STRING,A2)

/*------------------------------------------------------------------
fortran filename   : izrtoc.f
------------------------------------------------------------------*/
/*
#define izrtoc_ELEMS_2          ZTRINGV_NUM(1)
#define izrtoc_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#define IZRTOC(A1,A2)  CCALLSFSUB2(IZRTOC,izrtoc,FLOAT,PSTRING,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izsav.f
------------------------------------------------------------------*/

#define IZSAV() CCALLSFSUB0(IZSAV,izsav)

/*------------------------------------------------------------------
fortran filename   : izsava.f
------------------------------------------------------------------*/

#define IZSAVA() CCALLSFSUB0(IZSAVA,izsava)

/*------------------------------------------------------------------
fortran filename   : izscan.f
------------------------------------------------------------------*/

#define IZSCAN(A1,A2,A3,A4,A5)  CCALLSFSUB5(IZSCAN,izscan,INT,INT,INT,INT,PINT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izscli.f
------------------------------------------------------------------*/

#define IZSCLI(A1)  CCALLSFSUB1(IZSCLI,izscli,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izscor.f
------------------------------------------------------------------*/

#define IZSCOR(A1,A2,A3,A4,A5)  CCALLSFSUB5(IZSCOR,izscor,INT,INT,FLOAT,FLOAT,FLOAT,A1,A2,A3,A4,A5)

/*------------------------------------------------------------------
fortran filename   : izscpi.f
------------------------------------------------------------------*/

#define IZSCPI(A1)  CCALLSFSUB1(IZSCPI,izscpi,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izscr.f
------------------------------------------------------------------*/
/*
#define izscr_ELEMS_1          ZTRINGV_NUM(1)
#define izscr_ELEMLEN_1        ZTRINGV_NUM(255)
*/

#define IZSCR(A1,A2)  CCALLSFSUB2(IZSCR,izscr,STRING,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : izseln.f
------------------------------------------------------------------*/

#define IZSELN(A1)  CCALLSFSUB1(IZSELN,izseln,INT,A1)

/*------------------------------------------------------------------
fortran filename   : izsenv.f
------------------------------------------------------------------*/

#define IZSENV() CCALLSFSUB0(IZSENV,izsenv)

/*------------------------------------------------------------------
fortran filename   : izset.f
------------------------------------------------------------------*/

#define IZSET() CCALLSFSUB0(IZSET,izset)

/*------------------------------------------------------------------
fortran filename   : izseta.f
------------------------------------------------------------------*/

#define IZSETA() CCALLSFSUB0(IZSETA,izseta)

/*------------------------------------------------------------------
fortran filename   : izstcc.f
------------------------------------------------------------------*/

#define IZSTCC(A1,A2)  CCALLSFSUB2(IZSTCC,izstcc,INT,INT,A1,A2)

/*------------------------------------------------------------------
fortran filename   : iztabl.f
------------------------------------------------------------------*/

#define IZTABL(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IZTABL,iztabl,INT,INT,PFLOAT,INT,FLOATV,INTV,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : iztext.f
------------------------------------------------------------------*/
/*
#define iztext_ELEMS_3          ZTRINGV_NUM(1)
#define iztext_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IZTEXT(A1,A2,A3,A4,A5,A6)  CCALLSFSUB6(IZTEXT,iztext,FLOAT,FLOAT,STRING,FLOAT,FLOAT,INTV,A1,A2,A3,A4,A5,A6)

/*------------------------------------------------------------------
fortran filename   : iztx.f
------------------------------------------------------------------*/
/*
#define iztx_ELEMS_3          ZTRINGV_NUM(1)
#define iztx_ELEMLEN_3        ZTRINGV_NUM(255)
*/

#define IZTX(A1,A2,A3)  CCALLSFSUB3(IZTX,iztx,FLOAT,FLOAT,STRING,A1,A2,A3)

/*------------------------------------------------------------------
fortran filename   : iztx3.f
------------------------------------------------------------------*/
/*
#define iztx3_ELEMS_7          ZTRINGV_NUM(1)
#define iztx3_ELEMLEN_7        ZTRINGV_NUM(255)
*/

#define IZTX3(A1,A2,A3,A4,A5,A6,A7)  CCALLSFSUB7(IZTX3,iztx3,FLOAT,FLOAT,FLOAT,FLOATV,FLOATV,FLOATV,STRING,A1,A2,A3,A4,A5,A6,A7)

/*------------------------------------------------------------------
fortran filename   : izundo.f
------------------------------------------------------------------*/

#define IZUNDO() CCALLSFSUB0(IZUNDO,izundo)

/*------------------------------------------------------------------
fortran filename   : izwip.f
------------------------------------------------------------------*/

#define IZWIP(A1)  CCALLSFSUB1(IZWIP,izwip,INT,A1)

