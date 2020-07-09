*
* $Id: cspar.inc,v 1.1.4.1 2020/07/09 19:44:07 perev Exp $
*
* $Log: cspar.inc,v $
* Revision 1.1.4.1  2020/07/09 19:44:07  perev
* Add64
*
* Revision 1.1  2018/11/19 20:43:51  perev
* 64bits add files
*
* Revision 1.2  2000/05/30 13:53:58  couet
* - A buffer size increased
*
* Revision 1.1.1.1  1996/02/26 17:16:33  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSPAR_INC
#define CERNLIB_COMIS_CSPAR_INC
*
#ifdef CERNLIB_PAW
!!!#undef CERNLIB_PAW
#endif
*
* cspar.inc
*
        integer LHP,KON1, KON2, KON3,LRECU,NRECU,MLRECU,MXRECU
     1,         LASTK,LSSTK,KLENID,LCSTK
        PARAMETER (LHP=50006, KON1=17694720, KON2=KON1
     +, KON3=2**16, LRECU=8,NRECU=5,MLRECU=NRECU*LRECU
     +, MXRECU=MLRECU-LRECU
     1,      LASTK=2000,LSSTK=50,KLENID=32
     2,      LCSTK=4800)

#endif
