*
* $Id: cskcal.F,v 1.2 2018/11/19 20:44:06 perev Exp $
*
* $Log: cskcal.F,v $
* Revision 1.2  2018/11/19 20:44:06  perev
* 64bits add files
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.1  2003/10/03 13:24:30  nevski
* make comis call re-entrant
*
* Revision 1.2  1996/12/05 09:50:39  berejnoi
* Mods for WINNT: form correct arg. list for chars
*
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 03/11/94  17.17.09  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKCAL(KS)
***--------------------------------------------
*       it is interpreter's command 'call' params.
*       For calls user routine it prepares corresponding
*       actual arguments list. The arg. list is depends
*       from f77 compiler.
*       For apollo and ibm arg list in array IF77PL
*       IBM: BZ00,NPAR*4,aARG1,...,aARGn [, aLEN1...,LEN1,....]
*       APOLLO: aARG1,....,aARGn [,aLEN1,....,LEN1*2**16,.....]
*       For details see on-line comments.
* PN,03.10.03: move pointer update before call as NPAR may not be preserved
***--------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/csfres.inc"
#include "comis/cssysd.inc"
      CHARACTER PRONAME*32
      INTEGER CSCALI
      DOUBLE PRECISION CSCALD
      INTEGER IF77PL(40)
#if defined(CERNLIB_APOLLO)
      INTEGER JNAME,INAME
      POINTER/JNAME/INAME
#endif
#if defined(CERNLIB_IBM)
      DATA IBZ00/4HBZ00/
#endif
      CSKCAL=1
      IP=KD1(IPC)
      NPAR=KD2(IPC)
      IF(IP.LT.0)THEN
        I=IAP-IP
        IP=IA(I)
        IF(IP.LT.0)THEN
          IA(ITA+1)=IFP
          IA(ITA+2)=IAP
          IA(ITA+3)=IBASE
          IA(ITA+4)=IDP
          IA(ITA+5)=IPC
          IA(ITA+6)=ITA
          IA(ITA+7)=NAMEST(1)
          IA(ITA+8)=NAMEST(2)
          IA(ITA+9)=NUMST
          IFP=ITA+1
          IPC=-IP
          ITA=ITA+9
          RETURN
        ENDIF
      ENDIF
      IADGP=IQ(IP+KSADGP)
      IFCS=IQ(IP+KSIFCS)
      IF(IFCS)9202,707,9204
*              F77 -NO- CS
*CALL TO F77 user's routine
 9202  I2=IPC+KS+2
       ITP=IABS(IQ(IP+KSTPGP))
       ITA=ITA-NPAR
       IF(KD(IPC).EQ.87)THEN
         IPC=IPC+NPAR+3
       ELSE
         IPC=IPC+NPAR*3+3
       ENDIF
** BYTE ADDRESS
#if defined(CERNLIB_STDUNIX)
      NOPAR=NPAR
      I=1
      DO 9203 K=ITA+1,ITA+NPAR
*      DO 9203 K=ITAN,IFP-1
        IB1=KD(I2)/100
        IF(IB1.NE.7)THEN
          IT=MOD(KD(I2),100)
          IF(IT.EQ.3)THEN
*** if arg is character then additionel arg. needed
             NOPAR=NOPAR+1
***            puts byte-address of char. var.
             IF77PL(I)=MIWORD(IA(K)+1)
***            puts len of char var
#if defined(CERNLIB_WINNT)
             I=I+1
             IF77PL(I)=MOD(MIWORD(IA(K)),KON3)
#else
             IF77PL(NOPAR)=MOD(MIWORD(IA(K)),KON3)
#endif
           ELSE
!//VP             IF77PL(I)=IA(K)*NBYTPW
             IF77PL(I)=IA(K)
           ENDIF
        ELSE
          IF(IQ(IA(K)+KSIFCS).LT.0)THEN
*  this is an external f77 routine, pass its address
            IF77PL(I)=IQ(IA(K)+KSADGP)
          ELSE
*  this is an external comis routine, pass pointer to cs_routine
            IF77PL(I)=LOCB(IA(K))
          ENDIF
        ENDIF
        I2=I2+KS
        I=I+1
 9203 CONTINUE
*      IT=IABS(ITYPGP)
      IF(ITP.EQ.1.OR.ITP.EQ.4.OR.ITP.EQ.6)THEN
        ICSRES=CSCALI(IADGP,NOPAR,IF77PL(1))
      ELSEIF(ITP.EQ.2)THEN
        RCSRES=CSCALR(IADGP,NOPAR,IF77PL(1))
      ELSEIF(ITP.EQ.5)THEN
        DCSRES=CSCALD(IADGP,NOPAR,IF77PL(1))
      ELSEIF(ITP.EQ.7)THEN
*  I think it will works correctly
        DCSRES=CSCALD(IADGP,NOPAR,IF77PL(1))
      ENDIF
*+SELF,IF=UNIX,IF=-SGI,IF=-IBMRT,IF=-DECS,IF=-HPUX,IF=-SUN,IF=-MSDOS.
#endif
* 9301 CONTINUE
      RETURN
*CALL TO CS routine
 9204 CONTINUE
*      IF(ITA+9.GE.LAST)GO TO 701
*                      ---------
* FRAME::= IFP;IAP;IBASE;IDP;IPCALL;ITA;NAME(2);NUMBER
      IA(ITA+1)=IFP
      IA(ITA+2)=IAP
      IA(ITA+3)=IBASE
      IA(ITA+4)=IDP
      IA(ITA+5)=IPC
      IA(ITA+6)=ITA
      IA(ITA+7)=NAMEST(1)
      IA(ITA+8)=NAMEST(2)
      IA(ITA+9)=NUMST
      IFP=ITA+1
      IAP=ITA-NPAR
      ITA=ITA+9
*      IDGP(1)=IQ(IP+5)
*      IDGP(2)=IQ(IP+6)
      NAMEST(1)=IQ(IP+KSIDP)
      NAMEST(2)=IQ(IP+KSIDP+1)
 9205 IBASE=IADGP
      IPC=IADGP
      RETURN
* 701  CSKCAL=-1
*      RETURN
 707  CSKCAL=-7
      CALL CSGTIDP(IP,PRONAME,NCH)
      PRINT *,' call to missing routine ',PRONAME(:NCH)
      END
