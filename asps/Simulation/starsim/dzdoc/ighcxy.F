*
* $Id: ighcxy.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: ighcxy.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:25  nevski
*  first working release
*
* Revision 1.1  2000/01/06 14:57:43  nevski
* protect histogram link against I->R->I corruption
*
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "hbook/pilot.h"
*CMZ :  1.22/09 09/03/95  15.49.40  by  O.Couet
*-- Author :
      REAL FUNCTION IGHCXY(LSCAI,ICX,ICY,IOPT)
*.==========>
*.
*.   Returns the value of cell defined by ICX and ICY if IOPT=1. Returns
*. the error associated with the cell defined by ICX and ICY if IOPT=2.
*.
*..=========> (O.Couet)
* #include "higz/hipaw.inc"

      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,             LMAIN, IPAW(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)
      COMMON /QUEST/ IQUEST(100)
      DIMENSION RQUEST(100)
      EQUIVALENCE (IQUEST(1),RQUEST(1))
      COMMON /HILINK/ LHIGZ,LCG,LWV
     +,               LPICT,LHNT0,LHI,LHF,LHC,LHNT,LHNTLA
     +,               LPICD,LDNT0,LDI,LDF,LDC,LDNT,LDNTLA
     +,               LPIC2,L2NT0,L2I,L2F,L2C,L2NT,L2NTLA
     +,               LNDECO,LIDECO,LFDECO,LCDECO
     +,               LPSAV,LPCOP,LNTCOP,LSTACK(10),LAST
      COMMON /HIPTR/  ITBPTR,INTPTR,IFLPTR,ISTPTR
     +,               IIAPTR,IRAPTR
      COMMON /HISIZE/ INSIZ,IISIZ,IFSIZ,ISSIZ
     +,               IRNSIZ,IRISIZ,IRFSIZ,IRSSIZ
      COMMON /HINUM/  NBPICT,INBNT,INBLEV

* #include "higz/hipack.inc"

      COMMON /HIPACK/ NCX,NCY,IPACK,ILOG,ZLOW,ZHIGH,IDIM1,IOPTER
*
*PN, 06.01.00: Restore LSCA if it was corrupted by I->R->I conversion
* #include "hbook/hcbook.inc"
C ... partially:
      INTEGER       HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM,
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM(9),
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*.___________________________________________
*
      PARAMETER(KNBIT=1,KCON2=3,KCON1=9)
*
*      IF (LSCAI.NE.LSCAT) then
*         LSCB=FLOAT(LSCAT)
*         IF (LSCAI.NE.LSCB)  print *,' IGHCXY: suspecious ',
*     *            'LSCAT,LSCB,LSCAI =',LSCAT,LSCB,LSCAI
*      ENDIF
*
      NB = IQ(LSCAT+KNBIT)
      IF (NB.LE.0) NB = 32
      NW = 32/NB
*
      IF (IDIM1.NE.0) THEN
*
*              1D histos
*
         IF (NW.NE.1) THEN
            L1     = ICX*NB
            NBITH  = 32-MOD(32,NB)
            L2     = MOD(L1,NBITH)+1
            L1     = LSCAT+KCON1+L1/NBITH
            IGHCXY = JBYT(IQ(L1),L2,NB)
         ELSE
            IGHCXY = Q(LSCAT+KCON1+ICX)
         ENDIF
*
* If IOPT=2, then get error
*
         IF(IOPT.EQ.2) THEN
            LW = LQ(LSCAT)
            IF (LW.NE.0) THEN
*
* Error is packed in histogram
*
               IGHCXY = SQRT(Q(LW+ICX))
            ELSE
*
* Error must be calculated as SQRT contents
*
               IGHCXY = SQRT(ABS(IGHCXY))
            ENDIF
         ENDIF
      ELSE
*
*              2D histos
*
* First get contents
*
         J  = (NCY-ICY+1)*(NCX+2)
         L2 = ICX+J
         L1 = L2/NW+LSCAT+KCON2
         IF (NW.NE.1) THEN
            L2     = (NW-1-MOD(L2,NW))*NB +1
            IGHCXY = JBYT(IQ(L1),L2,NB)
         ELSE
            IGHCXY = Q(L1)
         ENDIF
*
* If IOPT=2, then get error
*
         IF(IOPT.EQ.2) THEN
            LW = LQ(LSCAT)
            IF (LW.NE.0) THEN
*
* Error is packed in histogram
*
               IOFF = (ICY-1)*NCX+ICX
               IGHCXY = SQRT(Q(LW+IOFF))
            ELSE
*
* Error must be calculated as SQRT contents
*
               IGHCXY = SQRT(ABS(IGHCXY))
            ENDIF
         ENDIF
      ENDIF
*
      END
