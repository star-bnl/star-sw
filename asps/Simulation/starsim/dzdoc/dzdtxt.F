* $Id: dzdtxt.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzdtxt.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:10  nevski
* dzdoc for agi
*
      SUBROUTINE DZDTXT(CHT,NCM,X0,Y0,CS,ANG,IALG)
*.
*...DZDTXT   output a text string
*.
*. INPUT     :  CHT     string containing the text
*.              NCM     max number of characters to be printed
*.              X0,Y0   position of the text
*.              CS      character size
*.              ANG     angle
*.              IALG    alignment (1 left, 2 centered, 3 right)
*.
*. CALLS     : DZDROT GSCHH  GSCHUP GSTXAL GTX
*. CALLED    :  many
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. Modification Log.
*. 20-Feb-90 : O.Schaile
*.             add LATEX
*. 12-Dec-90 : O.Schaile
*.             add PostScript
*. 07-Dec-99 : PN: always write to screen after Latex or PS
*.**********************************************************************
*.
#include "dzedit/dzdprm.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER*(*) CHT
      CHARACTER*4 CCENT(3)
      CHARACTER*24 CCINT
      CHARACTER*128 CLINEI
      REAL CTXFP(2)
      LOGICAL NONZAN
      DATA CCENT/'[bl]','[c]','[br]'/
*
      NC = LENOCC(CHT)
      IF(NCM.GT.0)NC = MIN(NC,NCM)
      NC=MIN(NC,132)
      ILC = INDXBC(CHT(1:NC),' ')
      IF(ILC.NE.0)NC=ILC
      IF(IFOMED.EQ.3)THEN
          CLINEI(1:NC)=CHT(1:NC)
          IF(IALG.LT.1 .OR. IALG.GT. 3)THEN
              IALGI=1
          ELSE
              IALGI=IALG
          ENDIF
          XT=X0*PAGECM
          YT=Y0*PAGECM
          WRITE(LUNGRA,'(A,2(F5.2,A),A,A)')
     &    BS//'p(', XT, ',', YT, '){'//BS//'m(0,0)',
     &    CCENT(IALGI), '{'
          WRITE(LUNGRA,'(A)')CLINEI(1:NC)
          WRITE(LUNGRA,'(A)')'}}'
          GOTO 999
      ELSEIF(IFOMED.EQ.2)THEN
*     PostScript, check for ( )
          IOUT=0
          DO 10 I=1,NC
             IOUT=IOUT+1
             IF(CHT(I:I).EQ.'(' .OR. CHT(I:I).EQ.')')THEN
               CLINEI(IOUT:IOUT)=BS
               IOUT=IOUT+1
             ENDIF
             CLINEI(IOUT:IOUT)=CHT(I:I)
10        CONTINUE
          NC=IOUT
          IPCS=CS*PAMM10*1.1
          IF(IPCS.EQ.0)IPCS=40
          IF(IPCS.NE.IPCSIZ)THEN
             IPCSIZ=IPCS
             WRITE(CCINT,'(A,I4,A)')'H ',IPCS,' sf '
             CALL DZDPLN(LUNGRA,CCINT,-1)
          ENDIF
          CALL DZDPLN(LUNGRA,' n',-1)
          IX=X0*PAMM10
          IY=Y0*PAMM10
          IF(IALG.EQ.2)IY=IY-IPCSIZ/2
          CCINT=' '
          WRITE(CCINT,'(2I8)')IX,IY
          IF(IALG.EQ.2 .OR. IALG.EQ.3)THEN
             CCINT(18:18)='0'
          ELSE
             CCINT(18:18)='m'
          ENDIF
          CALL DZDPLN(LUNGRA,CCINT,-1)
          CALL DZDPLN(LUNGRA,'(',0)
          CALL DZDPLN(LUNGRA,CLINEI(1:NC),0)
          CALL DZDPLN(LUNGRA,')',0)
          IF(IALG.EQ.2)THEN
             CALL DZDPLN(LUNGRA,' cs ',0)
          ELSEIF(IALG.EQ.3)THEN
             CALL DZDPLN(LUNGRA,' rs ',0)
          ELSE
             CALL DZDPLN(LUNGRA,' sh ',0)
          ENDIF
          GOTO 999
      ENDIF

999   continue

      XT = 0.
      YT = 0.
      CTXFP(2)=-999.
      CLINEI(1:NC)=CHT(1:NC)
      IF(INDEX(CLINEI(1:NC),'[').NE.0 .OR.
     &   INDEX(CLINEI(1:NC),'"').NE.0 .OR.
     &   INDEX(CLINEI(1:NC),'^').NE.0 .OR.
     &   INDEX(CLINEI(1:NC),'?').NE.0)THEN
         CALL IGQ('TXFP',CTXFP)
         IFONT=-ABS(CTXFP(1))
         CALL ISTXFP(IFONT,1)
*         CALL IGSET('TXFP',10.*CTXFP(1)+1.)
         CALL IGSET('PASS',3.)
      ENDIF
#if defined(CERNLIB_GKS)
      CALL GSCHH(CS)
      IF(IALG .EQ. 1)CALL GSTXAL(0,0)
      IF(IALG .EQ. 2)CALL GSTXAL(2,3)
      IF(IALG .EQ. 3)CALL GSTXAL(3,0)
#endif
#if defined(CERNLIB_HIGZ)
      CALL ISCHH(CS)
      IF(IALG .EQ. 1)CALL ISTXAL(0,0)
      IF(IALG .EQ. 2)CALL ISTXAL(2,3)
      IF(IALG .EQ. 3)CALL ISTXAL(3,0)
#endif
      IF(ABS(ANG) .GT. 1.E-10)THEN
        A1 = ANG +90.
        IF(A1 .GT. 360.)A1 = A1-360.
        NONZAN = .TRUE.
        XT = 0.
        YT = 1.
        CALL DZDROT(XT,YT,XT,YT,A1)
#if defined(CERNLIB_GKS)
        CALL GSCHUP(XT,YT)
#endif
#if defined(CERNLIB_HIGZ)
        CALL ISCHUP(XT,YT)
#endif
      ELSE
        NONZAN = .FALSE.
      ENDIF
*
*
      IF(NONZAN)THEN
        CALL DZDROT(XT,YT,XTT,YTT,ANG)
        XTT=XTT+X0
        YTT=YTT+Y0
      ELSE
        XTT = XT+X0
        YTT = YT+Y0
      ENDIF
*
#if defined(CERNLIB_GKS)
      IF(NC.GT.0)CALL GTX(XTT,YTT,CLINEI(1:NC))
#endif
#if defined(CERNLIB_HIGZ)
*      WRITE(*,*)' DZDTXT: ',CLINEI(1:NC)
      IF(NC.GT.0)CALL ITX(XTT,YTT,CLINEI(1:NC))
*      CALL IUWK(0,1)
#endif
      IF(NONZAN)THEN
#if defined(CERNLIB_GKS)
        CALL GSCHUP(0.,1.)
#endif
#if defined(CERNLIB_HIGZ)
        CALL ISCHUP(0.,1.)
#endif
      ENDIF
      IF(CTXFP(2).GT.-999.)THEN
         CALL IGSET('TXFP',10.*CTXFP(1)+CTXFP(2))
      ENDIF

      END
************************************************************************
