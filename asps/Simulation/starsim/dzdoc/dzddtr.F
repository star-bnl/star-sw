* $Id: dzddtr.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzddtr.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:21  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:23  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:09  nevski
* dzdoc for agi
*
      SUBROUTINE DZDDTR(LUNSGM,LUNLOG,LUNMET,IWMETA,ITMETA,L,CHOPT)
      CHARACTER*(*) CHOPT
      CHARACTER*7 CTEMP
      CHARACTER*4 CHODRW
      CHARACTER*8 CTEMP1
      CHARACTER*80 CLINE
#include "dzedit/dzdprm.inc"
#include "zebra/zunit.inc"
#include "dzdoc/bkwrp.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      LOGICAL DOPDOC, NOTDON
      INTEGER MALD
      PARAMETER (MALD=50)
      INTEGER IPCSEQ,ILABEL, IALD(2,MALD), IPALD, LALD
      DATA IPCSEQ/0/,ILABEL/0/

      PAGECM=14./20.
      IF(INDEX(CHOPT,'L').NE.0)THEN
*        LATEX
         LUNGRA=LUNSGM
         IFOMED=3
      ELSE
*        GKS or HiGZ
         IFOMED=0
#if defined(CERNLIB_GKS)
         CALL GSFASI(-103)
#endif
#if defined(CERNLIB_HIGZ)
         CALL ISFASI(-103)
#endif
      ENDIF
      IF(INDEX(CHOPT,'1').NE.0)THEN
         CHODRW='RT1'
      ELSE
         CHODRW='RT'
      ENDIF
      IF(INDEX(CHOPT,'0').NE.0)ILABEL=0
      YBSING = 0.85
      WRITE(IQPRNT,'(A,A4)') ' Draw tree below: ',IQ(KQSP+L-4)
      NBK = 0
      NBDOCT = 0
      IPALD=0
      LALD=0
      CALL UZERO(IALD,1,2*MALD)
*
*       init links
*
      LORIG = L
      LUP = L
      LFROM = LQ(KQSP+L+1)
      LT = LFROM

*      IPCSEQ=0
*       get next down link

   10   CONTINUE
      JB = LUP - LORIG + 1
      NS = IQ(KQSP+LUP -2)

      IF(JB .EQ. 1)THEN
            ILKACT = 0

   20       CONTINUE
            CALL UHTOC(IQ(L+KQSP-4),4,CTEMP(1:4),4)
            WRITE(CTEMP(5:7),'(I3.3)')IPCSEQ
            CTEMP1(1:4) = CTEMP(1:4)
            ILKSAV = ILKACT
            CALL DZDRA1(0,L,ILKACT,CHODRW,
     &                  0.,0.,.TRUE.,IFLCUR,YTOP,YBOTTM)
            LU = LQ(KQSP+L+1)
            IF(LU .GT. 1)THEN
                  CALL UHTOC(IQ(KQSP+LU-4),4,CTEMP1(5:8),4)
            ELSE
                  CTEMP1(5:8) = 'NONE'
            ENDIF

*            remember complete subtrees
            IF(ILKACT.GT.0)THEN
               ILLAST=ILKACT
            ELSE
               ILLAST=IQ(KQSP+L-2)
            ENDIF
            DO 25 I=ILKSAV+1,ILLAST
               IF(IAND(IQ(LQ(L+KQSP-I)+KQSP),ALDBIT).NE.0)THEN
                  IPALD=IPALD+1
                  IF(IPALD.GT.MALD)IPALD=1
                  IF(LALD.LT.MALD) LALD=LALD+1
                  IALD(1,IPALD) = IQ(LQ(L+KQSP-I)+KQSP-4)
                  IALD(2,IPALD) = IQ(L +KQSP-4)
               ENDIF
25          CONTINUE

*           check for complete subtree
            NOTDON=.TRUE.
            DO 26 I=1,LALD
               IF(IQ(L +KQSP-4).EQ. IALD(1,I) .AND.
     &            IQ(LU+KQSP-4).EQ. IALD(2,I))THEN
                     NOTDON=.FALSE.
                     GOTO 27
               ENDIF
26          CONTINUE
27          CONTINUE

            ILKACT = ILKSAV
*           output part
            IF(  (YBOTTM .LT. YBSING
     &       .OR. ILKACT.GT.0)
     &       .AND.LUNSGM.GT.0
     &       .AND.NOTDON)      THEN
                IPCSEQ=IPCSEQ+1
                IF(IPCSEQ. GT. 999)IPCSEQ=1
                IFBL = INDEX(CTEMP(1:4),' ')
                IF(IFBL .GT. 0 .AND. IFBL .LE. 4)THEN
                   DO 30 I=IFBL,4
   30              CTEMP(I:I)='X'
                ENDIF
                IXMM=140
                IYMM=(1.-YBOTTM)*140.
                WRITE(CLINE,'(A,2I5)')
     +          '%%PICTURE '//CTEMP(1:7),-IXMM,IYMM
                IF(INDEX(CHOPT,'L').EQ.0 .AND.
     &             INDEX(CHOPT,'P').EQ.0)THEN
*                   GKS or HIGZ metafile
                    XAPA = .1465
#if defined(CERNLIB_GKS)
                    IF(INDEX(CHOPT,'N').EQ.0)CALL GACWK(IWMETA)
                    CALL GSWKWN(IWMETA,0.,1.,YBOTTM,1.)
                    CALL GSWKVP(IWMETA,0.,XAPA,0.,XAPA*(1.-YBOTTM))
                    CALL GCLRWK(IWMETA,1)
                    CALL GCNAME(IWMETA,CTEMP(1:7))
#endif
#if defined(CERNLIB_HIGZ)
                   IF(INDEX(CHOPT,'N').EQ.0)CALL IACWK(IWMETA)
                   CALL IGRNG(XAPA*100.,XAPA*100.*(1.-YBOTTM))
*                   CALL ISWKWN(IWMETA,0.,1.,YBOTTM,1.)
*                   CALL ISWKVP(IWMETA,0.,XAPA,0.,XAPA*(1.-YBOTTM))
                   CALL ICLRWK(IWMETA,1)
                   IF(ITMETA.EQ. -111)THEN
                       CALL IPPSTR(CLINE(1:28))
                       CALL IPPSTR('@ ')
                   ENDIF
#endif
                   CALL DZDRAW(0,L,ILKACT,CHODRW)
#if defined(CERNLIB_GKS)
                   IF(INDEX(CHOPT,'N').EQ.0)CALL GDAWK(IWMETA)
#endif
#if defined(CERNLIB_HIGZ)
*                   IF(INDEX(CHOPT,'N').EQ.0)CALL IDAWK(IWMETA)
#endif
                ELSEIF(INDEX(CHOPT,'P').NE.0 .AND.
     &                 LUNMET.GT.0)THEN
                   IFOMED=2
                   PAMM10=73.25
                   IPCSIZ=0
                   LUNGRA=LUNMET
                   CALL DZDPLN(LUNGRA,CLINE,0)
                   CALL DZDPLN(LUNGRA,' ',-99)
                   CALL DZDRAW(0,L,ILKACT,CHODRW)
                   CALL DZDPLN(LUNGRA,' ',-99)
*                   CALL DZDPLN(LUNGRA,' showpage',0)
                   CALL DZDPLN(LUNGRA,' ',-99)
                ENDIF
                IF(LUNSGM .GT. 0)THEN
                    CALL DZDGDS(CTEMP1,CLINE,NLT,NLU)
                    IF(NLT .LE. 0)THEN
                         CLINE = ' '
                         NLT=1
                    ENDIF
                    IF(IFOMED.LE.2)THEN
                        WRITE(LUNSGM,'(A,I3.3,A)') '<FIG ID=T'
     +                  ,IPCSEQ,'>'
                        WRITE(LUNSGM,'(A,A,A)')'<PICTURE NAME='
     +                  ,CTEMP,'S>'
                        WRITE(LUNSGM,'(A,A,A/A)')'<FIGCAP>',CTEMP1(1:4)
     +                  ,'  ',CLINE(1:NLT)
                        WRITE(LUNSGM,'(A)')'</FIG>'
                    ELSEIF(IFOMED.EQ.3)THEN
*                       LATEX
                        IWX=14
                        IWY=(1.-YBOTTM)*14 + 1
                        IYB=14-IWY
                        WRITE(LUNSGM,'(A,3(I2,A)/A)')
     +                  BS//'bp(',IWX,',',IWY,')(0,',IYB,
     +                  ')','{'//BS//'small '//BS//'sf '
                        WRITE(LUNSGM,'(A,I2,A,I2,A)')
     +                  BS//'p(0.1,',IYB,'){'//BS//'f(14,',IWY,
     +                  ')[bl]{'//CTEMP1(1:4)//'}}'
                        CALL DZDRAW(0,L,ILKACT,CHODRW)
                        WRITE(LUNSGM,'(A/A/A)')
     +                  '}',BS//'ep',' '
                    ENDIF
                ENDIF
            ENDIF
*           dont write documentation more than once if:
*           more than one picture needed and for next-banks
*           with the same Holl Id
            LN=LQ(KQSP+L)
            IF(ILKACT.NE.0)THEN
               DOPDOC=.FALSE.
            ELSE
               DOPDOC=.TRUE.
               IF(LN.NE.0)THEN
                  IF(IQ(KQSP+LN-4).EQ.IQ(KQSP+L-4))THEN
                    DOPDOC=.FALSE.
                 ENDIF
               ENDIF
            ENDIF
            IF(DOPDOC)THEN
               IF(LU.EQ.0)THEN
                  IF(IQ(KQSP+L-1).GE.4)THEN
                     CALL UHTOC(IQ(KQSP+L+4),4,CTEMP1(5:8),4)
                  ELSE
                      CTEMP1(5:8)='NONE'
                  ENDIF
               ENDIF
               CALL DZDGDS(CTEMP1,CLINE,NLT,NLU)
               IF(NLT .GT. 0)THEN
                  NBDOCT=NBDOCT+1
                  ILC=INDXBC(CLINE,' ')
               ELSE
                  ILC=0
               ENDIF
               NBK = NBK + 1
               WRITE(LUNLOG,'(A)')CTEMP1
*               IF(LU.EQ.0)CTEMP1(5:8)='****'
*              output part
               IF(LUNSGM.GT.0)THEN
                  IF(INDEX(CHOPT,'L').EQ.0)THEN
                     WRITE(LUNSGM,'(A)')'<LISTING>'
                     CALL DZDOCO(LUNSGM,CTEMP1,'RH')
                  ELSE
                     IF(ILC.GT.0)THEN
                        WRITE(LUNSGM,'(A)')BS//'rule{15cm}{.05cm}'
                        WRITE(LUNSGM,'(A)')' '
                       WRITE(LUNSGM,'(A)')
     +                 '{'//BS//'large '//BS//'bf '//BS//'sf'
                       WRITE(LUNSGM,'(A)')CTEMP1(1:4)//' '
     +                 //CLINE(1:ILC)//'}'
                     ENDIF
                     ILABEL=ILABEL+1
                     WRITE(LUNSGM,'(A,I4.4,A)')BS//'label{',ILABEL,'}'
                     WRITE(LUNSGM,'(A)')BS//'begin{verbatim}'
                     CALL DZDOCO(LUNSGM,CTEMP1,'RN')
                  ENDIF
                  IF(INDEX(CHOPT,'L').EQ.0)THEN
                     WRITE(LUNSGM,'(A)')'</LISTING>'
                     WRITE(LUNSGM,'(A,A)')'<I1>',CTEMP1(1:4)
                  ELSE
                     WRITE(LUNSGM,'(A)')BS//'end{verbatim}'
                  ENDIF
               ENDIF
            ENDIF
            IF(ILKACT .NE. 0)GOTO 20
      ENDIF
*
   40   CONTINUE
      IF(JB .LE. NS)THEN

*             go down

            LD1 = LQ(KQSP+ L - JB)

*             look if this link is used

            IF(LD1 .EQ. 0)THEN
                  JB = JB + 1
                  GOTO 40
            ENDIF
            LUP = LD1
            LORIG = LD1
            L = LD1
            GOTO 10
      ENDIF

*       look if its part of linear structure

      LN = LQ(KQSP+L)
      IF(LN .NE. 0)THEN
            LUP = LN
            LORIG = LN
            L = LN
            GOTO 10
      ENDIF

*       look if it is end of a linear structure
*       i.e. origin and up link are in different banks

   50   CONTINUE
      LUP = LQ(KQSP+L+1)
      LORIG = LQ(KQSP+L+2)

*       look if back at top bank

      IF(LUP .LE. 1) GOTO 60

*       look if it would move above top bank

      IF(LUP .EQ. LFROM)      GOTO 60

      NSU = IQ(KQSP+LUP-2)
      IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN

*             its different, step back

            L = LORIG
            GOTO 50
      ENDIF

*       go up

      L = LUP
      GOTO 10

*       all done, but top bank

   60   CONTINUE
*
       WRITE(IQPRNT,'(A,I8,A)')' Total # of banks in tree: ',NBK
       WRITE(IQPRNT,'(A,I8,A)')' # of documented banks     ',NBDOCT
#if defined(CERNLIB_GKS)
       CALL GSFASI(-5)
#endif
#if defined(CERNLIB_HIGZ)
       CALL ISFASI(-5)
#endif
      IFOMED=0
      END
*********************************************************************
