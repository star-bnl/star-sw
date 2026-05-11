
      SUBROUTINE PHO_HARSCA(Imode,Ip)
C***********************************************************************
C
C     PHO_HARSCA determines the type of hard subprocess, the partons
C     taking part in this subprocess and the kinematic variables
C
C     input:  IMODE   1   direct processes
C                     2   resolved processes
C                     -1  initialization
C                     -2  output of statistics
C             IP      1-4 particle combination (hadron/photon)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION b , cosphi , DEPS , DT_RNDM , ecm2 , EPS , f , 
     &                 fac , fac1 , fac2 , fxp , pds , PHO_PMASS , 
     &                 scheck , shat , sinphi , sum , xm3 , xm4
      INTEGER Imode , Ip , irej , j , k , m
      SAVE 
 
      PARAMETER (EPS=1.D-10,DEPS=1.D-30)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  hard scattering data
      INCLUDE 'inc/pohslt'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  cross sections
      INCLUDE 'inc/pocsec'
C  some constants
      INCLUDE 'inc/pocons'
 
 
C  resolved processes
 100  IF ( Imode.EQ.2 ) THEN
 
         MH_pro_on(0,Ip,IDXmpar) = 0
         HWGx(9,IDXmpar) = 0.D0
         DO m = -1 , 8
            IF ( MH_pro_on(m,Ip,IDXmpar).EQ.1 ) HWGx(9,IDXmpar)
     &           = HWGx(9,IDXmpar) + HWGx(m,IDXmpar)
         END DO
         IF ( HWGx(9,IDXmpar).LT.DEPS ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I4,1P,E12.4)')
     &            'PHO_HARSCA:ERROR: ' , 
     &           'no resolved process possible for IP' , Ip , 
     &           HWGx(9,IDXmpar)
            CALL PHO_ABORT
         END IF
C
C ----------------------------------------------I
C  begin of iteration loop (resolved processes) I
C                                               I
         IREjsc = 0
 150     IREjsc = IREjsc + 1
         IF ( IREjsc.GT.1000 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I10)')
     &            'PHO_HARSCA:ERROR: too many rejections (resolved)' , 
     &           IREjsc
            CALL PHO_ABORT
         END IF
 
C  find subprocess
         b = DT_RNDM(X1)*HWGx(9,IDXmpar)
         MSPr = -2
         sum = 0.D0
 200     MSPr = MSPr + 1
         IF ( MH_pro_on(MSPr,Ip,IDXmpar).EQ.1 ) sum = sum + 
     &        HWGx(MSPr,IDXmpar)
         IF ( sum.LT.b .AND. MSPr.LT.8 ) GOTO 200
 
         IF ( LPRi.GT.4 .AND. IDEb(78).GE.20 ) WRITE (LO,'(1x,a,i3,i6)')
     &         'PHO_HARSCA: resolved process (MSPR,IREJSC)' , MSPr , 
     &        IREjsc
 
C  find kin. variables X1,X2 and V
         CALL PHO_HARKIN(irej)
         IF ( irej.NE.0 ) THEN
            IFAil(29) = IFAil(29) + 1
            GOTO 150
         END IF
C  calculate remaining distribution
         CALL PHO_HARWGH(pds,PDF1,PDF2,f)
C  actualize counter for cross-section calculation
         IF ( f.LE.1.D-15 ) THEN
            f = 0.D0
            GOTO 150
         END IF
C       XSECT(5,MSPR) = XSECT(5,MSPR)+F
C       XSECT(6,MSPR) = XSECT(6,MSPR)+F*F
         MH_tried(MSPr,Ip,IDXmpar) = MH_tried(MSPr,Ip,IDXmpar) + 1
C  check F against FMAX
         WEIght = f/(HWGx(MSPr,IDXmpar)+DEPS)
         IF ( WEIght.LT.DT_RNDM(X2) ) GOTO 150
C-------------------------------------------------------------------
         IF ( WEIght.GT.1.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,99010) MSPr , Ip , IDPdg1 , 
     &           IDPdg2 , f , HWGx(MSPr,IDXmpar) , WEIght
99010       FORMAT (/,' PHO_HARSCA: (resolved) W>1 (MSPR,IP,ID1,2)',2I3,
     &              2I7,/' F,HWgx(MSPR),W',3E12.4)
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.3)')
     &            'ECM,PTWANT,AS,AH,PT' , ECMp , PTWant , AS , AH , PT
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.3)')
     &            'ETAC,ETAD,X1,X2,V' , ETAc , ETAd , X1 , X2 , V
            CALL PHO_PREVNT(-1)
         END IF
C-------------------------------------------------------------------
C                                             I
C  end of iteration loop (resolved processes) I
C --------------------------------------------I
C
C*********************************************************************
C
C  direct processes
 
      ELSE IF ( Imode.EQ.1 ) THEN
 
C  single-resolved processes kinematically forbidden
         IF ( Z1Dif.LT.0.D0 ) THEN
            HWGx(10,IDXmpar) = 0.D0
            HWGx(11,IDXmpar) = 0.D0
            HWGx(12,IDXmpar) = 0.D0
            HWGx(13,IDXmpar) = 0.D0
         END IF
 
         HWGx(15,IDXmpar) = 0.D0
         IF ( (IPAmdl(115).EQ.0) .AND. (Ip.EQ.1) ) THEN
            DO m = 10 , 14
               IF ( MH_pro_on(m,Ip,IDXmpar).EQ.1 ) THEN
                  IF ( (m.EQ.10) .OR. (m.EQ.11) ) THEN
                     fac = FSUh(1)*FSUp(2)
                  ELSE IF ( (m.EQ.12) .OR. (m.EQ.13) ) THEN
                     fac = FSUp(1)*FSUh(2)
                  ELSE
                     fac = FSUh(1)*FSUh(2)
                  END IF
                  HWGx(15,IDXmpar) = HWGx(15,IDXmpar) + HWGx(m,IDXmpar)
     &               *fac
               END IF
            END DO
         ELSE
            DO m = 10 , 14
               IF ( MH_pro_on(m,Ip,IDXmpar).EQ.1 ) HWGx(15,IDXmpar)
     &              = HWGx(15,IDXmpar) + HWGx(m,IDXmpar)
            END DO
         END IF
         IF ( HWGx(15,IDXmpar).LT.DEPS ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I4)')
     &            'PHO_HARSCA:ERROR: ' , 
     &           'no direct/single-resolved process possible (IP)' , Ip
            CALL PHO_ABORT
         END IF
C
C ----------------------------------------------I
C  begin of iteration loop (direct processes)   I
C                                               I
         IREjsc = 0
 250     IREjsc = IREjsc + 1
         IF ( IREjsc.GT.1000 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I10)')
     &            'PHO_HARSCA:ERROR: too many rejections (direct)' , 
     &           IREjsc
            CALL PHO_ABORT
         END IF
 
C  find subprocess
         b = DT_RNDM(X1)*HWGx(15,IDXmpar)
         MSPr = 9
         sum = 0.D0
         IF ( (IPAmdl(115).EQ.0) .AND. (Ip.EQ.1) ) THEN
 260        MSPr = MSPr + 1
            IF ( MH_pro_on(MSPr,Ip,IDXmpar).EQ.1 ) THEN
               IF ( (MSPr.EQ.10) .OR. (MSPr.EQ.11) ) THEN
                  fac = FSUh(1)*FSUp(2)
               ELSE IF ( (MSPr.EQ.12) .OR. (MSPr.EQ.13) ) THEN
                  fac = FSUp(1)*FSUh(2)
               ELSE
                  fac = FSUh(1)*FSUh(2)
               END IF
               sum = sum + HWGx(MSPr,IDXmpar)*fac
            END IF
            IF ( sum.LT.b .AND. MSPr.LT.14 ) GOTO 260
         ELSE
 280        MSPr = MSPr + 1
            IF ( MH_pro_on(MSPr,Ip,IDXmpar).EQ.1 ) sum = sum + 
     &           HWGx(MSPr,IDXmpar)
            IF ( sum.LT.b .AND. MSPr.LT.14 ) GOTO 280
         END IF
 
         IF ( LPRi.GT.4 .AND. IDEb(78).GE.20 ) WRITE (LO,'(1x,a,i3,i6)')
     &         'PHO_HARSCA: direct process (MSPR,IREJSC)' , MSPr , 
     &        IREjsc
 
C  find kin. variables X1,X2 and V
         CALL PHO_HARKIN(irej)
         IF ( irej.NE.0 ) THEN
            IFAil(28) = IFAil(28) + 1
            GOTO 250
         END IF
 
C  calculate remaining distribution
         CALL PHO_HARWGH(pds,PDF1,PDF2,f)
 
C  counter for cross-section calculation
         IF ( f.LE.1.D-15 ) THEN
            f = 0.D0
            GOTO 250
         END IF
C       XSECT(5,MSPR) = XSECT(5,MSPR)+F
C       XSECT(6,MSPR) = XSECT(6,MSPR)+F*F
         MH_tried(MSPr,Ip,IDXmpar) = MH_tried(MSPr,Ip,IDXmpar) + 1
C  check F against FMAX
         WEIght = f/(HWGx(MSPr,IDXmpar)+DEPS)
         IF ( WEIght.LT.DT_RNDM(X2) ) GOTO 250
C-------------------------------------------------------------------
         IF ( WEIght.GT.1.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,99020) MSPr , Ip , IDPdg1 , 
     &           IDPdg2 , f , HWGx(MSPr,IDXmpar) , WEIght
99020       FORMAT (/,' PHO_HARSCA: (direct) W>1 (MSPR,IP,ID1,2)',2I3,
     &              2I7,/,' F,HWgx(MSPR),W',3E12.4)
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.3)')
     &            'ECM,PTWANT,AS,AH,PT' , ECMp , PTWant , AS , AH , PT
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E11.3)')
     &            'ETAC,ETAD,X1,X2,V' , ETAc , ETAd , X1 , X2 , V
            CALL PHO_PREVNT(-1)
         END IF
C-------------------------------------------------------------------
C                                             I
C  end of iteration loop (direct processes)   I
C --------------------------------------------I
 
      ELSE IF ( Imode.EQ.-1 ) THEN
 
C  initialize cross section calculations
 
         DO m = -1 , MAX_PRO_2
C         DO 30 I=5,6
C           XSECT(I,M) = 0.D0
C30       CONTINUE
C  reset counters
            DO j = 1 , 4
               MH_tried(m,j,IDXmpar) = 0
               MH_acc_1(m,j,IDXmpar) = 0
               MH_acc_2(m,j,IDXmpar) = 0
            END DO
         END DO
         IF ( IDEb(78).GE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)')
     &            'PHO_HARSCA: activated hard processes' , 
     &           '------------------------------------'
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &            'PROCESS,    IP= 1 ... 4 (on/off)'
            DO m = 1 , MAX_PRO_2
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,I3,5X,A,4I3)') m , 
     &              PROc(m) , (MH_pro_on(m,j,IDXmpar),j=1,4)
            END DO
         END IF
         RETURN
 
      ELSE IF ( Imode.EQ.-2 ) THEN
 
C  calculation of process statistics
 
         DO k = 1 , 4
 
            MH_tried(0,k,IDXmpar) = 0
            MH_acc_1(0,k,IDXmpar) = 0
            MH_acc_2(0,k,IDXmpar) = 0
            MH_tried(9,k,IDXmpar) = 0
            MH_acc_1(9,k,IDXmpar) = 0
            MH_acc_2(9,k,IDXmpar) = 0
            MH_tried(15,k,IDXmpar) = 0
            MH_acc_1(15,k,IDXmpar) = 0
            MH_acc_2(15,k,IDXmpar) = 0
 
            MH_tried(3,k,IDXmpar) = MH_tried(3,k,IDXmpar)
     &         + MH_tried(-1,k,IDXmpar)
            MH_acc_1(3,k,IDXmpar) = MH_acc_1(3,k,IDXmpar)
     &         + MH_acc_1(-1,k,IDXmpar)
            MH_acc_2(3,k,IDXmpar) = MH_acc_2(3,k,IDXmpar)
     &         + MH_acc_2(-1,k,IDXmpar)
 
            DO m = 1 , 8
               MH_tried(9,k,IDXmpar) = MH_tried(9,k,IDXmpar)
     &            + MH_tried(m,k,IDXmpar)
               MH_acc_1(9,k,IDXmpar) = MH_acc_1(9,k,IDXmpar)
     &            + MH_acc_1(m,k,IDXmpar)
               MH_acc_2(9,k,IDXmpar) = MH_acc_2(9,k,IDXmpar)
     &            + MH_acc_2(m,k,IDXmpar)
            END DO
            DO m = 10 , 14
               MH_tried(15,k,IDXmpar) = MH_tried(15,k,IDXmpar)
     &            + MH_tried(m,k,IDXmpar)
               MH_acc_1(15,k,IDXmpar) = MH_acc_1(15,k,IDXmpar)
     &            + MH_acc_1(m,k,IDXmpar)
               MH_acc_2(15,k,IDXmpar) = MH_acc_2(15,k,IDXmpar)
     &            + MH_acc_2(m,k,IDXmpar)
            END DO
            MH_tried(0,k,IDXmpar) = MH_tried(9,k,IDXmpar)
     &         + MH_tried(15,k,IDXmpar)
            MH_acc_1(0,k,IDXmpar) = MH_acc_1(9,k,IDXmpar)
     &         + MH_acc_1(15,k,IDXmpar)
            MH_acc_2(0,k,IDXmpar) = MH_acc_2(9,k,IDXmpar)
     &         + MH_acc_2(15,k,IDXmpar)
         END DO
 
         IF ( IDEb(78).GE.1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)')
     &            'PHO_HARSCA: internal rejection statistics' , 
     &           '-----------------------------------------'
            DO k = 1 , 4
               IF ( MH_tried(0,k,IDXmpar).GT.0 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I3)')
     &                  'process (sampled/accepted) for IP:' , k
                  DO m = 0 , MAX_PRO_2
                     IF ( LPRi.GT.4 )
     &                     WRITE (LO,'(1X,I3,1X,A,2X,3I12,F10.4)') m , 
     &                    PROc(m) , MH_tried(m,k,IDXmpar) , 
     &                    MH_acc_1(m,k,IDXmpar) , MH_acc_2(k,k,IDXmpar)
     &                    , DBLE(MH_acc_1(m,k,IDXmpar))
     &                    /DBLE(MAX(1,MH_tried(m,k,IDXmpar)))
                  END DO
               END IF
            END DO
         END IF
         RETURN
 
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I10)')
     &        'PHO_HARSCA:ERROR: ' , 'unsupported mode' , Imode
         CALL PHO_ABORT
      END IF
 
C  the event is accepted now
C  actualize counter for accepted events
      MH_acc_1(MSPr,Ip,IDXmpar) = MH_acc_1(MSPr,Ip,IDXmpar) + 1
      IF ( MSPr.EQ.-1 ) MSPr = 3
C
C  find flavor of initial partons
C
      sum = 0.D0
      scheck = DT_RNDM(sum)*pds - EPS
      IF ( MSPr.EQ.1 .OR. MSPr.EQ.4 ) THEN
         IA = 0
         IB = 0
      ELSE IF ( MSPr.EQ.2 .OR. MSPr.EQ.5 .OR. MSPr.EQ.6 ) THEN
         DO IA = -NF , NF
            IF ( IA.NE.0 ) THEN
               sum = sum + PDF1(IA)*PDF2(-IA)
               IF ( sum.GE.scheck ) GOTO 300
            END IF
         END DO
 300     IB = -IA
      ELSE IF ( MSPr.EQ.3 ) THEN
         IB = 0
         DO IA = -NF , NF
            IF ( IA.NE.0 ) THEN
               sum = sum + PDF1(0)*PDF2(IA)
               IF ( sum.GE.scheck ) GOTO 350
               sum = sum + PDF1(IA)*PDF2(0)
               IF ( sum.GE.scheck ) GOTO 500
            END IF
         END DO
 350     IB = IA
         IA = 0
      ELSE IF ( MSPr.EQ.7 ) THEN
         DO IA = -NF , NF
            IF ( IA.NE.0 ) THEN
               sum = sum + PDF1(IA)*PDF2(IA)
               IF ( sum.GE.scheck ) GOTO 400
            END IF
         END DO
 400     IB = IA
      ELSE IF ( MSPr.EQ.8 ) THEN
         DO IA = -NF , NF
            IF ( IA.NE.0 ) THEN
               DO IB = -NF , NF
                  IF ( ABS(IB).NE.ABS(IA) .AND. IB.NE.0 ) THEN
                     sum = sum + PDF1(IA)*PDF2(IB)
                     IF ( sum.GE.scheck ) GOTO 500
                  END IF
               END DO
            END IF
         END DO
      ELSE IF ( MSPr.EQ.10 ) THEN
         IA = 0
         DO IB = -NF , NF
            IF ( IB.NE.0 ) THEN
               IF ( IDPdg1.EQ.22 ) THEN
C             IF(MOD(ABS(IB),2).EQ.0) THEN
C               SUM = SUM+PDF2(IB)*4.D0/9.D0
C             ELSE
C               SUM = SUM+PDF2(IB)*1.D0/9.D0
C             ENDIF
                  sum = sum + PDF2(IB)*Q_Ch2(IB)
               ELSE
                  sum = sum + PDF2(IB)
               END IF
               IF ( sum.GE.scheck ) GOTO 500
            END IF
         END DO
      ELSE IF ( MSPr.EQ.12 ) THEN
         IB = 0
         DO IA = -NF , NF
            IF ( IA.NE.0 ) THEN
               IF ( IDPdg2.EQ.22 ) THEN
C             IF(MOD(ABS(IA),2).EQ.0) THEN
C               SUM = SUM+PDF1(IA)*4.D0/9.D0
C             ELSE
C               SUM = SUM+PDF1(IA)*1.D0/9.D0
C             ENDIF
                  sum = sum + PDF1(IA)*Q_Ch2(IA)
               ELSE
                  sum = sum + PDF1(IA)
               END IF
               IF ( sum.GE.scheck ) GOTO 500
            END IF
         END DO
      ELSE IF ( (MSPr.EQ.11) .OR. (MSPr.EQ.13) .OR. (MSPr.EQ.14) ) THEN
         IA = 0
         IB = 0
      END IF
C  final check
 500  IF ( (ABS(IA).GT.NF) .OR. (ABS(IB).GT.NF) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &         'PHO_HARSCA: rejection, final check IA,IB' , IA , IB
         IF ( LPRi.GT.4 ) WRITE (LO,*) 'EVENT,MSPR,IA,IB,NF: ' , 
     &        KEVent , MSPr , IA , IB , NF
         GOTO 100
      END IF
C
C  find flavour of final partons
C
      IC = IA
      ID = IB
      IF ( MSPr.EQ.2 ) THEN
         IC = 0
         ID = 0
      ELSE IF ( MSPr.EQ.4 ) THEN
         IC = INT(FLOAT(NF+NF)*DT_RNDM(sum)) + 1
         IF ( IC.GT.NF ) IC = NF - IC
         ID = -IC
      ELSE IF ( MSPr.EQ.6 ) THEN
         IC = INT(FLOAT(NF+NF-2)*DT_RNDM(sum)) + 1
         IF ( IC.GT.NF-1 ) IC = NF - 1 - IC
         IF ( ABS(IC).EQ.ABS(IA) ) IC = SIGN(NF,IC)
         ID = -IC
      ELSE IF ( MSPr.EQ.11 ) THEN
         sum = 0.D0
         DO IC = -NF , NF
            IF ( IC.NE.0 ) THEN
               IF ( IDPdg1.EQ.22 ) THEN
C             IF(MOD(ABS(IC),2).EQ.0) THEN
C               SUM = SUM + 4.D0
C             ELSE
C               SUM = SUM + 1.D0
C             ENDIF
                  sum = sum + Q_Ch2(IC)
               ELSE
                  sum = sum + 1.D0
               END IF
            END IF
         END DO
         scheck = DT_RNDM(sum)*sum - EPS
         sum = 0.D0
         DO IC = -NF , NF
            IF ( IC.NE.0 ) THEN
               IF ( IDPdg1.EQ.22 ) THEN
C             IF(MOD(ABS(IC),2).EQ.0) THEN
C               SUM = SUM + 4.D0
C             ELSE
C               SUM = SUM + 1.D0
C             ENDIF
                  sum = sum + Q_Ch2(IC)
               ELSE
                  sum = sum + 1.D0
               END IF
               IF ( sum.GE.scheck ) GOTO 550
            END IF
         END DO
 550     ID = -IC
      ELSE IF ( MSPr.EQ.12 ) THEN
         IC = 0
         ID = IA
      ELSE IF ( MSPr.EQ.13 ) THEN
         sum = 0.D0
         DO IC = -NF , NF
            IF ( IC.NE.0 ) THEN
               IF ( IDPdg2.EQ.22 ) THEN
C             IF(MOD(ABS(IC),2).EQ.0) THEN
C               SUM = SUM + 4.D0
C             ELSE
C               SUM = SUM + 1.D0
C             ENDIF
                  sum = sum + Q_Ch2(IC)
               ELSE
                  sum = sum + 1.D0
               END IF
            END IF
         END DO
         scheck = DT_RNDM(sum)*sum - EPS
         sum = 0.D0
         DO IC = -NF , NF
            IF ( IC.NE.0 ) THEN
               IF ( IDPdg2.EQ.22 ) THEN
C             IF(MOD(ABS(IC),2).EQ.0) THEN
C               SUM = SUM + 4.D0
C             ELSE
C               SUM = SUM + 1.D0
C             ENDIF
                  sum = sum + Q_Ch2(IC)
               ELSE
                  sum = sum + 1.D0
               END IF
               IF ( sum.GE.scheck ) GOTO 600
            END IF
         END DO
 600     ID = -IC
      ELSE IF ( MSPr.EQ.14 ) THEN
         sum = 0.D0
         DO IC = 1 , NF
            fac1 = 1.D0
            fac2 = 1.D0
            IF ( MOD(ABS(IC),2).EQ.0 ) THEN
               IF ( IDPdg1.EQ.22 ) fac1 = 4.D0
               IF ( IDPdg2.EQ.22 ) fac2 = 4.D0
            END IF
            sum = sum + fac1*fac2
         END DO
         IF ( IPAmdl(64).NE.0 ) THEN
            IF ( (IDPdg1.EQ.22) .AND. (IDPdg2.EQ.22) ) sum = sum + 81.D0
         END IF
         scheck = DT_RNDM(sum)*sum - EPS
         sum = 0.D0
         DO IC = 1 , NF
            fac1 = 1.D0
            fac2 = 1.D0
            IF ( MOD(ABS(IC),2).EQ.0 ) THEN
               IF ( IDPdg1.EQ.22 ) fac1 = 4.D0
               IF ( IDPdg2.EQ.22 ) fac2 = 4.D0
            END IF
            sum = sum + fac1*fac2
            IF ( sum.GE.scheck ) GOTO 650
         END DO
         IC = 15
 650     ID = -IC
         IF ( DT_RNDM(fac1).GT.0.5D0 ) CALL PHO_SWAPI(IC,ID)
      END IF
      IF ( IC.EQ.0 ) THEN
         xm3 = 0.D0
      ELSE
         xm3 = PHO_PMASS(IC,3)
      END IF
      IF ( ID.EQ.0 ) THEN
         xm4 = 0.D0
      ELSE
         xm4 = PHO_PMASS(ID,3)
      END IF
      IF ( ABS(IC).NE.15 ) THEN
 
C  valence quarks involved?
         IV1 = 0
         IF ( IA.NE.0 ) THEN
            IF ( IDPdg1.EQ.22 ) THEN
               CALL PHO_QPMPDF(IA,X1,QQPd,0.D0,PVIrtp(1),fxp)
               IF ( DT_RNDM(xm3)*PDF1(IA).GT.PDF1(IA)-fxp ) IV1 = 1
            ELSE
               IF ( DT_RNDM(xm3)*PDF1(IA).GT.PDF1(-IA) ) IV1 = 1
            END IF
         END IF
         IV2 = 0
         IF ( IB.NE.0 ) THEN
            IF ( IDPdg2.EQ.22 ) THEN
               CALL PHO_QPMPDF(IB,X2,QQPd,0.D0,PVIrtp(2),fxp)
               IF ( DT_RNDM(xm4)*PDF2(IB).GT.PDF2(IB)-fxp ) IV2 = 1
            ELSE
               IF ( DT_RNDM(xm4)*PDF2(IB).GT.PDF2(-IB) ) IV2 = 1
            END IF
         END IF
      END IF
C
C  fill event record
C
      CALL PHO_SFECFE(sinphi,cosphi)
      ecm2 = ECMp/2.D0
C  incoming partons
      PHI1(1) = 0.D0
      PHI1(2) = 0.D0
      PHI1(3) = ecm2*X1
      PHI1(4) = PHI1(3)
      PHI1(5) = 0.D0
      PHI2(1) = 0.D0
      PHI2(2) = 0.D0
      PHI2(3) = -ecm2*X2
      PHI2(4) = -PHI2(3)
      PHI2(5) = 0.D0
C  outgoing partons
      PHO1(1) = PT*cosphi
      PHO1(2) = PT*sinphi
      PHO1(3) = -ecm2*(U*X1-V*X2)
      PHO1(4) = -ecm2*(U*X1+V*X2)
      PHO1(5) = xm3
      PHO2(1) = -PHO1(1)
      PHO2(2) = -PHO1(2)
      PHO2(3) = -ecm2*(V*X1-U*X2)
      PHO2(4) = -ecm2*(V*X1+U*X2)
      PHO2(5) = xm4
 
C  convert to mass shell
      CALL PHO_MSHELL(PHO1,PHO2,xm3,xm4,PHO1,PHO2,irej)
      IF ( irej.NE.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(78).GE.5 )
     &         WRITE (LO,'(1X,A,1P,3E11.3)')
     &         'PHO_HARSCA: rejection by PHO_MSHELL (PT,M1,M2)' , PT , 
     &        xm3 , xm4
         GOTO 100
      END IF
      PTFin = SQRT(PHO1(1)**2+PHO1(2)**2)
 
C  debug output
      IF ( IDEb(78).GE.20 ) THEN
         shat = X1*X2*ECMp*ECMp
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5I4)')
     &         'PHO_HARSCA: MSPR,IA,IB,IC,ID' , MSPr , IA , IB , IC , ID
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,4E11.3)') 'X1/2,MU2,Q2 ' , 
     &        X1 , X2 , QQPd , QQAl
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,4E11.3)') 'U,V,PT,SHAT ' , 
     &        U , V , PT , shat
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,5E11.3)') 'PHI1 ' , PHI1
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,5E11.3)') 'PHI2 ' , PHI2
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,5E11.3)') 'PHO1 ' , PHO1
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,5E11.3)') 'PHO2 ' , PHO2
      END IF
 
      END SUBROUTINE
