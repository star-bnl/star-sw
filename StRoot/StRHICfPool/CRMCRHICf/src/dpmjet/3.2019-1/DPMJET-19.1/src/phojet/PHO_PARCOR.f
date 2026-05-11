
      SUBROUTINE PHO_PARCOR(Mode,Irej)
C********************************************************************
C
C    conversion of string partons (using JETSET masses)
C
C    input:      MODE    >0 position index of corresponding string
C                        -1 initialization
C                        -2 output of statistics
C
C    output:     /POSTRG/
C                IREJ    1 combination of strings impossible
C                        0 successful combination
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION dele , DELM , DEPS , DT_RNDM , EPS , gam , gamb , 
     &                 pb1 , pb2 , pl , pp1 , pp2 , xm1 , xm2 , xmc , 
     &                 xml , xmp
      INTEGER i , i1 , i2 , ich , ifai , ii , ik , imode , ip1 , ip2 , 
     &        ipar , ipos , Irej , iter , k , k1 , k2 , kk , ks , l
      INTEGER Mode
      SAVE 
 
      PARAMETER (DELM=0.005D0,DEPS=1.D-15,EPS=1.D-5)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
      DIMENSION pp1(4) , pp2(4) , pb1(4) , pb2(4) , gam(3) , gamb(3) , 
     &          pl(4,100) , xmp(100) , xml(100)
 
      DOUBLE PRECISION PYMASS
 
      Irej = 0
      imode = Mode
C
      IF ( imode.GT.0 ) THEN
         ich = 0
         i1 = JMOhep(1,imode)
         i2 = ABS(JMOhep(2,imode))
C  copy to local field
         l = 0
         DO i = i1 , i2
            l = l + 1
            DO k = 1 , 4
               pl(k,l) = PHEp(k,i)
            END DO
            xmp(l) = PHEp(5,i)
 
            xml(l) = PYMASS(IDHep(i))
 
         END DO
         ipar = l
         xmc = PHEp(5,imode)
         IF ( IDEb(82).GE.20 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7,2I4)')
     &            'PHO_PARCOR: ini.momenta,masses(C/L),EV,ICH,L' , 
     &           KEVent , imode , l
            DO i = 1 , l
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,4E12.4,2X,2E12.4)')
     &              (pl(k,i),k=1,4) , xmp(i) , xml(i)
            END DO
         END IF
C
C  two parton configurations
C  -----------------------------------------
         IF ( ipar.EQ.2 ) THEN
            xm1 = xml(1)
            xm2 = xml(2)
            IF ( (xm1+xm2).GE.xmc ) THEN
               IF ( LPRi.GT.4 .AND. IDEb(82).GE.6 )
     &               WRITE (LO,'(1X,A,/,5X,I3,3E12.4)')
     &               'PHO_PARCOR: REJECTION I,XM1,XM2,XMC' , imode , 
     &              xm1 , xm2 , xmc
               GOTO 100
            END IF
C  conversion possible
            CALL PHO_MSHELL(pl(1,1),pl(1,2),xm1,xm2,pp1,pp2,Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(36) = IFAil(36) + 1
               IF ( LPRi.GT.4 .AND. IDEb(82).GE.6 )
     &               WRITE (LO,'(1X,A,I8,I4,E12.4)') 
     &              'PHO_PARCOR: rejection by PHO_MSHELL EV,STRING,MASS'
     &              , KEVent , imode , xmc
               GOTO 100
            END IF
            ich = 1
            DO k = 1 , 4
               pl(k,1) = pp1(k)
               pl(k,2) = pp2(k)
               xmp(1) = xm1
               xmp(2) = xm2
            END DO
C
C  multi parton configurations
C  ---------------------------------
         ELSE
C
C  random selection of string side to start with
            IF ( DT_RNDM(xmc).LT.0.5D0 ) THEN
               k1 = 1
               k2 = ipar
               ks = 1
            ELSE
               k1 = ipar
               k2 = 1
               ks = -1
            END IF
            iter = 0
C
            DO WHILE ( iter.LT.4 )
               kk = k1
               k1 = k2
               k2 = kk
               ks = -ks
               iter = iter + 1
C  select method
               IF ( iter.GT.2 ) THEN
C
C
C  conversion according to remainder method
                  DO i = k1 , k2 , ks
                     xm1 = xml(i)
                     IF ( ABS(xm1-xmp(i)).GT.DELM ) THEN
                        ich = ich + 1
                        ifai = i
C  conversion necessary
                        DO k = 1 , 4
                           pb1(k) = pl(k,i)
                           pb2(k) = PHEp(k,imode) - pb1(k)
                        END DO
                        xm2 = pb2(4)**2 - pb2(1)**2 - pb2(2)**2 - pb2(3)
     &                        **2
                        IF ( xm2.LT.0.D0 ) THEN
                           IF ( LPRi.GT.4 .AND. IDEb(82).GE.10 )
     &                        WRITE (LO,'(1X,2A,/,5X,3I3,4E12.4)')
     &                         'PHO_PARCOR: ' , 
     &                        'int.rej. I,IPA,ICH,XML,XMP,XM2**2,MCHAIN'
     &                        , i , ipar , imode , xm1 , xmp(i) , xm2 , 
     &                        xmc
                           GOTO 20
                        END IF
                        xm2 = SQRT(xm2)
                        IF ( (xm1+xm2).GE.xmc ) THEN
                           IF ( LPRi.GT.4 .AND. IDEb(82).GE.10 )
     &                        WRITE (LO,'(1X,2A,/,5X,3I3,4E12.4)')
     &                         'PHO_PARCOR: ' , 
     &                        'int.rej. I,IPA,ICH,XML,XMP,XM2,XMC' , i , 
     &                        ipar , imode , xm1 , xmp(i) , xm2 , xmc
                           GOTO 20
                        END IF
C  conversion possible
                        CALL PHO_MSHELL(pb1,pb2,xm1,xm2,pp1,pp2,Irej)
                        IF ( Irej.NE.0 ) THEN
                           IFAil(36) = IFAil(36) + 1
                           IF ( LPRi.GT.4 .AND. IDEb(82).GE.6 )
     &                        WRITE (LO,'(1X,A,I8,3I4)') 
     &                  'PHO_PARCOR: PHO_MSHELL rej. ITER,STRING,PARTON'
     &                  , iter , imode , i
                           GOTO 20
                        END IF
C  calculate Lorentz transformation
                        CALL PHO_GETLTR(pb2,pp2,gam,gamb,dele,Irej)
                        IF ( Irej.NE.0 ) THEN
                           IF ( LPRi.GT.4 .AND. IDEb(82).GE.6 )
     &                        WRITE (LO,'(1X,A,I8,3I4)') 
     &                  'PHO_PARCOR: PHO_GETLTR rej. ITER,STRING,PARTON'
     &                  , iter , imode , i
                           GOTO 20
                        END IF
                        ifai = 0
C  transform remaining partons
                        DO l = k1 , k2 , ks
                           IF ( l.NE.i ) THEN
                              CALL PHO_MKSLTR(pl(1,l),pp2,gam,gamb)
                              DO k = 1 , 4
                               pl(k,l) = pp2(k)
                              END DO
                           ELSE
                              DO k = 1 , 4
                               pl(k,l) = pp1(k)
                              END DO
                           END IF
                        END DO
                        xmp(i) = xm1
                     END IF
                  END DO
               ELSE
 
C  conversion according to color flow method
                  ifai = 0
                  DO ii = k1 , k2 - ks , ks
                     DO ik = ii + ks , k2 , ks
                        xm1 = xml(ii)
                        xm2 = xml(ik)
C             IF(IDEB(82).GE.10) WRITE(LO,'(1X,A,2I3,4E12.4)')
C    &          'PHO_PARCOR:I,K,XM(1-4)',II,IK,XM1,XMP(II),XM2,XMP(IK)
                        IF ( (ABS(xm1-xmp(ii)).LE.DELM) .AND. 
     &                       (ABS(xm2-xmp(ik)).LE.DELM) ) GOTO 5
                        CALL PHO_MSHELL(pl(1,ii),pl(1,ik),xm1,xm2,pp1,
     &                     pp2,Irej)
                        IF ( Irej.NE.0 ) THEN
                           IFAil(36) = IFAil(36) + 1
                           IF ( LPRi.GT.4 .AND. IDEb(82).GE.6 )
     &                        WRITE (LO,'(1X,2A,I8,3I4)')
     &                        'PHO_PARCOR: ' , 
     &                        'int.rej. by PHO_MSHELL EV,IC,I1,I2' , 
     &                        KEVent , imode , ii , ik
                           Irej = 0
                        ELSE
                           ich = ich + 1
                           DO kk = 1 , 4
                              pl(kk,ii) = pp1(kk)
                              pl(kk,ik) = pp2(kk)
                           END DO
                           xmp(ii) = xm1
                           xmp(ik) = xm2
                           GOTO 5
                        END IF
                     END DO
                     ifai = ii
 5                END DO
                  IF ( ifai.NE.0 ) GOTO 20
               END IF
               GOTO 50
 20         END DO
            GOTO 100
         END IF
 
C  register transformed partons
 50      Irej = 0
         IF ( ich.NE.0 ) THEN
            ip1 = NHEp + 1
            l = 0
            DO i = i1 , i2
               l = l + 1
               CALL PHO_REGPAR(-1,IDHep(i),0,imode,0,pl(1,l),pl(2,l),
     &            pl(3,l),pl(4,l),IPHist(1,i),IPHist(2,i),ICOlor(1,i),
     &            ICOlor(2,i),ipos,1)
            END DO
            ip2 = ipos
C  register string
            CALL PHO_REGPAR(-1,90,0,ip1,-ip2,PHEp(1,imode),PHEp(2,imode)
     &                      ,PHEp(3,imode),PHEp(4,imode),IPHist(1,imode)
     &                      ,IPHist(2,imode),ICOlor(1,imode),
     &                      ICOlor(2,imode),ipos,1)
C  update /POSTRG/
            i = IPHist(1,imode)
            NPOs(1,i) = ipos
            NPOs(2,i) = ip1
            NPOs(3,i) = -ip2
         END IF
C  debug output
         IF ( IDEb(82).GE.20 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7,2I4)')
     &            'PHO_PARCOR: fin.momenta,masses(C/L),(EV,ICH,L)' , 
     &           KEVent , imode , l
            DO i = 1 , l
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,4E12.4,2X,2E12.4)')
     &              (pl(k,i),k=1,4) , xmp(i) , xml(i)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &            'PHO_PARCOR: conversion done (old/new ICH)' , imode , 
     &           ipos
         END IF
         RETURN
C  rejection
 100     Irej = 1
         IF ( IDEb(82).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/,5X,3I5,E12.4)')
     &            'PHO_PARCOR: rejection I,IPAR,ICHAIN,MCHAIN' , ifai , 
     &           ipar , imode , xmc
            IF ( IDEb(82).GE.5 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7,2I4)')
     &               'PHO_PARCOR: momenta,masses(C/L),(EV,ICH,L)' , 
     &              KEVent , imode , ipar
               DO i = 1 , ipar
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,4E12.4,2X,2E12.4)')
     &                 (pl(k,i),k=1,4) , xmp(i) , xml(i)
               END DO
            END IF
         END IF
         RETURN
 
      ELSE IF ( imode.EQ.-1 ) THEN
C  initialization
         RETURN
 
      ELSE IF ( imode.EQ.-2 ) THEN
C  final output
         RETURN
      END IF
      END SUBROUTINE
