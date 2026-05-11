
 
C***********************************************************************
C Kinematics of diffractive nucleon-nucleon interaction.               *
C          IFP1/2   PDG-indizes of projectile partons                  *
C                   (baryon: IFP2 - adiquark)                          *
C          PP(4)    projectile 4-momentum                              *
C          IFT1/2   PDG-indizes of target partons                      *
C                   (baryon: IFT1 - adiquark)                          *
C          PT(4)    target 4-momentum                                  *
C          KP   = 0 projectile quasi-elastically scattered             *
C               = 1            excited to low-mass diff. state         *
C               = 2            excited to high-mass diff. state        *
C          KT   = 0 target     quasi-elastically scattered             *
C               = 1            excited to low-mass diff. state         *
C               = 2            excited to high-mass diff. state        *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      SUBROUTINE DT_DIFFKI(Ifp1,Ifp2,Pp,Mop,Kp,Ift1,Ift2,Pt,Mot,Kt,Ncsy,
     &                     Irej)
      IMPLICIT NONE
      DOUBLE PRECISION anorf , bgtot , cod , cof , cosphi , dev1 , 
     &                 dev2 , DT_TDIFF , DT_XMHMD , DT_XMLMD , 
     &                 DT_YLAMB , dum , ONE , pitot , Pp , pp1 , 
     &                 ppblob , ppblt , ppblt2 , ppom1
      DOUBLE PRECISION ppomto , ppt , pptot , pptotf , Pt , pt1 , 
     &                 ptblob , pttot , pttotf , sid , sif , sinphi , 
     &                 TINY10 , TINY5 , tmin , tt , xmp , xmp2 , xmpf , 
     &                 xmpf2
      DOUBLE PRECISION xmt , xmt2 , xmtf , xmtf2 , xmtot , xmtot2 , xx , 
     &                 yy , ZERO , zz
      INTEGER ibp , ibt , idum , Ifp1 , Ifp2 , Ift1 , Ift2 , Irej , 
     &        irej1 , k , Kp , Kt , Mop , Mot , Ncsy
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY10=1.0D-10,TINY5=1.0D-5)
 
      LOGICAL lstart
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C rejection counter
      INCLUDE 'inc/dtrejc'
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
 
      DIMENSION pitot(4) , bgtot(4) , pp1(4) , pt1(4) , ppblob(4) , 
     &          ptblob(4) , Pp(4) , Pt(4) , ppom1(4) , dev1(4) , dev2(4)
 
      DATA lstart/.TRUE./
 
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,1X,'DIFEVT:  diffractive interactions treated ')
         lstart = .FALSE.
      END IF
 
      Irej = 0
 
C initialize common /DTDIKI/
      CALL DT_DIFINI
C store momenta of initial incoming particles for emc-check
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(Pp(1),Pp(2),Pp(3),Pp(4),1,idum,idum)
         CALL DT_EVTEMC(Pt(1),Pt(2),Pt(3),Pt(4),2,idum,idum)
      END IF
 
C masses of initial particles
      xmp2 = Pp(4)**2 - Pp(1)**2 - Pp(2)**2 - Pp(3)**2
      xmt2 = Pt(4)**2 - Pt(1)**2 - Pt(2)**2 - Pt(3)**2
      IF ( (xmp2.GE.ZERO) .AND. (xmt2.GE.ZERO) ) THEN
         xmp = SQRT(xmp2)
         xmt = SQRT(xmt2)
C check quark-input (used to adjust coherence cond. for M-selection)
         ibp = 0
         IF ( (ABS(Ifp1).GE.1000) .OR. (ABS(Ifp2).GE.1000) ) ibp = 1
         ibt = 0
         IF ( (ABS(Ift1).GE.1000) .OR. (ABS(Ift2).GE.1000) ) ibt = 1
 
C parameter for Lorentz-transformation into nucleon-nucleon cms
         DO k = 1 , 4
            pitot(k) = Pp(k) + Pt(k)
         END DO
         xmtot2 = pitot(4)**2 - pitot(1)**2 - pitot(2)**2 - pitot(3)**2
         IF ( xmtot2.LE.ZERO ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) xmtot2
99020       FORMAT (1X,'DIFEVT:   negative cm. energy!  ','XMTOT2 = ',
     &              E12.3)
            GOTO 100
         END IF
         xmtot = SQRT(xmtot2)
         DO k = 1 , 4
            bgtot(k) = pitot(k)/xmtot
         END DO
C transformation of nucleons into cms
         CALL DT_DALTRA(bgtot(4),-bgtot(1),-bgtot(2),-bgtot(3),Pp(1),
     &                  Pp(2),Pp(3),Pp(4),pptot,pp1(1),pp1(2),pp1(3),
     &                  pp1(4))
         CALL DT_DALTRA(bgtot(4),-bgtot(1),-bgtot(2),-bgtot(3),Pt(1),
     &                  Pt(2),Pt(3),Pt(4),pttot,pt1(1),pt1(2),pt1(3),
     &                  pt1(4))
C rotation angles
         cod = pp1(3)/pptot
C     SID = SQRT((ONE-COD)*(ONE+COD))
         ppt = SQRT(pp1(1)**2+pp1(2)**2)
         sid = ppt/pptot
         cof = ONE
         sif = ZERO
         IF ( pptot*sid.GT.TINY10 ) THEN
            cof = pp1(1)/(sid*pptot)
            sif = pp1(2)/(sid*pptot)
            anorf = SQRT(cof*cof+sif*sif)
            cof = cof/anorf
            sif = sif/anorf
         END IF
C check consistency
         DO k = 1 , 4
            dev1(k) = ABS(pp1(k)+pt1(k))
         END DO
         dev1(4) = ABS(dev1(4)-xmtot)
         IF ( (dev1(1).GT.TINY10) .OR. (dev1(2).GT.TINY10) .OR. 
     &        (dev1(3).GT.TINY10) .OR. (dev1(4).GT.TINY10) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) dev1
99030       FORMAT (1X,'DIFEVT:   inconsitent Lorentz-transformation! ',
     &              /,8X,4E12.3)
            GOTO 100
         END IF
 
C select x-fractions in high-mass diff. interactions
 
C select diffractive masses
C - projectile
         IF ( (Kp.EQ.2) .OR. (Kt.EQ.2) ) CALL DT_XVALHM(Kp,Kt)
         IF ( Kp.EQ.1 ) THEN
            xmpf = DT_XMLMD(xmtot)
            CALL DT_LM2RES(Ifp1,Ifp2,xmpf,IDPr,IDXpr,irej1)
            IF ( irej1.GT.0 ) GOTO 100
         ELSE IF ( Kp.EQ.2 ) THEN
            xmpf = DT_XMHMD(xmtot,ibp,1)
         ELSE
            xmpf = xmp
         END IF
C - target
         IF ( Kt.EQ.1 ) THEN
            xmtf = DT_XMLMD(xmtot)
            CALL DT_LM2RES(Ift1,Ift2,xmtf,IDTr,IDXtr,irej1)
            IF ( irej1.GT.0 ) GOTO 100
         ELSE IF ( Kt.EQ.2 ) THEN
            xmtf = DT_XMHMD(xmtot,ibt,2)
         ELSE
            xmtf = xmt
         END IF
 
C kinematical treatment of "two-particle" system (masses - XMPF,XMTF)
         xmpf2 = xmpf**2
         xmtf2 = xmtf**2
         ppblob(3) = DT_YLAMB(xmtot2,xmpf2,xmtf2)/(2.D0*xmtot)
         ppblob(4) = SQRT(xmpf2+ppblob(3)**2)
 
C select momentum transfer (all t-values used here are <0)
C   minimum absolute value to produce diffractive masses
         tmin = xmp2 + xmpf2 - 2.0D0*(pp1(4)*ppblob(4)-pptot*ppblob(3))
         tt = DT_TDIFF(xmtot,tmin,xmpf,Kp,xmtf,Kt,irej1)
         IF ( irej1.LE.0 ) THEN
 
C longitudinal momentum of excited/elastically scattered projectile
            ppblob(3) = (tt-xmp2-xmpf2+2.0D0*pp1(4)*ppblob(4))
     &                  /(2.0D0*pptot)
C total transverse momentum due to t-selection
            ppblt2 = ppblob(4)**2 - ppblob(3)**2 - xmpf2
            IF ( ppblt2.LT.ZERO ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) ppblt2 , Kp , pp1 , 
     &              xmpf , Kt , pt1 , xmtf , tt
99040          FORMAT (1X,'DIFEVT:   inconsistent transverse momentum! '
     &                 ,E12.3,2(/,1X,I2,5E12.3),/,1X,E12.3)
               GOTO 100
            END IF
            CALL DT_DSFECF(sinphi,cosphi)
            ppblt = SQRT(ppblt2)
            ppblob(1) = cosphi*ppblt
            ppblob(2) = sinphi*ppblt
 
C rotate excited/elastically scattered projectile into n-n cms.
            CALL DT_MYTRAN(1,ppblob(1),ppblob(2),ppblob(3),cod,sid,cof,
     &                     sif,xx,yy,zz)
            ppblob(1) = xx
            ppblob(2) = yy
            ppblob(3) = zz
 
C 4-momentum of excited/elastically scattered target and of exchanged
C Pomeron
            DO k = 1 , 4
               IF ( k.LT.4 ) ptblob(k) = -ppblob(k)
               ppom1(k) = pp1(k) - ppblob(k)
            END DO
            ptblob(4) = xmtot - ppblob(4)
 
C Lorentz-transformation back into system of initial diff. collision
            CALL DT_DALTRA(bgtot(4),bgtot(1),bgtot(2),bgtot(3),ppblob(1)
     &                     ,ppblob(2),ppblob(3),ppblob(4),pptotf,PPF(1),
     &                     PPF(2),PPF(3),PPF(4))
            CALL DT_DALTRA(bgtot(4),bgtot(1),bgtot(2),bgtot(3),ptblob(1)
     &                     ,ptblob(2),ptblob(3),ptblob(4),pttotf,PTF(1),
     &                     PTF(2),PTF(3),PTF(4))
            CALL DT_DALTRA(bgtot(4),bgtot(1),bgtot(2),bgtot(3),ppom1(1),
     &                     ppom1(2),ppom1(3),ppom1(4),ppomto,PPOm(1),
     &                     PPOm(2),PPOm(3),PPOm(4))
 
C store 4-momentum of elastically scattered particle (in single diff.
C events)
            IF ( Kp.EQ.0 ) THEN
               DO k = 1 , 4
                  PSC(k) = PPF(k)
               END DO
            ELSE IF ( Kt.EQ.0 ) THEN
               DO k = 1 , 4
                  PSC(k) = PTF(k)
               END DO
            END IF
 
C check consistency of kinematical treatment so far
            IF ( LEMcck ) THEN
               CALL DT_EVTEMC(-PPF(1),-PPF(2),-PPF(3),-PPF(4),2,idum,
     &                        idum)
               CALL DT_EVTEMC(-PTF(1),-PTF(2),-PTF(3),-PTF(4),2,idum,
     &                        idum)
               CALL DT_EVTEMC(dum,dum,dum,dum,3,60,irej1)
               IF ( irej1.NE.0 ) GOTO 100
            END IF
            DO k = 1 , 4
               dev1(k) = ABS(Pp(k)-PPF(k)-PPOm(k))
               dev2(k) = ABS(Pt(k)-PTF(k)+PPOm(k))
            END DO
            IF ( (dev1(1).GT.TINY5) .OR. (dev1(2).GT.TINY5) .OR. 
     &           (dev1(3).GT.TINY5) .OR. (dev1(4).GT.TINY5) .OR. 
     &           (dev2(1).GT.TINY5) .OR. (dev2(2).GT.TINY5) .OR. 
     &           (dev2(3).GT.TINY5) .OR. (dev2(4).GT.TINY5) ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99050) dev1 , dev2
99050          FORMAT (1X,
     &                 'DIFEVT:   inconsitent kinematical treatment!  ',
     &                 2(/,8X,4E12.3))
               GOTO 100
            END IF
 
C kinematical treatment for low-mass diffraction
            CALL DT_LMKINE(Ifp1,Ifp2,Kp,Ift1,Ift2,Kt,irej1)
            IF ( irej1.EQ.0 ) THEN
 
C dump diffractive chains into DTEVT1
               CALL DT_DIFPUT(Ifp1,Ifp2,Pp,Mop,Kp,Ift1,Ift2,Pt,Mot,Kt,
     &                        Ncsy,irej1)
 
               IF ( irej1.EQ.0 ) RETURN
            END IF
         END IF
      END IF
 
 100  IRDiff(1) = IRDiff(1) + 1
      Irej = 1
      END SUBROUTINE
