
      SUBROUTINE DT_HADRIN(Idpr,Ppr,Idta,Pta,Mode,Irej)
 
C***********************************************************************
C Interface to the HADRIN-routines for inelastic and elastic           *
C scattering.                                                          *
C      IDPR,PPR(5)   identity, momentum of projectile                  *
C      IDTA,PTA(5)   identity, momentum of target                      *
C      MODE  = 1     inelastic interaction                             *
C            = 2     elastic   interaction                             *
C Revised version of the original FHAD.                                *
C This version dated 27.10.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amfsp2 , amp2 , amt2 , bgta , cx , cy , cz , 
     &                 dum , ONE , p1in , p1out , p2in , p2out , Ppr , 
     &                 ppr1 , pprto1 , Pta , ptofsp , px , py
      DOUBLE PRECISION pz , TINY1 , TINY10 , TINY2 , TINY3 , TINY5 , 
     &                 xm1 , xm2 , ZERO
      INTEGER i , i1 , i2 , idhpr , idhta , Idpr , Idta , idum , 
     &        imcorr , inthad , Irej , irej1 , k , kcorr , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,TINY5=1.0D-5,TINY3=1.0D-3,
     &           TINY2=1.0D-2,TINY1=1.0D-1,ONE=1.0D0)
 
      LOGICAL lcorr , lmssg
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
C final state after inc step
      INCLUDE 'inc/dtcapa'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C output-common for DHADRI/ELHAIN
C final state from HADRIN interaction
      INCLUDE 'inc/hnfspa'
 
      DIMENSION Ppr(5) , ppr1(5) , Pta(5) , bgta(4) , p1in(4) , p2in(4)
     &          , p1out(4) , p2out(4) , imcorr(2)
 
      DATA lmssg/.TRUE./
 
      Irej = 0
      NFSp = 0
      kcorr = 0
      imcorr(1) = 0
      imcorr(2) = 0
      lcorr = .FALSE.
 
C   dump initial particles for energy-momentum cons. check
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(Ppr(1),Ppr(2),Ppr(3),Ppr(4),1,idum,idum)
         CALL DT_EVTEMC(Pta(1),Pta(2),Pta(3),Pta(4),2,idum,idum)
      END IF
 
      amp2 = Ppr(4)**2 - Ppr(1)**2 - Ppr(2)**2 - Ppr(3)**2
      amt2 = Pta(4)**2 - Pta(1)**2 - Pta(2)**2 - Pta(3)**2
      IF ( (amp2.LT.ZERO) .OR. (amt2.LT.ZERO) .OR. 
     &     (ABS(amp2-AAM(Idpr)**2).GT.TINY5) .OR. 
     &     (ABS(amt2-AAM(Idta)**2).GT.TINY5) ) THEN
 
         IF ( lmssg .AND. (IOUlev(3).GT.0) .AND. LPRi.GT.4 )
     &        WRITE (LOUt,99010) amp2 , AAM(Idpr)**2 , amt2 , AAM(Idta)
     &        **2
99010    FORMAT (1X,'HADRIN:   warning! inconsistent projectile/target',
     &           ' mass',/,20X,'AMP2 = ',E12.4,', AAM(IDPR)**2 = ',
     &           E12.4,/,20X,'AMT2 = ',E12.4,', AAM(IDTA)**2 = ',E12.4)
         lmssg = .FALSE.
         lcorr = .TRUE.
      END IF
 
C convert initial state particles into particles which can be
C handled by HADRIN
      idhpr = Idpr
      idhta = Idta
      IF ( (idhpr.LE.0) .OR. (idhpr.GE.111) .OR. lcorr ) THEN
         IF ( (idhpr.LE.0) .OR. (idhpr.GE.111) ) idhpr = 1
         DO k = 1 , 4
            p1in(k) = Ppr(k)
            p2in(k) = Pta(k)
         END DO
         xm1 = AAM(idhpr)
         xm2 = AAM(idhta)
         CALL DT_MASHEL(p1in,p2in,xm1,xm2,p1out,p2out,irej1)
         IF ( irej1.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &            'HADRIN:   inconsistent mass trsf.'
            GOTO 200
         END IF
         DO k = 1 , 4
            Ppr(k) = p1out(k)
            Pta(k) = p2out(k)
         END DO
         Ppr(5) = SQRT(Ppr(4)**2-Ppr(1)**2-Ppr(2)**2-Ppr(3)**2)
         Pta(5) = SQRT(Pta(4)**2-Pta(1)**2-Pta(2)**2-Pta(3)**2)
      END IF
 
C Lorentz-parameter for trafo into rest-system of target
      DO k = 1 , 4
         bgta(k) = Pta(k)/Pta(5)
      END DO
C transformation of projectile into rest-system of target
      CALL DT_DALTRA(bgta(4),-bgta(1),-bgta(2),-bgta(3),Ppr(1),Ppr(2),
     &               Ppr(3),Ppr(4),pprto1,ppr1(1),ppr1(2),ppr1(3),
     &               ppr1(4))
 
C direction cosines of projectile in target rest system
      cx = ppr1(1)/pprto1
      cy = ppr1(2)/pprto1
      cz = ppr1(3)/pprto1
 
C sample inelastic interaction
      IF ( Mode.EQ.1 ) THEN
         CALL DT_DHADRI(idhpr,pprto1,ppr1(4),cx,cy,cz,idhta)
         IF ( IRH.EQ.1 ) GOTO 100
C sample elastic interaction
      ELSE IF ( Mode.EQ.2 ) THEN
         CALL DT_ELHAIN(idhpr,pprto1,ppr1(4),cx,cy,cz,idhta,irej1)
         IF ( irej1.NE.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 1 in HADRIN'
            GOTO 200
         END IF
         IF ( IRH.EQ.1 ) GOTO 100
      ELSE
Caf: 1 line, make the compiler happy
         inthad = 0
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) Mode , inthad
99020    FORMAT (1X,'HADRIN:   warning! inconsistent interaction mode',
     &           I4,' (INTHAD =',I4,')')
         GOTO 200
      END IF
 
C transform final state particles back into Lab.
      DO i = 1 , IRH
         NFSp = NFSp + 1
         px = CXRh(i)*PLRh(i)
         py = CYRh(i)*PLRh(i)
         pz = CZRh(i)*PLRh(i)
         CALL DT_DALTRA(bgta(4),bgta(1),bgta(2),bgta(3),px,py,pz,ELRh(i)
     &                  ,ptofsp,PFSp(1,NFSp),PFSp(2,NFSp),PFSp(3,NFSp),
     &                  PFSp(4,NFSp))
         IDFsp(NFSp) = ITRh(i)
         amfsp2 = PFSp(4,NFSp)**2 - PFSp(1,NFSp)**2 - PFSp(2,NFSp)
     &            **2 - PFSp(3,NFSp)**2
         IF ( amfsp2.LT.-TINY3 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) IDFsp(NFSp) , 
     &           PFSp(1,NFSp) , PFSp(2,NFSp) , PFSp(3,NFSp) , 
     &           PFSp(4,NFSp) , amfsp2
99030       FORMAT (1X,'HADRIN:   warning! final state particle (id = ',
     &              I2,') with negative mass^2',/,1X,5E12.4)
            GOTO 200
         ELSE
            PFSp(5,NFSp) = SQRT(ABS(amfsp2))
            IF ( ABS(PFSp(5,NFSp)-AAM(IDFsp(NFSp))).GT.TINY1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) IDFsp(NFSp) , 
     &              AAM(IDFsp(NFSp)) , PFSp(5,NFSp)
99040          FORMAT (1X,'HADRIN:   warning! final state particle',
     &                 ' (id = ',I2,') with inconsistent mass',/,1X,
     &                 2E12.4)
               kcorr = kcorr + 1
               IF ( kcorr.GT.2 ) GOTO 200
               imcorr(kcorr) = NFSp
            END IF
         END IF
C   dump final state particles for energy-momentum cons. check
         IF ( LEMcck ) CALL DT_EVTEMC(-PFSp(1,i),-PFSp(2,i),-PFSp(3,i),
     &        -PFSp(4,i),2,idum,idum)
      END DO
 
C transform momenta on mass shell in case of inconsistencies in
C HADRIN
      IF ( kcorr.GT.0 ) THEN
         IF ( kcorr.EQ.2 ) THEN
            i1 = imcorr(1)
            i2 = imcorr(2)
         ELSE IF ( imcorr(1).EQ.1 ) THEN
            i1 = 1
            i2 = 2
         ELSE
            i1 = 1
            i2 = imcorr(1)
         END IF
         IF ( LEMcck ) CALL DT_EVTEMC(PFSp(1,i1),PFSp(2,i1),PFSp(3,i1),
     &        PFSp(4,i1),2,idum,idum)
         IF ( LEMcck ) CALL DT_EVTEMC(PFSp(1,i2),PFSp(2,i2),PFSp(3,i2),
     &        PFSp(4,i2),2,idum,idum)
         DO k = 1 , 4
            p1in(k) = PFSp(k,i1)
            p2in(k) = PFSp(k,i2)
         END DO
         xm1 = AAM(IDFsp(i1))
         xm2 = AAM(IDFsp(i2))
         CALL DT_MASHEL(p1in,p2in,xm1,xm2,p1out,p2out,irej1)
         IF ( irej1.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &            'HADRIN:   inconsistent mass trsf.'
C           GOTO 9999
         END IF
         DO k = 1 , 4
            PFSp(k,i1) = p1out(k)
            PFSp(k,i2) = p2out(k)
         END DO
         PFSp(5,i1) = SQRT(PFSp(4,i1)**2-PFSp(1,i1)**2-PFSp(2,i1)
     &                **2-PFSp(3,i1)**2)
         PFSp(5,i2) = SQRT(PFSp(4,i2)**2-PFSp(1,i2)**2-PFSp(2,i2)
     &                **2-PFSp(3,i2)**2)
C   dump final state particles for energy-momentum cons. check
         IF ( LEMcck ) CALL DT_EVTEMC(-PFSp(1,i1),-PFSp(2,i1),
     &        -PFSp(3,i1),-PFSp(4,i1),2,idum,idum)
         IF ( LEMcck ) CALL DT_EVTEMC(-PFSp(1,i2),-PFSp(2,i2),
     &        -PFSp(3,i2),-PFSp(4,i2),2,idum,idum)
      END IF
 
C check energy-momentum conservation
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(dum,dum,dum,dum,4,102,irej1)
         IF ( irej1.NE.0 ) GOTO 200
      END IF
 
      RETURN
 
 100  Irej = 2
      RETURN
 
 200  Irej = 1
      END SUBROUTINE
