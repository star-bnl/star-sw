
      SUBROUTINE DT_ABSORP(Idcas,Pcas,Ncas,Nspe,Idspe,Idxspe,Mode,Irej)
 
C***********************************************************************
C Two-nucleon absorption of antiprotons, pi-, and K-.                  *
C Antiproton absorption is handled by HADRIN.                          *
C The following channels for meson-absorption are considered:          *
C          pi- + p + p ---> n + p                                      *
C          pi- + p + n ---> n + n                                      *
C          K-  + p + p ---> sigma+ + n / Lam + p / sigma0 + p          *
C          K-  + p + n ---> sigma- + n / Lam + n / sigma0 + n          *
C          K-  + p + p ---> sigma- + n                                 *
C      IDCAS, PCAS   identity, momentum of particle to be absorbed     *
C      NCAS =  1     intranuclear cascade in projectile                *
C           = -1     intranuclear cascade in target                    *
C      NSPE          number of spectator nucleons involved             *
C      IDXSPE(2)     DTEVT1-indices of spectator nucleons involved     *
C Revised version of the original STOPIK written by HJM and J. Ranft.  *
C This version dated 24.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION am3p , bg3p , codf , coff , DT_RNDM , dum , 
     &                 ecmf , ONE , ONETHI , p3p , Pcas , pcmf , pspe , 
     &                 pspe1 , ptofsp , ptot3p , px , py , pz , r
      DOUBLE PRECISION sdf , siff , TINY10 , TINY5 , TWOTHI
      INTEGER i , Idcas , Idspe , idum , Idxspe , Irej , irej1 , k , 
     &        Mode , Ncas , Nspe , nucas
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY5=1.0D-5,ONE=1.0D0,ONETHI=0.3333D0,
     &           TWOTHI=0.6666D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C final state after inc step
      INCLUDE 'inc/dtcapa'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION Pcas(5) , Idxspe(2) , Idspe(2) , pspe(2,5) , pspe1(5) , 
     &          ptot3p(4) , bg3p(4) , ecmf(2) , pcmf(2) , codf(2) , 
     &          coff(2) , siff(2)
 
      Irej = 0
      NFSp = 0
 
C skip particles others than ap, pi-, K- for mode=0
      IF ( (Mode.EQ.0) .AND. (Idcas.NE.2) .AND. (Idcas.NE.14) .AND. 
     &     (Idcas.NE.16) ) RETURN
C skip particles others than pions for mode=1
C (2-nucleon absorption in intranuclear cascade)
      IF ( (Mode.EQ.1) .AND. (Idcas.NE.13) .AND. (Idcas.NE.14) .AND. 
     &     (Idcas.NE.23) ) RETURN
 
      nucas = Ncas
      IF ( nucas.EQ.-1 ) nucas = 2
 
      IF ( Mode.EQ.0 ) THEN
C scan spectator nucleons for nucleons being able to "absorb"
         Nspe = 0
         Idxspe(1) = 0
         Idxspe(2) = 0
         DO i = 1 , NHKk
            IF ( (ISThkk(i).EQ.12+nucas) .OR. (ISThkk(i).EQ.14+nucas) )
     &           THEN
               Nspe = Nspe + 1
               Idxspe(Nspe) = i
               Idspe(Nspe) = IDBam(i)
               IF ( (Nspe.EQ.1) .AND. (Idcas.EQ.2) ) GOTO 100
               IF ( Nspe.EQ.2 ) THEN
                  IF ( (Idcas.NE.14) .OR. (Idspe(1).NE.8) .OR. 
     &                 (Idspe(2).NE.8) ) GOTO 100
C    there is no pi-+n+n channel
                  Nspe = 1
               END IF
            END IF
         END DO
 
      END IF
C transform excited projectile nucleons (status=15) into proj. rest s.
 100  DO i = 1 , Nspe
         DO k = 1 , 5
            pspe(i,k) = PHKk(k,Idxspe(i))
         END DO
      END DO
 
C antiproton absorption
      IF ( (Idcas.EQ.2) .AND. (Nspe.GE.1) ) THEN
         DO k = 1 , 5
            pspe1(k) = pspe(1,k)
         END DO
         CALL DT_HADRIN(Idcas,Pcas,Idspe(1),pspe1,1,irej1)
         IF ( irej1.NE.0 ) GOTO 200
 
C meson absorption
      ELSE IF ( ((Idcas.EQ.13) .OR. (Idcas.EQ.14) .OR. (Idcas.EQ.23)
     &          .OR. (Idcas.EQ.16)) .AND. (Nspe.GE.2) ) THEN
         IF ( Idcas.EQ.14 ) THEN
C   pi- absorption
            IDFsp(1) = 8
            IDFsp(2) = 8
            IF ( (Idspe(1).EQ.1) .AND. (Idspe(2).EQ.1) ) IDFsp(2) = 1
         ELSE IF ( Idcas.EQ.13 ) THEN
C   pi+ absorption
            IDFsp(1) = 1
            IDFsp(2) = 1
            IF ( (Idspe(1).EQ.8) .AND. (Idspe(2).EQ.8) ) IDFsp(2) = 8
         ELSE IF ( Idcas.EQ.23 ) THEN
C   pi0 absorption
            IDFsp(1) = Idspe(1)
            IDFsp(2) = Idspe(2)
         ELSE IF ( Idcas.EQ.16 ) THEN
C   K- absorption
            r = DT_RNDM(ONE)
            IF ( (Idspe(1).EQ.1) .AND. (Idspe(2).EQ.1) ) THEN
               IF ( r.LT.ONETHI ) THEN
                  IDFsp(1) = 21
                  IDFsp(2) = 8
               ELSE IF ( r.LT.TWOTHI ) THEN
                  IDFsp(1) = 17
                  IDFsp(2) = 1
               ELSE
                  IDFsp(1) = 22
                  IDFsp(2) = 1
               END IF
            ELSE IF ( (Idspe(1).EQ.8) .AND. (Idspe(2).EQ.8) ) THEN
               IDFsp(1) = 20
               IDFsp(2) = 8
            ELSE IF ( r.LT.ONETHI ) THEN
               IDFsp(1) = 20
               IDFsp(2) = 1
            ELSE IF ( r.LT.TWOTHI ) THEN
               IDFsp(1) = 17
               IDFsp(2) = 8
            ELSE
               IDFsp(1) = 22
               IDFsp(2) = 8
            END IF
         END IF
C   dump initial particles for energy-momentum cons. check
         IF ( LEMcck ) THEN
            CALL DT_EVTEMC(Pcas(1),Pcas(2),Pcas(3),Pcas(4),1,idum,idum)
            CALL DT_EVTEMC(pspe(1,1),pspe(1,2),pspe(1,3),pspe(1,4),2,
     &                     idum,idum)
            CALL DT_EVTEMC(pspe(2,1),pspe(2,2),pspe(2,3),pspe(2,4),2,
     &                     idum,idum)
         END IF
C   get Lorentz-parameter of 3 particle initial state
         DO k = 1 , 4
            ptot3p(k) = Pcas(k) + pspe(1,k) + pspe(2,k)
         END DO
         p3p = SQRT(ptot3p(1)**2+ptot3p(2)**2+ptot3p(3)**2)
         am3p = SQRT((ptot3p(4)-p3p)*(ptot3p(4)+p3p))
         DO k = 1 , 4
            bg3p(k) = ptot3p(k)/MAX(am3p,TINY10)
         END DO
C   2-particle decay of the 3-particle compound system
         CALL DT_DTWOPD(am3p,ecmf(1),ecmf(2),pcmf(1),pcmf(2),codf(1),
     &                  coff(1),siff(1),codf(2),coff(2),siff(2),
     &                  AAM(IDFsp(1)),AAM(IDFsp(2)))
         DO i = 1 , 2
            sdf = SQRT((ONE-codf(i))*(ONE+codf(i)))
            px = pcmf(i)*coff(i)*sdf
            py = pcmf(i)*siff(i)*sdf
            pz = pcmf(i)*codf(i)
            CALL DT_DALTRA(bg3p(4),bg3p(1),bg3p(2),bg3p(3),px,py,pz,
     &                     ecmf(i),ptofsp,PFSp(1,i),PFSp(2,i),PFSp(3,i),
     &                     PFSp(4,i))
            PFSp(5,i) = SQRT((PFSp(4,i)-ptofsp)*(PFSp(4,i)+ptofsp))
C   check consistency of kinematics
            IF ( ABS(AAM(IDFsp(i))-PFSp(5,i)).GT.TINY5 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99010) IDFsp(i) , 
     &              AAM(IDFsp(i)) , PFSp(5,i)
99010          FORMAT (1X,'ABSORP:   warning! inconsistent',
     &                 ' tree-particle kinematics',/,20X,'id: ',I3,
     &                 ' AAM = ',E10.4,' MFSP = ',E10.4)
            END IF
C   dump final state particles for energy-momentum cons. check
            IF ( LEMcck ) CALL DT_EVTEMC(-PFSp(1,i),-PFSp(2,i),
     &           -PFSp(3,i),-PFSp(4,i),2,idum,idum)
         END DO
         NFSp = 2
         IF ( LEMcck ) THEN
            CALL DT_EVTEMC(dum,dum,dum,dum,3,100,irej1)
            IF ( irej1.NE.0 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'ABSORB: EMC ' , 
     &              AAM(IDFsp(1)) , AAM(IDFsp(2)) , am3p
               GOTO 200
            END IF
         END IF
      ELSE
 
         IF ( LPRi.GT.4 .AND. IOUlev(3).GT.0 ) WRITE (LOUt,99020)
     &        Idcas , Nspe
99020    FORMAT (1X,'ABSORP:   warning! absorption for particle ',I3,
     &           ' impossible',/,20X,'too few spectators (',I2,')')
         Nspe = 0
      END IF
 
      RETURN
 
 
 200  IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &      'rejected 1 in ABSORP'
      Irej = 1
      END SUBROUTINE
