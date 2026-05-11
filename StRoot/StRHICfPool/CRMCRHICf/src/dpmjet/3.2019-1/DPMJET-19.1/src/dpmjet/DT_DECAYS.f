
      SUBROUTINE DT_DECAYS(Pin,Idxin,Pout,Idxout,Nsec,Irej)
 
C***********************************************************************
C Resonance-decay.                                                     *
C This subroutine replaces DDECAY/DECHKK.                              *
C             PIN(4)      4-momentum of resonance          (input)     *
C             IDXIN       BAMJET-index of resonance        (input)     *
C             POUT(20,4)  4-momenta of decay-products      (output)    *
C             IDXOUT(20)  BAMJET-indices of decay-products (output)    *
C             NSEC        number of secondaries            (output)    *
C Adopted from the original version DECHKK.                            *
C This version dated 09.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION bgam , codf , coff , dcos , dcosf , DT_RNDM , 
     &                 dum , ef , gam , pf , pff , pi , Pin , Pout , 
     &                 ptot , siff , TINY17
      INTEGER i , idum , idx , idxi , Idxin , Idxout , idxstk , Irej , 
     &        irej1 , istab , j , kchan , ndec , Nsec , nstk
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY17=1.0D-17)
 
C HADRIN: decay channel information
      INCLUDE 'inc/hndech'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      DIMENSION Pin(4) , pi(20,4) , Pout(20,4) , Idxout(20) , ef(3) , 
     &          pf(3) , pff(3) , idxstk(20) , idx(3) , codf(3) , coff(3)
     &          , siff(3) , dcos(3) , dcosf(3)
 
C ISTAB = 1 strong and weak decays
C       = 2 strong decays only
C       = 3 strong decays, weak decays for charmed particles and tau
C           leptons only
      DATA istab/2/
 
      Irej = 0
      Nsec = 0
C put initial resonance to stack
      nstk = 1
      idxstk(nstk) = Idxin
      DO i = 1 , 4
         pi(nstk,i) = Pin(i)
      END DO
 
C store initial configuration for energy-momentum cons. check
      IF ( LEMcck ) CALL DT_EVTEMC(pi(nstk,1),pi(nstk,2),pi(nstk,3),
     &     pi(nstk,4),1,idum,idum)
 
C get particle from stack
 100  idxi = idxstk(nstk)
C skip stable particles
      IF ( istab.EQ.1 ) THEN
         IF ( (idxi.EQ.135) .OR. (idxi.EQ.136) ) GOTO 300
         IF ( (idxi.GE.1) .AND. (idxi.LE.7) ) GOTO 300
      ELSE IF ( istab.EQ.2 ) THEN
         IF ( (idxi.GE.1) .AND. (idxi.LE.30) ) GOTO 300
         IF ( (idxi.GE.97) .AND. (idxi.LE.103) ) GOTO 300
         IF ( (idxi.GE.115) .AND. (idxi.LE.122) ) GOTO 300
         IF ( (idxi.GE.131) .AND. (idxi.LE.136) ) GOTO 300
         IF ( idxi.EQ.109 ) GOTO 300
         IF ( (idxi.GE.137) .AND. (idxi.LE.160) ) GOTO 300
      ELSE IF ( istab.EQ.3 ) THEN
         IF ( (idxi.GE.1) .AND. (idxi.LE.23) ) GOTO 300
         IF ( (idxi.GE.97) .AND. (idxi.LE.103) ) GOTO 300
         IF ( (idxi.GE.109) .AND. (idxi.LE.115) ) GOTO 300
         IF ( (idxi.GE.133) .AND. (idxi.LE.136) ) GOTO 300
      END IF
 
C calculate direction cosines and Lorentz-parameter of decaying part.
      ptot = SQRT(pi(nstk,1)**2+pi(nstk,2)**2+pi(nstk,3)**2)
      ptot = MAX(ptot,TINY17)
      DO i = 1 , 3
         dcos(i) = pi(nstk,i)/ptot
      END DO
      gam = pi(nstk,4)/AAM(idxi)
      bgam = ptot/AAM(idxi)
 
C get decay-channel
      kchan = K1(idxi) - 1
 200  kchan = kchan + 1
      IF ( (DT_RNDM(gam)-TINY17).GT.WT(kchan) ) GOTO 200
 
C identities of secondaries
      idx(1) = NZK(kchan,1)
      idx(2) = NZK(kchan,2)
      IF ( idx(2).LT.1 ) THEN
 
         Irej = 1
         GOTO 99999
      ELSE
         idx(3) = NZK(kchan,3)
 
C handle decay in rest system of decaying particle
         IF ( idx(3).EQ.0 ) THEN
C   two-particle decay
            ndec = 2
            CALL DT_DTWOPD(AAM(idxi),ef(1),ef(2),pf(1),pf(2),codf(1),
     &                     coff(1),siff(1),codf(2),coff(2),siff(2),
     &                     AAM(idx(1)),AAM(idx(2)))
         ELSE
C   three-particle decay
            ndec = 3
            CALL DT_DTHREP(AAM(idxi),ef(1),ef(2),ef(3),pf(1),pf(2),
     &                     pf(3),codf(1),coff(1),siff(1),codf(2),coff(2)
     &                     ,siff(2),codf(3),coff(3),siff(3),AAM(idx(1)),
     &                     AAM(idx(2)),AAM(idx(3)))
         END IF
         nstk = nstk - 1
 
C transform decay products back
         DO i = 1 , ndec
            nstk = nstk + 1
            CALL DT_DTRAFO(gam,bgam,dcos(1),dcos(2),dcos(3),codf(i),
     &                     coff(i),siff(i),pf(i),ef(i),pff(i),dcosf(1),
     &                     dcosf(2),dcosf(3),pi(nstk,4))
C add particle to stack
            idxstk(nstk) = idx(i)
            DO j = 1 , 3
               pi(nstk,j) = dcosf(j)*pff(i)
            END DO
         END DO
         GOTO 100
      END IF
 
C stable particle, put to output-arrays
 300  Nsec = Nsec + 1
      DO i = 1 , 4
         Pout(Nsec,i) = pi(nstk,i)
      END DO
      Idxout(Nsec) = idxstk(nstk)
C store secondaries for energy-momentum conservation check
      IF ( LEMcck ) CALL DT_EVTEMC(-Pout(Nsec,1),-Pout(Nsec,2),
     &     -Pout(Nsec,3),-Pout(Nsec,4),2,idum,idum)
      nstk = nstk - 1
      IF ( nstk.GT.0 ) GOTO 100
 
C check energy-momentum conservation
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(dum,dum,dum,dum,3,5,irej1)
         IF ( irej1.NE.0 ) Irej = 1
      END IF
 
99999 END SUBROUTINE
