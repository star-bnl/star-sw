
      SUBROUTINE DT_XSTABL(What,Ixsqel,Iratio)
 
      IMPLICIT NONE
      DOUBLE PRECISION adebin , adqbin , aehi , aelo , aq2hi , aq2lo , 
     &                 debins , DLARGE , dqbins , e , ecm , edel , 
     &                 edqe , eela , ehi , elo , epro , eqe2 , eqep , 
     &                 eqet
      DOUBLE PRECISION etot , OHALF , ONE , plab , ppn0 , q2hi , q2i , 
     &                 q2lo , sumfra , TINY10 , TINY2 , TWO , What , 
     &                 xdel , xdqe , xela , xi , xpro , xpro1 , xqe2
      DOUBLE PRECISION xqep , xqet , xtot , ypro , ZERO
      INTEGER i , ic , iit , Iratio , Ixsqel , j , nebins , nqbins
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0,TWO=2.0D0)
      LOGICAL llab , lelog , lqlog
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      DIMENSION What(6)
 
      llab = (What(1).GT.ZERO) .OR. (What(2).GT.ZERO)
      elo = ABS(What(1))
      ehi = ABS(What(2))
      IF ( elo.GT.ehi ) elo = ehi
      lelog = What(3).LT.ZERO
      nebins = MAX(INT(ABS(What(3))),1)
      debins = (ehi-elo)/DBLE(nebins)
      IF ( lelog ) THEN
         aelo = LOG10(elo)
         aehi = LOG10(ehi)
         adebin = (aehi-aelo)/DBLE(nebins)
      END IF
      q2lo = What(4)
      q2hi = What(5)
      IF ( q2lo.GT.q2hi ) q2lo = q2hi
      lqlog = What(6).LT.ZERO
      nqbins = MAX(INT(ABS(What(6))),1)
      dqbins = (q2hi-q2lo)/DBLE(nqbins)
      IF ( lqlog ) THEN
         aq2lo = LOG10(q2lo)
         aq2hi = LOG10(q2hi)
         adqbin = (aq2hi-aq2lo)/DBLE(nqbins)
      END IF
 
      IF ( elo.EQ.ehi ) nebins = 0
      IF ( q2lo.EQ.q2hi ) nqbins = 0
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010) elo , ehi , llab , Ixsqel , 
     &                        q2lo , q2hi , IJProj , IP , IT
99010 FORMAT (/,1X,'XSTABL:  E_lo  =',E10.3,' GeV  E_hi  =',E10.3,
     &        ' GeV     Lab = ',L1,'  qel: ',I2,/,10X,'Q2_lo =',F10.5,
     &        ' GeV^2  Q2_hi =',F10.5,' GeV^2',/,10X,'id_p = ',I2,
     &        '   A_p = ',I3,'   A_t = ',I3,/)
 
C     IF (IJPROJ.NE.7) THEN
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,/)')
     &                         '(E,STOT,SELA,SQEP,SQET,SQE2,SINE,SPROD)'
C normalize fractions of emulsion components
      IF ( NCOmpo.GT.0 ) THEN
         sumfra = ZERO
         DO i = 1 , NCOmpo
            sumfra = sumfra + EMUfra(i)
         END DO
         IF ( sumfra.GT.ZERO ) THEN
            DO i = 1 , NCOmpo
               EMUfra(i) = EMUfra(i)/sumfra
            END DO
         END IF
      END IF
C     ELSE
C        WRITE(LOUT,'(1X,A,/)') '(Q2,E,STOT,ETOT,SIN,EIN,STOT0)'
C     ENDIF
      DO i = 1 , nebins + 1
         IF ( lelog ) THEN
            e = 10**(aelo+DBLE(i-1)*adebin)
         ELSE
            e = elo + DBLE(i-1)*debins
         END IF
         DO j = 1 , nqbins + 1
            IF ( lqlog ) THEN
               Q2 = 10**(aq2lo+DBLE(j-1)*adqbin)
            ELSE
               Q2 = q2lo + DBLE(j-1)*dqbins
            END IF
C            IF (IJPROJ.NE.7) THEN
            IF ( llab ) THEN
               plab = ZERO
               ecm = ZERO
               CALL DT_LTINI(IJProj,1,e,ppn0,ecm,0)
            ELSE
               ecm = e
            END IF
            xi = ZERO
            q2i = ZERO
            IF ( IJProj.EQ.7 ) q2i = Q2
            IF ( NCOmpo.GT.0 ) THEN
               DO ic = 1 , NCOmpo
                  iit = IEMuma(ic)
                  CALL DT_XSGLAU(IP,iit,IJProj,xi,q2i,ecm,1,1,-ic)
               END DO
            ELSE
               CALL DT_XSGLAU(IP,IT,IJProj,xi,q2i,ecm,1,1,-1)
C                 CALL AMPLIT(IP,IT,IJPROJ,XI,Q2I,ECM,1,1,1)
            END IF
            IF ( NCOmpo.GT.0 ) THEN
               xtot = ZERO
               etot = ZERO
               xela = ZERO
               eela = ZERO
               xqep = ZERO
               eqep = ZERO
               xqet = ZERO
               eqet = ZERO
               xqe2 = ZERO
               eqe2 = ZERO
               xpro = ZERO
               epro = ZERO
               xpro1 = ZERO
               xdel = ZERO
               edel = ZERO
               xdqe = ZERO
               edqe = ZERO
               DO ic = 1 , NCOmpo
                  xtot = xtot + EMUfra(ic)*XSTot(1,1,ic)
                  etot = etot + EMUfra(ic)*XETot(1,1,ic)**2
                  xela = xela + EMUfra(ic)*XSEla(1,1,ic)
                  eela = eela + EMUfra(ic)*XEEla(1,1,ic)**2
                  xqep = xqep + EMUfra(ic)*XSQep(1,1,ic)
                  eqep = eqep + EMUfra(ic)*XEQep(1,1,ic)**2
                  xqet = xqet + EMUfra(ic)*XSQet(1,1,ic)
                  eqet = eqet + EMUfra(ic)*XEQet(1,1,ic)**2
                  xqe2 = xqe2 + EMUfra(ic)*XSQe2(1,1,ic)
                  eqe2 = eqe2 + EMUfra(ic)*XEQe2(1,1,ic)**2
                  xpro = xpro + EMUfra(ic)*XSPro(1,1,ic)
                  epro = epro + EMUfra(ic)*XEPro(1,1,ic)**2
                  xdel = xdel + EMUfra(ic)*XSDel(1,1,ic)
                  edel = edel + EMUfra(ic)*XEDel(1,1,ic)**2
                  xdqe = xdqe + EMUfra(ic)*XSDqe(1,1,ic)
                  edqe = edqe + EMUfra(ic)*XEDqe(1,1,ic)**2
                  ypro = XSTot(1,1,ic) - XSEla(1,1,ic) - XSQep(1,1,ic)
     &                   - XSQet(1,1,ic) - XSQe2(1,1,ic)
                  xpro1 = xpro1 + EMUfra(ic)*ypro
               END DO
               etot = SQRT(etot)
               eela = SQRT(eela)
               eqep = SQRT(eqep)
               eqet = SQRT(eqet)
               eqe2 = SQRT(eqe2)
               epro = SQRT(epro)
               edel = SQRT(edel)
               edqe = SQRT(edqe)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(8E9.3)') e , xtot , xela , 
     &              xqep , xqet , xqe2 , xpro , xpro1
C                 WRITE(LOUT,'(4E9.3)')
C    &               E,XDEL,XDQE,XDEL+XDQE
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(11E10.3)') e , 
     &              XSTot(1,1,1) , XSEla(1,1,1) , XSQep(1,1,1) , 
     &              XSQet(1,1,1) , XSQe2(1,1,1) , XSPro(1,1,1) , 
     &              XSTot(1,1,1) - XSEla(1,1,1) - XSQep(1,1,1)
     &              - XSQet(1,1,1) - XSQe2(1,1,1) , XSDel(1,1,1) , 
     &              XSDqe(1,1,1) , XSDel(1,1,1) + XSDqe(1,1,1)
C                 WRITE(LOUT,'(4E9.3)') E,XSDEL(1,1,1),XSDQE(1,1,1),
C    &                                    XSDEL(1,1,1)+XSDQE(1,1,1)
            END IF
C            ELSE
C               IF (LLAB) THEN
C                  IF (IT.GT.1) THEN
C                     IF (IXSQEL.EQ.0) THEN
CC                       CALL DT_SIGGA(IT,  Q2, E,ZERO,ZERO,
CC                       CALL DT_SIGGA(IT,   E,Q2,ZERO,ZERO,
C                        CALL DT_SIGGA(IT,ZERO,Q2,ZERO,E,
C     &                             STOT,ETOT,SIN,EIN,STOT0)
C                        IF (IRATIO.EQ.1) THEN
C                           CALL DT_SIGGP(  Q2, E,ZERO,ZERO,STGP,SIGP,SDGP)
CC                          CALL DT_SIGGP(   E,Q2,ZERO,ZERO,STGP,SIGP,SDGP)
CC                          CALL DT_SIGGP(ZERO,Q2,ZERO,E,STGP,SIGP,SDGP)
C*!! save cross sections
C                           STOTA = STOT
C                           ETOTA = ETOT
C                           STOTP = STGP
C*!!
C                           STOT  = STOT/(DBLE(IT)*STGP)
C                           SIN   =  SIN/(DBLE(IT)*SIGP)
C                           STOT0 = STGP
C                           ETOT  = ZERO
C                           EIN   = ZERO
C                        ENDIF
C                     ELSE
C                        WRITE(LOUT,*)
C     &                  ' XSTABL:  qel. xs. not implemented for nuclei'
C                        STOP
C                     ENDIF
C                  ELSE
C                     ETOT = ZERO
C                     EIN  = ZERO
C                     STOT0= ZERO
C                     IF (IXSQEL.EQ.0) THEN
C                        CALL DT_SIGGP(ZERO,Q2,ZERO,E,STOT,SIN,SDIR)
C                     ELSE
C                       SIN = ZERO
C                       CALL DT_SIGVEL(ZERO,Q2,ZERO,E,IXSQEL,STOT,SIN,STOT0)
C                     ENDIF
C                  ENDIF
C               ELSE
C                  IF (IT.GT.1) THEN
C                     IF (IXSQEL.EQ.0) THEN
C                        CALL DT_SIGGA(IT,ZERO,Q2,E,ZERO,
C     &                             STOT,ETOT,SIN,EIN,STOT0)
C                        IF (IRATIO.EQ.1) THEN
C                           CALL DT_SIGGP(ZERO,Q2,E,ZERO,STGP,SIGP,SDGP)
C*!! save cross sections
C                           STOTA = STOT
C                           ETOTA = ETOT
C                           STOTP = STGP
C*!!
C                           STOT  = STOT/(DBLE(IT)*STGP)
C                           SIN   =  SIN/(DBLE(IT)*SIGP)
C                           STOT0 = STGP
C                           ETOT  = ZERO
C                           EIN   = ZERO
C                        ENDIF
C                     ELSE
C                        WRITE(LOUT,*)
C     &                  ' XSTABL:  qel. xs. not implemented for nuclei'
C                        STOP
C                     ENDIF
C                  ELSE
C                     ETOT = ZERO
C                     EIN  = ZERO
C                     STOT0= ZERO
C                     IF (IXSQEL.EQ.0) THEN
C                        CALL DT_SIGGP(ZERO,Q2,E,ZERO,STOT,SIN,SDIR)
C                     ELSE
C                       SIN = ZERO
C                       CALL DT_SIGVEL(ZERO,Q2,E,ZERO,IXSQEL,STOT,SIN,STOT0)
C                     ENDIF
C                  ENDIF
C               ENDIF
CC              WRITE(LOUT,'(1X,7E10.3)')Q2,E,STOT,STOTA,ETOTA,STOTP,ZERO
CC              WRITE(LOUT,'(1X,7E10.3)')Q2,E,STOT,ETOT,SIN,EIN,SDIR
CC              WRITE(LOUT,'(1X,7E10.3)')Q2,E,STOT,ETOT,SIN,EIN,STOT0
C               WRITE(LOUT,'(1X,6E10.3)')Q2,E,STOT,ETOT,SIN,EIN
C            ENDIF
         END DO
      END DO
 
      END SUBROUTINE
