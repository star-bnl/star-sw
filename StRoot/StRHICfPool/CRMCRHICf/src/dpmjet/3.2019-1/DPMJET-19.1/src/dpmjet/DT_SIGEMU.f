
      SUBROUTINE DT_SIGEMU
 
C***********************************************************************
C Combined cross section for target compounds.                         *
C This version dated 6.4.98   is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DLARGE , errdel , errdqe , errela , errpro , 
     &                 errqe2 , errqep , errqet , errtot , OHALF , ONE , 
     &                 sigdel , sigdqe , sigela , sigpro , sigqe2 , 
     &                 sigqep , sigqet , sigtot , TINY10
      DOUBLE PRECISION TINY2 , ZERO
      INTEGER ic , ie , iq
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
 
      IF ( MCGene.NE.4 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A)')
     &         ' DT_SIGEMU:    Combined cross sections'
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(15X,A)')
     &         '-----------------------'
      END IF
      DO ie = 1 , NEBini
         DO iq = 1 , NQBini
            sigtot = ZERO
            sigela = ZERO
            sigqep = ZERO
            sigqet = ZERO
            sigqe2 = ZERO
            sigpro = ZERO
            sigdel = ZERO
            sigdqe = ZERO
            errtot = ZERO
            errela = ZERO
            errqep = ZERO
            errqet = ZERO
            errqe2 = ZERO
            errpro = ZERO
            errdel = ZERO
            errdqe = ZERO
            IF ( NCOmpo.GT.0 ) THEN
               DO ic = 1 , NCOmpo
                  sigtot = sigtot + EMUfra(ic)*XSTot(ie,iq,ic)
                  sigela = sigela + EMUfra(ic)*XSEla(ie,iq,ic)
                  sigqep = sigqep + EMUfra(ic)*XSQep(ie,iq,ic)
                  sigqet = sigqet + EMUfra(ic)*XSQet(ie,iq,ic)
                  sigqe2 = sigqe2 + EMUfra(ic)*XSQe2(ie,iq,ic)
                  sigpro = sigpro + EMUfra(ic)*XSPro(ie,iq,ic)
                  sigdel = sigdel + EMUfra(ic)*XSDel(ie,iq,ic)
                  sigdqe = sigdqe + EMUfra(ic)*XSDqe(ie,iq,ic)
                  errtot = errtot + XETot(ie,iq,ic)**2
                  errela = errela + XEEla(ie,iq,ic)**2
                  errqep = errqep + XEQep(ie,iq,ic)**2
                  errqet = errqet + XEQet(ie,iq,ic)**2
                  errqe2 = errqe2 + XEQe2(ie,iq,ic)**2
                  errpro = errpro + XEPro(ie,iq,ic)**2
                  errdel = errdel + XEDel(ie,iq,ic)**2
                  errdqe = errdqe + XEDqe(ie,iq,ic)**2
               END DO
               errtot = SQRT(errtot)
               errela = SQRT(errela)
               errqep = SQRT(errqep)
               errqet = SQRT(errqet)
               errqe2 = SQRT(errqe2)
               errpro = SQRT(errpro)
               errdel = SQRT(errdel)
               errdqe = SQRT(errdqe)
            ELSE
               sigtot = XSTot(ie,iq,1)
               sigela = XSEla(ie,iq,1)
               sigqep = XSQep(ie,iq,1)
               sigqet = XSQet(ie,iq,1)
               sigqe2 = XSQe2(ie,iq,1)
               sigpro = XSPro(ie,iq,1)
               sigdel = XSDel(ie,iq,1)
               sigdqe = XSDqe(ie,iq,1)
               errtot = XETot(ie,iq,1)
               errela = XEEla(ie,iq,1)
               errqep = XEQep(ie,iq,1)
               errqet = XEQet(ie,iq,1)
               errqe2 = XEQe2(ie,iq,1)
               errpro = XEPro(ie,iq,1)
               errdel = XEDel(ie,iq,1)
               errdqe = XEDqe(ie,iq,1)
            END IF
            IF ( MCGene.NE.4 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ECMnn(ie) , Q2G(iq)
99010          FORMAT (/,1X,'E_cm =',F9.1,' GeV  Q^2 =',F6.1,' GeV^2 :',
     &                 /)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99020) sigtot , errtot
99020          FORMAT (1X,'total',32X,F10.4,' +-',F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99030) sigela , errela
99030          FORMAT (1X,'elastic',30X,F10.4,' +-',F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) sigqep , errqep
99040          FORMAT (1X,'quasi-elastic (A+B-->A+X)',12X,F10.4,' +-',
     &                 F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99050) sigqet , errqet
99050          FORMAT (1X,'quasi-elastic (A+B-->X+B)',12X,F10.4,' +-',
     &                 F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99060) sigqe2 , errqe2
99060          FORMAT (1X,'quasi-elastic (A+B-->X, excl. 2-4)',3X,F10.4,
     &                 ' +-',F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99070) sigpro , errpro
99070          FORMAT (1X,'production',27X,F10.4,' +-',F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99080) sigdel , errdel
99080          FORMAT (1X,'diff-el   ',27X,F10.4,' +-',F11.5,' mb')
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99090) sigdqe , errdqe
99090          FORMAT (1X,'diff-qel  ',27X,F10.4,' +-',F11.5,' mb')
            END IF
 
         END DO
      END DO
 
      END SUBROUTINE
