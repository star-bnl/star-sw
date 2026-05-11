
      SUBROUTINE DT_POILIK(Nb,Ntarg,Ecm,Virt,Ipnt,Rpnt,Mode)
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , DT_SANO , dum1 , dum2 , Ecm , ecmold , 
     &                 fac1 , fac2 , fsup1 , fsup2 , ONE , q2old , 
     &                 rate , ratq , Rpnt , rr , san , sdi , sga
      DOUBLE PRECISION sigano , sigdir , sigtot , spl , TINY14 , Virt , 
     &                 ZERO
      INTEGER i , i1 , i2 , ip , Ipnt , j1 , j2 , k1 , k2 , Mode , Nb , 
     &        NE , Ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY14=1.0D0)
      PARAMETER (NE=8)
 
 
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  energy-interpolation table
      INCLUDE 'inc/potabl'
C*
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C*sr 22.7.97
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C*
 
      DATA ecmold , q2old/ - 1.0D0 , -1.0D0/
 
      IF ( (Ecm.NE.ecmold) .OR. (Virt.NE.q2old) ) THEN
 
C load cross sections from interpolation table
         ip = 1
         IF ( Ecm.LE.SIGecm(1,ip,IDXmpar) ) THEN
            i1 = 1
            i2 = 1
         ELSE IF ( Ecm.LT.SIGecm(ISImax(IDXmpar),ip,IDXmpar) ) THEN
            DO i = 2 , ISImax(IDXmpar)
               IF ( Ecm.LE.SIGecm(i,ip,IDXmpar) ) GOTO 20
            END DO
 20         i1 = i - 1
            i2 = i
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(/1X,A,2E12.3)')
     &            'POILIK:WARNING:TOO HIGH ENERGY' , Ecm , 
     &           SIGecm(ISImax(IDXmpar),ip,IDXmpar)
            i1 = ISImax(IDXmpar)
            i2 = ISImax(IDXmpar)
         END IF
         fac2 = ZERO
         IF ( i1.NE.i2 ) fac2 = LOG(Ecm/SIGecm(i1,ip,IDXmpar))
     &        /LOG(SIGecm(i2,ip,IDXmpar)/SIGecm(i1,ip,IDXmpar))
         fac1 = ONE - fac2
 
         sigano = DT_SANO(Ecm)
 
C cross section dependence on photon virtuality
         fsup1 = ZERO
         DO i = 1 , 3
            fsup1 = fsup1 + PARmdl(26+i)*(ONE+Virt/(4.D0*PARmdl(30+i)))
     &              /(ONE+Virt/PARmdl(30+i))**2
         END DO
         fsup1 = fsup1 + PARmdl(30)/(ONE+Virt/PARmdl(34))
         fac1 = fac1*fsup1
         fac2 = fac2*fsup1
         fsup2 = ONE
 
         ecmold = Ecm
         q2old = Virt
      END IF
 
 
C C     SIGTOT = FAC2*SIGTAB(,IP,IDXMPAR 1,I2)+FAC1*SIGTAB(,IP,IDXMPAR 1,I1)
      CALL DT_SIGGP(ZERO,Virt,Ecm,ZERO,sigtot,dum1,dum2)
      IF ( ISHad(1).EQ.1 ) THEN
         sigdir = fac2*SIGtab(29,i2,ip,IDXmpar)
     &            + fac1*SIGtab(29,i1,ip,IDXmpar)
      ELSE
         sigdir = ZERO
      END IF
      sigano = fsup1*fsup2*sigano
      sigtot = sigtot - sigdir - sigano
      sigdir = sigdir/(fsup1*fsup2)
      sigano = sigano/(fsup1*fsup2)
      sigtot = sigtot + sigdir + sigano
 
      rr = DT_RNDM(sigtot)
      IF ( rr.LT.sigdir/sigtot ) THEN
         Ipnt = 1
      ELSE IF ( (rr.GE.sigdir/sigtot) .AND. 
     &          (rr.LT.(sigdir+sigano)/sigtot) ) THEN
         Ipnt = 2
      ELSE
         Ipnt = 0
      END IF
      Rpnt = (sigdir+sigano)/sigtot
C     WRITE(LOUT,'(I3,2F15.5)') ISHAD(1),FAC1,FAC2
C     WRITE(LOUT,'(I3,2F15.5)') MODE,SIGDIR,SIGANO
C     WRITE(LOUT,'(I3,4F15.5)') MODE,SIGDIR+SIGANO,SIGTOT,RPNT,ECM
C     WRITE(LOUT,'(1X,6E12.4)') ECM,VIRT,SIGTOT,SIGDIR,SIGANO,RPNT
 
C*sr 22.7.97
      IF ( Mode.EQ.1 ) RETURN
      k1 = 1
      k2 = 1
      rate = ZERO
      IF ( Ecm.GE.ECMnn(NEBini) ) THEN
         k1 = NEBini
         k2 = NEBini
         rate = ONE
      ELSE IF ( Ecm.GT.ECMnn(1) ) THEN
         DO i = 2 , NEBini
            IF ( Ecm.LT.ECMnn(i) ) THEN
               k1 = i - 1
               k2 = i
               rate = (Ecm-ECMnn(k1))/(ECMnn(k2)-ECMnn(k1))
               GOTO 100
            END IF
         END DO
      END IF
 100  j1 = 1
      j2 = 1
      ratq = ZERO
      IF ( NQBini.GT.1 ) THEN
         IF ( Virt.GE.Q2G(NQBini) ) THEN
            j1 = NQBini
            j2 = NQBini
            ratq = ONE
         ELSE IF ( Virt.GT.Q2G(1) ) THEN
            DO i = 2 , NQBini
               IF ( Virt.LT.Q2G(i) ) THEN
                  j1 = i - 1
                  j2 = i
                  ratq = LOG10(Virt/MAX(Q2G(j1),TINY14))
     &                   /LOG10(Q2G(j2)/MAX(Q2G(j1),TINY14))
                  GOTO 200
               END IF
            END DO
         END IF
      END IF
 200  sga = XSPro(k1,j1,Ntarg)
     &      + rate*(XSPro(k2,j1,Ntarg)-XSPro(k1,j1,Ntarg))
     &      + ratq*(XSPro(k1,j2,Ntarg)-XSPro(k1,j1,Ntarg))
     &      + rate*ratq*(XSPro(k2,j2,Ntarg)-XSPro(k1,j2,Ntarg)
     &      +XSPro(k1,j1,Ntarg)-XSPro(k2,j1,Ntarg))
      sdi = DBLE(Nb)*sigdir
      san = DBLE(Nb)*sigano
      spl = sdi + san
      rr = DT_RNDM(spl)
      IF ( rr.LT.sdi/sga ) THEN
         Ipnt = 1
      ELSE IF ( (rr.GE.sdi/sga) .AND. (rr.LT.spl/sga) ) THEN
         Ipnt = 2
      ELSE
         Ipnt = 0
      END IF
      Rpnt = spl/sga
C     WRITE(LOUT,'(I3,4F15.5)') MODE,SPL,SGA,RPNT,ECM
C*
 
      END SUBROUTINE
