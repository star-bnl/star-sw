
      SUBROUTINE DT_SIGGP(Xi,Q2i,Ecmi,Xnui,Stot,Sine,Sdir)
 
C***********************************************************************
C Total/inelastic photon-nucleon cross sections.                       *
C This version dated 30.04.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION abszx , ALPHEM , am2 , amhi2 , amhi20 , amlo2 , 
     &                 bot , chm , dnv , dsea , DT_SIGVP , dum , ecm , 
     &                 Ecmi , eine , etot , f2 , fac , fac1 , fac2
      DOUBLE PRECISION fsup1 , fsup2 , GEV2MB , gl , ONE , PI , q2 , 
     &                 Q2i , r , scale , Sdir , Sine , Stot , str , 
     &                 sum , TINY10 , top
      DOUBLE PRECISION TWO , TWOPI , upv , usea , w2 , weight , x , 
     &                 xamhi , xamlo , Xi , Xnui , ZERO
      INTEGER i , i1 , i2 , ip , j , NPOINT
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,GEV2MB=0.38938D0,ALPHEM=ONE/137.0D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  energy-interpolation table
      INCLUDE 'inc/potabl'
C*
 
C     PARAMETER (NPOINT=80)
      PARAMETER (NPOINT=16)
      DIMENSION abszx(NPOINT) , weight(NPOINT)
 
      Stot = ZERO
      Sine = ZERO
      Sdir = ZERO
 
      w2 = Ecmi**2
      IF ( (Ecmi.LE.ZERO) .AND. (Xnui.GT.ZERO) ) w2 = AAM(1)**2 - Q2i + 
     &     TWO*Xnui*AAM(1)
      q2 = Q2i
      x = Xi
C photoprod.
      IF ( (x.LE.ZERO) .AND. (q2.LE.ZERO) .AND. (w2.GT.ZERO) ) THEN
         q2 = 0.0001D0
         x = q2/(w2+q2-AAM(1)**2)
C DIS
      ELSE IF ( (x.LE.ZERO) .AND. (q2.GT.ZERO) .AND. (w2.GT.ZERO) ) THEN
         x = q2/(w2+q2-AAM(1)**2)
      ELSE IF ( (x.GT.ZERO) .AND. (q2.LE.ZERO) .AND. (w2.GT.ZERO) ) THEN
         q2 = (w2-AAM(1)**2)*x/(ONE-x)
      ELSE IF ( (x.GT.ZERO) .AND. (q2.GT.ZERO) ) THEN
         w2 = q2*(ONE-x)/x + AAM(1)**2
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'SIGGP: inconsistent input ' , 
     &        w2 , q2 , x
         STOP
      END IF
      ecm = SQRT(w2)
 
      IF ( MODega.EQ.1 ) THEN
         scale = SQRT(q2)
         CALL DT_CKMT(x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl,f2,
     &                IDPdf)
C        W = SQRT(W2)
 
C        ALLMF2 = PHO_ALLM97(Q2,W)
 
C        write(*,*) 'X,Q2,W,F2,ALLMF2',X,Q2,W,F2,ALLMF2
         Stot = TWOPI**2*ALPHEM/(q2*(ONE-x))*f2*GEV2MB
         Sine = ZERO
         Sdir = ZERO
      ELSE IF ( MODega.EQ.2 ) THEN
         IF ( INTrge(1).EQ.1 ) THEN
            amlo2 = (3.0D0*AAM(13))**2
         ELSE IF ( INTrge(1).EQ.2 ) THEN
            amlo2 = AAM(33)**2
         ELSE
            amlo2 = AAM(96)**2
         END IF
         IF ( INTrge(2).EQ.1 ) THEN
            amhi2 = w2/TWO
         ELSE IF ( INTrge(2).EQ.2 ) THEN
            amhi2 = w2/4.0D0
         ELSE
            amhi2 = w2
         END IF
         amhi20 = (ecm-AAM(1))**2
         IF ( amhi2.GE.amhi20 ) amhi2 = amhi20
         xamlo = LOG(amlo2+q2)
         xamhi = LOG(amhi2+q2)
C*PHOJET105a
C        CALL GSET(XAMLO,XAMHI,NPOINT,ABSZX,WEIGHT)
C*PHOJET112
 
         CALL PHO_GAUSET(xamlo,xamhi,NPOINT,abszx,weight)
 
C*
         sum = ZERO
         DO j = 1 , NPOINT
            am2 = EXP(abszx(j)) - q2
            IF ( am2.LT.16.0D0 ) THEN
               r = TWO
            ELSE IF ( (am2.GE.16.0D0) .AND. (am2.LT.121.0D0) ) THEN
               r = 10.0D0/3.0D0
            ELSE
               r = 11.0D0/3.0D0
            END IF
C           FAC = R * AM2/( (AM2+Q2)*(AM2+Q2+RL2) )
            fac = r*am2/((am2+q2)*(am2+q2+RL2))*(ONE+EPSpol*q2/am2)
            sum = sum + weight(j)*fac
         END DO
         Sine = sum
         Sdir = DT_SIGVP(x,q2)
         Stot = ALPHEM/(3.0D0*PI*(ONE-x))*sum*Sdir
         Sdir = Sdir/(0.588D0+RL2+q2)
C        STOT = ALPHEM/(3.0D0*PI*(ONE-X))*SUM*DT_SIGVP(X,Q2)
      ELSE IF ( MODega.EQ.3 ) THEN
         CALL DT_SIGGA(1,Xi,Q2i,Ecmi,ZERO,Stot,etot,Sine,eine,dum)
      ELSE IF ( MODega.EQ.4 ) THEN
C  load cross sections from PHOJET interpolation table
         ip = 1
         IF ( ecm.LE.SIGecm(1,ip,IDXmpar) ) THEN
            i1 = 1
            i2 = 1
         ELSE IF ( ecm.LT.SIGecm(ISImax(IDXmpar),ip,IDXmpar) ) THEN
            DO i = 2 , ISImax(IDXmpar)
               IF ( ecm.LE.SIGecm(i,ip,IDXmpar) ) GOTO 20
            END DO
 20         i1 = i - 1
            i2 = i
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(/1X,A,2E12.3)')
     &            'SIGGP:WARNING:TOO HIGH ENERGY' , ecm , 
     &           SIGecm(ISImax(IDXmpar),ip,IDXmpar)
            i1 = ISImax(IDXmpar)
            i2 = ISImax(IDXmpar)
         END IF
         fac2 = ZERO
         IF ( i1.NE.i2 ) fac2 = LOG(ecm/SIGecm(i1,ip,IDXmpar))
     &        /LOG(SIGecm(i2,ip,IDXmpar)/SIGecm(i1,ip,IDXmpar))
         fac1 = ONE - fac2
C  cross section dependence on photon virtuality
         fsup1 = ZERO
         DO i = 1 , 3
            fsup1 = fsup1 + PARmdl(26+i)*(1.D0+q2/(4.D0*PARmdl(30+i)))
     &              /(1.D0+q2/PARmdl(30+i))**2
         END DO
         fsup1 = fsup1 + PARmdl(30)/(1.D0+q2/PARmdl(34))
         fac1 = fac1*fsup1
         fac2 = fac2*fsup1
         fsup2 = 1.0D0
         Stot = fac2*SIGtab(1,i2,ip,IDXmpar)
     &          + fac1*SIGtab(1,i1,ip,IDXmpar)
         Sine = fac2*SIGtab(28,i2,ip,IDXmpar)
     &          + fac1*SIGtab(28,i1,ip,IDXmpar)
         Sdir = fac2*SIGtab(29,i2,ip,IDXmpar)
     &          + fac1*SIGtab(29,i1,ip,IDXmpar)
C*re:
         Stot = Stot - Sdir
C*
         Sdir = Sdir/(fsup1*fsup2)
C*re:
         Stot = Stot + Sdir
C*
      END IF
 
      END SUBROUTINE
