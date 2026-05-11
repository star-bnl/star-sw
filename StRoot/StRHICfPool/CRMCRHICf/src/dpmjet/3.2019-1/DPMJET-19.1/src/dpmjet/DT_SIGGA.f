
      SUBROUTINE DT_SIGGA(Nti,Xi,Q2i,Ecmi,Xnui,Stot,Etot,Sin,Ein,Stot0)
 
C***********************************************************************
C Total/inelastic photon-nucleus cross sections.                       *
C     !!!! Overwrites SHMAKI-initialization. Do not use it during      *
C          production runs !!!!                                        *
C This version dated 27.03.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION AMPROT , DLARGE , ecm , Ecmi , Ein , Etot , 
     &                 OHALF , ONE , Q2i , Sin , Stot , Stot0 , TINY10 , 
     &                 TINY2 , x , Xi , xnu , Xnui , ZERO
      INTEGER nt , Nti
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0)
      PARAMETER (AMPROT=0.938D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      nt = Nti
      x = Xi
      Q2 = Q2i
      ecm = Ecmi
      xnu = Xnui
      IF ( (Ecmi.LE.ZERO) .AND. (Xnui.GT.ZERO) )
     &     ecm = SQRT(AMPROT**2-Q2+2.0D0*Xnui*AMPROT)
      CALL DT_XSGLAU(1,nt,7,x,Q2,ecm,1,1,-1)
      Stot = XSTot(1,1,1)
      Etot = XETot(1,1,1)
      Sin = XSPro(1,1,1)
      Ein = XEPro(1,1,1)
 
      END SUBROUTINE
