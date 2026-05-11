
      SUBROUTINE DT_SIHNAB(Idp,Idt,Plab,Sigabs)
 
C*********************************************************************
C Pion 2-nucleon absorption cross sections.                          *
C (sigma_tot for pi+ d --> p p, pi- d --> n n                        *
C  taken from Ritchie PRC 28 (1983) 926 )                            *
C This version dated 18.05.96 is written by S. Roesler               *
C*********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , AMDE , AMPI , AMPR , B , C , D , ecm , ekin , 
     &                 ER , ONE , Plab , ptot , Sigabs , TINY3 , TWO , 
     &                 ZERO
      INTEGER Idp , Idt
      SAVE 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY3=1.0D-3)
      PARAMETER (AMPR=938.0D0,AMPI=140.0D0,AMDE=TWO*AMPR,A=-1.2D0,
     &           B=3.5D0,C=7.4D0,D=5600.0D0,ER=2136.0D0)
 
      Sigabs = ZERO
      IF ( ((Idp.NE.13) .AND. (Idp.NE.14) .AND. (Idp.NE.23)) .OR. 
     &     ((Idt.NE.1) .AND. (Idt.NE.8)) ) RETURN
      ptot = Plab*1.0D3
      ekin = SQRT(AMPI**2+ptot**2) - AMPI
      IF ( (ekin.LT.TINY3) .OR. (ekin.GT.400.0D0) ) RETURN
      ecm = SQRT((AMPI+AMDE)**2+TWO*ekin*AMDE)
      Sigabs = A + B/SQRT(ekin) + C*1.0D4/((ecm-ER)**2+D)
C approximate 3N-abs., I=1-abs. etc.
      Sigabs = Sigabs/0.40D0
C pi0-absorption (rough approximation!!)
      IF ( Idp.EQ.23 ) Sigabs = 0.5D0*Sigabs
 
      END SUBROUTINE
